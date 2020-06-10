##############################
# Data Portal Application
##############################

# Required R Packages
library(shiny)      # Shiny: The Web application framework we are working with
library(shinyBS)    # Shiny Bootstrap: Bootstrap web themes and features for shiny
library(DT)         # A wrapper for JavaScript library 'DataTables'
library(haven)      # Read and write STATA, SPSS and SAS files
library(readr)      # Read and write CSV and TSV/TAB files
library(readxl)     # Read excel
library(writexl)    # Write excel
library(ggcorrplot) # Fancy plot of correlation matrix
library(qtlcharts)  # Fancy interactive plot of correlation matrix + scatter chart
library(collapse)   # Fast Data Manipulation and Transformation

# Some global options affecting appearance
options(digits = 3L, width = 100L)
options(htmlwidgets.TOJSON_ARGS = list(na = 'string', digits = 6L)) # better option ?

# Make sure the working directory is set to the data portal project folder
getwd()
# setwd("C:/Users/Sebastian Krantz/...")

# Load All the Datsets from Different Sources, downloaded and processed in get_data.R
load("data/BOU.RData")
load("data/IMF.RData")
load("data/WB.RData")
load("data/ILO.RData")
load("data/OTS_UN_COMT.RData")

# Load Administrative Information about the data
DS_DESC <- read_excel("data/datasets.xlsx")
DS_SRC <- read_excel("data/datasources.xlsx")

# Updating Dataset Description: Adding links and latest data availability
dataset_ov <- function(X) {
  res <- unlist2d(lapply(as.list(X$DSID), function(y) {
    dat <- get(y)
    url <- attr(dat, "url")
    upd <- attr(dat, "updated")
    `oldClass<-`(list(Dataset = if(is.null(url)) NA_character_ else url,
         Updated = if(is.null(upd)) NA_real_ else as.double(upd)), "data.frame")
  }), FALSE)
  attributes(res[[2L]]) <- attributes(Sys.Date())
  linked <- !is.na(res$Dataset)
  res$Dataset[linked] <- paste0('<a href = "', res$Dataset[linked], '"> ', X$Dataset[linked], '</a>')
  res$Dataset[!linked] <- X$Dataset[!linked]
  return(add_vars(res, get_vars(X, c('Frequency', 'Description', 'Dataset Source')),
                  pos = c(2L, 4:5)))
}

DS_DESC_UPDATED <- dataset_ov(DS_DESC)

# Creating an Indicators table, counting number of observations for each indicator (more sophisticated information such as start and end date for each indicator can be added... if data is formatted in a uniform way)
indicator_ov <- function(X, dsn, src) {
  X <- unclass(get(X)) 
  nam <- names(X)
  ntd <- function(x) if(length(x)) x else "-"
  ff <- function(x) c(ntd(attr(x, "label")), ntd(attr(x, "source")))
  lx <- length(X)
  res <- unclass(qDF(setColnames(t(vapply(X, ff, character(2))), 
                                 c("Label", "Source")), "Variable"))
  return(qDF(c(res[1:2], list(Nobs = fNobs.data.frame(X), Dataset = rep(dsn, lx), 
               `Dataset Source` = rep(src, lx)), res[3L])))
}

IND_OV <- unlist2d(Map(indicator_ov, 
                       DS_DESC$DSID, 
                       DS_DESC$Dataset, 
                       DS_DESC$`Dataset Source`), FALSE)




# Small functions to help process data on the server side 
fsetdiff <- function(x, y) x[match(x, y, 0L) == 0L]
fnames <- function(x) attr(x, "names")
selectvars <- function(x, v, exc = FALSE) {
  if(exc) v <- -ckmatch(v, fnames(x))
  get_vars(x, v)
}
vlabdf <- function(X) {
  res <- lapply(unattrib(X), attr, "label")
  res[vapply(res, is.null, TRUE)] <- "-"
  unlist(res, use.names = FALSE)
}
vlabclsrc <- function(X) {
  ntd <- function(x) if(length(x)) x else "-"
  ff <- function(x) c(paste0(paste(class(x), collapse = " "), " (", typeof(x),")"), ntd(attr(x, "label")), ntd(attr(x, "source")))
  qDF(setColnames(t(vapply(X, ff, character(3))), 
                  c("Class (Storage Mode)", "Label", "Source")), "Variable")
}
namlabHTML <- function(df) {
  if(is.list(df) && length(df) > 1L) names(df) <- paste0("<b>", fnames(df), "</b> <br> <small>", vlabdf(df), "</small>")
  df
}
mysummary <- function(df, labels = TRUE) {
  ord <- as.integer(c(3,1,4,2,5,6,7,8,9))
  sumf <- function(x) if(is.numeric(x)) c(fNdistinct.default(x), fmedian.default(x), qsu.default(x, higher = TRUE))[ord] else
                                        c(fNobs.default(x), fNdistinct.default(x), rep(NA_real_,7L))
  res <- t(vapply(df, sumf, numeric(9)))
  colnames(res) <- c("N","Distinct","Mean","Median","SD","Min","Max","Skewness","Kurtosis")
  res <- qDF(print.qsu(res, return = TRUE), "Variable")
  if(labels) res$Label <- vlabels(df)
  return(res)
}
renameSTATA <- function(x) {
  names(x) <- gsub("\\.", "_", fnames(x))
  x
}


# This creates the website, integrating the code in ui.R, which creates the 'Data Access' tab.

# Set title and provide logo for application
title <- "My Data Portal"
logo <- "logo.png" # should be placed in www folder in the project directory, and be png format.

ui <- bootstrapPage('', # Can run print(ui) to see the HTML code for the website after running this.
                 # This compounds the title with the logo on the left    
      navbarPage(div(img(src=logo, height = 40, style = "position: absolute; top: 0; bottom:0; left: 0; right:0; 
                         margin-top: auto; margin-bottom: auto; margin-left: 5px; margin-right: 50px;"), 
                     title, style = "margin-left: 45px;"), 
                 
                 # This is for the browser tab
                 windowTitle = tags$head(tags$link(rel = "icon", type = "image/png", href = logo),
                                         tags$title(title)),
                 
                 # Select default tab when calling site
                 selected = "Data Access",
                 
                 # Read code from ui.R
                 tabPanel("Data Access",
                          source(file.path("ui_code.R"),  local = TRUE)$value
                 ),
                 
                 # 'Data Catalog' tab
                 tabPanel("Data Catalog",
                          div(class="outer",
                              includeCSS("style.css"),
                              p(" "),
                              tabsetPanel(
                                tabPanel("Data Sources",  
                                         p(" "),
                                         DT::dataTableOutput("datasources")),
                                tabPanel("Datasets",  
                                         p(" "),
                                         DT::dataTableOutput("datasetdesc")),
                                tabPanel("Variables", 
                                         p(" "),
                                         div(class = "indicatortable",
                                             DT::dataTableOutput("dataoverview")
                                         ))
                              ) 
                          ) 
                 ),
                 
                 # About tab (can include more sections and useful links), see ?shiny::tags
                 tabPanel("About",
                          h3("Purpose of the Portal"),
                          p('A free Shiny Application to host, filter, aggregate and and download data in various formats. 
                            It can be set up by anyone for any purpose and with any data using the source code and instructions (README.md) available on gitub:'),
                          a("Source Code", href = "https://github.com/SebKrantz/shiny-data-portal"),
                          h3("Details"),
                          p("The app was built using the 'shiny' web-application framework in R (and some small custom HTML and CSS). 
                             It is currently serviced with macroeconomic data for Uganda downloaded and processed from various sources using various R API packages (IMFData, wbstats, Rilostat, tradestatistics) as well as data from the Bank of Uganda. 
                             The app was built by Sebastian Krantz (ODI Fellow in the Ugandan Ministry of Finance, Planning and Economic Development 2020/21).") 
                          # Source code is available on github and released under a GPL 2.0 license (so it may be appropriated and adapted by other organizations under certain conditions). "),
                 )
      )
  )


# This reads the server side code in server.R and creates the server function.
server <- function(input, output, session) {

  source(file.path("server_code.R"), local = TRUE)$value

}

# This puts everything together and runs the application.
shinyApp(ui = ui, server = server)
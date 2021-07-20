##############################
# Data Portal Application
##############################

# Required R Packages
suppressPackageStartupMessages({
library(shiny)      # Shiny: The Web application framework we are working with
library(shinyBS)    # Shiny Bootstrap: Bootstrap web themes and features for shiny
library(DT)         # A wrapper for JavaScript library 'DataTables'
library(haven)      # Read and write STATA, SPSS and SAS files
library(readr)      # Read and write CSV and TSV/TAB files
library(writexl)    # Write excel
library(ggcorrplot) # Fancy plot of correlation matrix
library(qtlcharts)  # Fancy interactive plot of correlation matrix + scatter chart
library(collapse)   # Fast Data Manipulation and Transformation
})

# Some global options affecting appearance
options(digits = 3L, 
        width = 100L,
        htmlwidgets.TOJSON_ARGS = list(na = 'string', digits = 6L), # better option ?
        shiny.sanitize.errors = FALSE) # This makes sure shiny displays any error messages

# Load Data
load("load_all.RData")

# Small functions to help process data on the server side 
selectvars <- function(x, v, exc = FALSE) {
  if(exc) v <- -ckmatch(v, names(x))
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
  if(is.list(df) && length(df) > 1L) names(df) <- paste0("<b>", names(df), "</b> <br> <small>", vlabdf(df), "</small>")
  df
}
mysummary <- function(df, labels = TRUE) {
  ord <- as.integer(c(3,1,4,2,5,6,7,8,9))
  sumf <- function(x) if(is.numeric(x)) c(fndistinct.default(x), fmedian.default(x), qsu.default(x, higher = TRUE))[ord] else
                                        c(fnobs.default(x), fndistinct.default(x), rep(NA_real_, 7L))
  res <- t(vapply(df, sumf, numeric(9)))
  colnames(res) <- c("N","Distinct","Mean","Median","SD","Min","Max","Skewness","Kurtosis")
  res <- qDF(print.qsu(res, return = TRUE), "Variable")
  if(labels) res$Label <- vlabels(df)
  return(res)
}
renameSTATA <- function(x) {
  names(x) <- gsub("\\.", "_", names(x))
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
                          div(class = "AboutContent",
                          h3("Purpose of the Portal"),
                          p('A free Shiny Application to host, filter, aggregate and and download data in various formats. 
                            It can be set up by anyone for any purpose and with any data using the source code and instructions (README.md) available on Github:'),
                          a("Source Code", href = "https://github.com/SebKrantz/shiny-data-portal", target = "_blank"),
                          h3("Details"),
                          p("The app was built using the 'shiny' web-application framework in R (and some small custom HTML and CSS). 
                             It is currently serviced with macroeconomic data for Uganda downloaded and with various sources using various R API packages (IMFData, wbstats, Rilostat, tradestatistics) as well as data from the Bank of Uganda. 
                             The app was built by Sebastian Krantz (ODI Fellow in the Ugandan Ministry of Finance, Planning and Economic Development 2020/21).") 
                          # Source code is available on github and released under a GPL 2.0 license (so it may be appropriated and adapted by other organizations under certain conditions). "),
                          ),
                          tags$footer(
                            HTML("Copyright &copy; 2021, Sebastian Krantz")
                          )
                 )
      )
  )


# This reads the server side code in server.R and creates the server function.
server <- function(input, output, session) {

  source(file.path("server_code.R"), local = TRUE)$value

}

# This puts everything together and runs the application.
shinyApp(ui = ui, server = server)
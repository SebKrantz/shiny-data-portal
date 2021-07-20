# This file creates a single .RData file to load in app.R

library(collapse)   # Fast Data Manipulation and Transformation
library(readxl)     # Read excel

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
  return(qDF(c(res[1:2], list(Nobs = fnobs.data.frame(X), Dataset = rep(dsn, lx), 
                              `Dataset Source` = rep(src, lx)), res[3L])))
}

IND_OV <- unlist2d(Map(indicator_ov, 
                       DS_DESC$DSID, 
                       DS_DESC$Dataset, 
                       DS_DESC$`Dataset Source`), FALSE)

save.image("load_all.RData")

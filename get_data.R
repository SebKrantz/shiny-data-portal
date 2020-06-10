setwd("C:/Users/Sebastian Krantz/Dropbox/MoFPED/Macro Data Portal/SIMPLE DATA PORTAL")

library(readxl)
library(dplyr) 
library(collapse)
library(data.table)

# Functions ---------------------

rm_miss <- function(x) { # Remove rows or columns all missing 
  nonmis <- !is.na(x)                                                         
  x[rowSums(nonmis) > 0, colSums(nonmis) > 0]
}
rm_miss_row <- function(x) {
  wnm <- which(rowSums(!is.na(x)) > 0)
  `attr<-`(x[wnm, ], "cc", wnm)
}
collapselabels <- function(x, pre = NA, post = NA, sep = ": ", na.rm = TRUE) {
  if(na.rm) x <- x[!is.na(x[[1L]]), ]
  rx <- paste0(sep, "NA|^ | $|  |NA", sep)
  x[[2L]] <- gsub(rx,"",do.call(paste, c(x[-1L], list(sep = sep))))
  if(!is.na(pre)) x[[2L]] <- paste0(pre, x[[2L]])
  if(!is.na(post)) x[[2L]] <- paste0(x[[2L]], post)
  x[1:2]
}
rgrep <- function(exp, nam, ...) if(length(exp) > 1L) unlist(lapply(exp, grep, nam, ...), use.names = FALSE) else grep(exp, nam, ...)
dateexpand <- function(x, day = FALSE, origin = "1899-12-30", remove.missing.date = TRUE, sort = TRUE) {
  getFY <- function(y, m) { 
    fy <- ifelse(as.integer(m) >= 7L, y, y-1)
    fy <- paste(fy, substr(as.character(fy+1), 3, 4), sep = "/")
    qF(fy, na.exclude = FALSE)
  }
  getFQ <- function(m) {
    mod <- (m + 6) %% 12
    mod[mod == 0] <- 12
    structure(ceiling(mod/3), levels = paste0("Q",1:4), class = c("ordered","factor"))
  }
  vl <- vlabels(x)
  if(!is.Date(x[[1]])) x[[1]] <- as.Date.numeric(as.numeric(x[[1]]), origin = origin)
  if(remove.missing.date) if(any(na <- is.na(x[[1]]))) x <- x[!na, , drop = FALSE]
  y <- as.numeric(substring(x[[1]], 1, 4))
  m <- as.integer(substring(x[[1]], 6, 7))
  if(day) {
    res <- cbind(Date = x[[1]], 
                 Year = y, 
                 Quarter = structure(ceiling(m/3), levels = paste0("Q",1:4), class = c("ordered","factor")),
                 FY = getFY(y, m),
                 QFY = getFQ(m),
                 Month = structure(m, levels = month.name, class = c("ordered","factor")),
                 Day = as.numeric(substring(x[[1]], 9, 10)), x[, -1])
    if(sort) setorder(res, Year, Month) # res <- res[order(res[[1]]), ]
    vlabels(res) <- c('Date','Year','Quarter','Fiscal Year (July - June)','Quarter of Fiscal Year','Month','Day', vl[-1])
  } else {
    res <- cbind(Date = x[[1]], 
                 Year = y, 
                 Quarter = structure(ceiling(m/3), levels = paste0("Q",1:4), class = c("ordered","factor")),
                 FY = getFY(y, m),
                 QFY = getFQ(m),
                 Month = structure(m, levels = month.name, class = c("ordered","factor")), x[, -1])
    if(sort) setorder(res, Date) # res <- res[order(res[[1]]), ]
    vlabels(res) <- c('Date','Year','Quarter','Fiscal Year (July - June)','Quarter of Fiscal Year','Month', vl[-1])
  }
  return(res)
}
carryfwd <- function(x) {
  v <- !is.na(x)
  wv <- which(v)
  r <- rep(x[v], diff(c(wv, length(x)+1)))
  if(wv[1L] != 1L) return(c(rep(NA, wv[1L]-1L), r))
  r
}
cyfromfy <- function(fy, fq) {
  y <- as.integer(substr(as.character(fy), 1, 4))
  q <- as.integer(substr(as.character(fq), 2, 2))
  newy <- q > 2L
  y[newy] <- y[newy] + 1L
  q[newy] <- q[newy] - 2L
  q[!newy] <- q[!newy] + 2L
  res <- list(Year = y, Quarter = qF(paste0("Q",q)))
  vlabels(res) <- c("Year","Quarter")
  return(qDF(res))
}
fyfromcy <- function(y, q) {
  chary <- as.character(y)
  charyp <- substr(as.character(y+1), 3, 4)
  q <- as.integer(substr(as.character(q), 2, 2))
  newfy <- q > 2L
  fy <- character(length(y))
  fy[newfy] <- paste(chary[newfy], charyp[newfy], sep = "/")
  fy[!newfy] <- paste(y[!newfy]-1,  substr(chary[!newfy], 3, 4), sep = "/")
  q[newfy] <- q[newfy] - 2L
  q[!newfy] <- q[!newfy] + 2L
  res <- list(FY = qF(fy), QFY = qF(paste0("Q",q)))
  vlabels(res) <- c("Fiscal Year (July - June)","Quarter of Fiscal Year")
  return(qDF(res))
}
long2wide <- function(X, id_cols, names_from, values_from, labels_from = NULL, dateexpand = TRUE) {
  res <- tidyr::pivot_wider(X, id_cols, names_from, values_from = values_from)
  nl <- unique(get_vars(X, c(names_from, labels_from)))
  nr <- attr(res, "names")
  lr <- vlabels(res)
  for(i in seq_row(nl)) { # faster way ?? 
    indl <- endsWith(nr, nl[[1]][i])
    lr[indl] <- paste(nl[[2]][i], lr[indl])
  }
  vlabels(res) <- lr
  if(dateexpand) return(dateexpand(res)) else return(res)
}

#########################################################
# Bank of Uganda: This can serve as example on how to process unstructured data from local sources ...
#########################################################

# Selected Monthly Macroeconomic Indicators --------------------------------------
link_BOU_MMI <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/MacroeconomicIndicators/Disseminated-Indicators-file_Web-version.xlsx"
download.file(link_BOU_MMI, destfile = "data/raw/BOU_MMI.xlsx", mode = 'wb')

  # Monthly  
  BOU_MMI <- rm_miss(read_excel("data/raw/BOU_MMI.xlsx", skip = 3))
  BOU_MMI <- rm_miss(get_vars(BOU_MMI, -1))
  BOU_MMI_labs <- read_excel("data/raw/BOU_MMI_labels.xlsx")
  if(nrow(BOU_MMI) != nrow(BOU_MMI_labs)) stop("Names and size of data does not match")
  BOU_MMI <- transpose(cbind(BOU_MMI_labs[1], BOU_MMI), keep.names = "Date", make.names = 1)
  vlabels(BOU_MMI)[-1] <- BOU_MMI_labs[[2]]
  BOU_MMI <- dateexpand(BOU_MMI) 
  attr(BOU_MMI, "url") <- link_BOU_MMI
  attr(BOU_MMI, "updated") <- Sys.Date()
  
  # Annualized
  BOU_MMI_A <- rm_miss(read_excel("data/raw/BOU_MMI.xlsx", sheet = 2, skip = 3))
  BOU_MMI_A <- rm_miss(get_vars(BOU_MMI_A, -1))
  if(nrow(BOU_MMI_A) != nrow(BOU_MMI_labs)) stop("Names and size of data does not match")
  BOU_MMI_A <- transpose(cbind(BOU_MMI_labs[1], BOU_MMI_A), keep.names = "Year", make.names = 1)
  BOU_MMI_A$Year <- as.numeric(BOU_MMI_A$Year)
  vlabels(BOU_MMI_A) <- c("Year",  BOU_MMI_labs[[2]])
  setorder(BOU_MMI_A, Year)
  attr(BOU_MMI_A, "url") <- link_BOU_MMI
  attr(BOU_MMI_A, "updated") <- Sys.Date()
  
  # Annualized FY
  BOU_MMI_AF <- rm_miss(read_excel("data/raw/BOU_MMI.xlsx", sheet = 3, skip = 4))
  BOU_MMI_AF <- rm_miss(get_vars(BOU_MMI_AF, -1))
  if(nrow(BOU_MMI_AF) != nrow(BOU_MMI_labs)) stop("Names and size of data does not match")
  BOU_MMI_AF <- transpose(cbind(BOU_MMI_labs[1], BOU_MMI_AF), keep.names = "FY", make.names = 1)
  BOU_MMI_AF$FY <- ifelse(as.numeric(substr(BOU_MMI_AF$FY, 1, 2)) > 60,  
                          paste0("19",BOU_MMI_AF$FY), 
                          paste0("20",BOU_MMI_AF$FY))
  BOU_MMI_AF$FY <- qF(BOU_MMI_AF$FY)
  setorder(BOU_MMI_AF, FY)
  vlabels(BOU_MMI_AF) <- c("Fiscal Year (July - June)",  BOU_MMI_labs[[2]])
  attr(BOU_MMI_AF, "url") <- link_BOU_MMI
  attr(BOU_MMI_AF, "updated") <- Sys.Date()
  
  rm(BOU_MMI_labs)


# Monetary and Financial Sector Statistics ---------------------------------
  
  # Interest Rates ---------------------------------------------------------
  link_BOU_I <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/InterestRates/Interest_rates.xlsx"
  download.file(link_BOU_I, destfile = "data/raw/BOU_I.xlsx", mode = 'wb')
  
  # Daily interbank rates for Overnight, 7-day and Overall
  BOU_I <- rm_miss(read_xlsx("data/raw/BOU_I.xlsx", sheet = 2, skip = 3))
  get_vars(BOU_I, -1) <- dapply(get_vars(BOU_I, -1), as.numeric)
  vlabels(BOU_I)[-1] <- paste("Daily Interbank Money-Market Rates:", names(BOU_I)[-1])
  names(BOU_I)[-1] <- c("I_Overnight","I_7day","I_Overall")
  BOU_I <- dateexpand(BOU_I, day = TRUE)
  attr(BOU_I, "url") <- link_BOU_I
  attr(BOU_I, "updated") <- Sys.Date()
  
  # Monthly Policy, Commercial Bank and Interbank Rates + Annualized Trasury Yields
  BOU_I_M <- rm_miss(read_xlsx("data/raw/BOU_I.xlsx", sheet = 3, skip = 3))
  BOU_I_M <- rbind(BOU_I_M, setNames(rm_miss(read_xlsx("data/raw/BOU_I.xlsx", sheet = 5, skip = 3)), names(BOU_I_M)))
  BOU_I_M_labs <- collapselabels(read_excel("data/raw/BOU_I_labels.xlsx"))
  series <- c("Central Bank Rate","Rediscount rate","Bank rate to Commercial Banks","35 Days","63 Days","91 Days","182 Days","273 Days","364 Days",
              "Deposit Rates (WARD)","Demand deposits","Savings deposits","Time Deposits","Time Fixed Deposits","Lending Rates","Overnight","7 Day","Overall")
  ind <- rgrep(series, BOU_I_M[[1]])
  if(anyDuplicated(ind)) stop("Multiple series matching")
  View(cbind(BOU_I_M[[1]][ind], BOU_I_M_labs))
  BOU_I_M <- cbind(BOU_I_M_labs[1], dapply(BOU_I_M[ind, -1], as.numeric))
  BOU_I_M <- transpose(BOU_I_M, keep.names = "Date", make.names = 1)
  vlabels(BOU_I_M)[-1] <- BOU_I_M_labs[[2]]
  BOU_I_M <- dateexpand(BOU_I_M, sort = TRUE)
  attr(BOU_I_M, "url") <- link_BOU_I
  attr(BOU_I_M, "updated") <- Sys.Date()
  rm(ind, series, BOU_I_M_labs)
  
  # Treasury Bond Interest Rates
  BOU_I_TB <- rm_miss(read_xlsx("data/raw/BOU_I.xlsx", sheet = 4, skip = 6))
  BOU_I_TB[[2]] <- as.numeric(gsub("YEARS","",BOU_I_TB[[2]]))
  names(BOU_I_TB)[-1] <- c("Maturity","WAP","YTM")
  vlabels(BOU_I_TB)[-1] <- c("Maturity / Duration / Tenor (years)","Weighted Average Price (WAP)","Yield to Maturity (YTM)")
  BOU_I_TB <- dateexpand(BOU_I_TB, day = TRUE)
  vlabels(BOU_I_TB)[1] <- "Date Issued"
  attr(BOU_I_TB, "url") <- link_BOU_I
  attr(BOU_I_TB, "updated") <- Sys.Date()
    
  
  # Exchange  Rates --------------------------------------
  link_BOU_E <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/Exchange_Rates/Exchange-Rates.xlsx"
  download.file(link_BOU_E, destfile = "data/raw/BOU_E.xlsx", mode = 'wb')
  
  # Daily buying, selling and mid exchange rates
  BOU_E <- rm_miss(read_xlsx("data/raw/BOU_E.xlsx", sheet = 2, skip = 5))
  names(BOU_E)[-1] <- c("E_IFEM_B","E_IFEM_S","E_IFEM_MR","E_IFEM_Spread","E_IFEM_DA")
  vlabels(BOU_E)[-1] <- c("Interbank Foreign Exchange Market Buying Rate: Simple Avg. (UgShs/US$)",
                          "Interbank Foreign Exchange Market Selling Rate: Simple Avg. (UgShs/US$)",
                          "Interbank Foreign Exchange Market Mid-Rate (UgShs/US$)",
                          "Interbank Foreign Exchange Market Spread (UgShs)",
                          "Interbank Foreign Exchange Market Dep(+)/App(-) (UgShs)")
  BOU_E <- dateexpand(BOU_E, day = TRUE)
  attr(BOU_E, "url") <- link_BOU_E
  attr(BOU_E, "updated") <- Sys.Date()
  
  # Monthly rates in the forex bureau market and indices
  BOU_E_M <- rm_miss(read_xlsx("data/raw/BOU_E.xlsx", sheet = 3, skip = 7))
  BOU_E_M <- dapply(BOU_E_M, as.numeric)
  names(BOU_E_M) <- c("Date","E_BWA_B","E_BWA_S","E_BWA_MR","E_OFF_MR","NEER","NEER_Index","REER_Index")
  # getting the other sheeet of monthly Interbank Foreign Exchange Market (IFEM) Rates			
  temp <- rm_miss(read_xlsx("data/raw/BOU_E.xlsx", sheet = 4, skip = 5))
  temp <- dapply(temp, as.numeric)
  names(temp) <- c("Date","E_IFEM_B","E_IFEM_S","E_IFEM_Purchases","E_IFEM_Sales","E_IFEM_MR")
  # Joining 
  BOU_E_M <- full_join(BOU_E_M, temp)
  setcolorder(BOU_E_M, c("Date","E_BWA_B","E_IFEM_B","E_BWA_S","E_IFEM_S",
                         "E_BWA_MR","E_OFF_MR","E_IFEM_MR","NEER","NEER_Index","REER_Index",
                         "E_IFEM_Purchases","E_IFEM_Sales"))
  # getting period averages -> E_AV is the same as E_OFF_MR, REER is already there, and EP is not really needed or can be computed from monthly data.
  temp <- rm_miss(read_xlsx("data/raw/BOU_E.xlsx", sheet = 5, skip = 3))
  temp <- dapply(transpose(temp[1:2, -1], keep.names = "Date"), as.numeric)
  names(temp) <- c("Date","E_AV","E_EP")
  # Joining and Aggregating non-matches
  BOU_E_M <- collap(dateexpand(full_join(BOU_E_M, temp)), ~ Year + Month)
  rm(temp)
  vlabels(BOU_E_M)[-(1:6)] <- c("Bureau Weighted Average Buying Rate (UgShs/US$)",
                                "Interbank Foreign Exchange Market Buying Rate: Simple Avg. (UgShs/US$)",
                                "Bureau Weighted Average Selling Rate (UgShs/US$)",
                                "Interbank Foreign Exchange Market Selling Rate: Simple Avg. (UgShs/US$)",
                                "Bureau Weighted Average Mid-Rate (UgShs/US$)",
                                "Official Mid-Rate (UgShs/US$)",
                                "Interbank Foreign Exchange Market Mid-Rate (UgShs/US$)",
                                "Nominal Effective Exchange Rate (NEER)",
                                "NEER Index, 2010 = 100",
                                "Real Effective Exchange Rate (REER) Index, 2010 = 100",
                                "Interbank Foreign Exchange Market Purchases (US$ mn)",
                                "Interbank Foreign Exchange Market Sales (US$ mn)",
                                "Period Average Rate (UgShs/US$)","End Period Rate (UgShs/US$)")
  attr(BOU_E_M, "url") <- link_BOU_E
  attr(BOU_E_M, "updated") <- Sys.Date()
    
  # Credit to the Private Sector --------------------------------
  link_BOU_PSC <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/MonetaryStatistics/Credit-to-the-Private-Sector.xls"
  download.file(link_BOU_PSC, destfile = "data/raw/BOU_PSC.xls", mode = 'wb')
  
  nmn <- function(x, kwd = "Total") { 
    r <- seq_len(grep(kwd, x)[1L])
    r[!is.na(x[r])]
  }
  
  sheets <- list(TOT = list("Total", 2),
                 CB = list("Commerical Bank", 3),
                 CB_UGX = list("Commerical Bank UGX", 6),
                 CB_FX = list("Commerical Bank FOREX", 7),
                 CI = list("Credit Institutions", 4),
                 CI_UGX = list("Credit Institutions UGX", 8),
                 CI_FX = list("Credit Institutions FOREX", 9),
                 MDI = list("Microfinance Deposit Institutions", 5))
  
  BOU_PSC_labs <- collapselabels(rm_miss(read_excel("data/raw/BOU_PSC_labels.xlsx")), 
                                 pre = "Credit to Private Sector: ", post = " (million UGX)")
  
  BOU_PSC <- unlist2d(lapply(sheets, function(i) {
    cat(i[[1]], fill = TRUE)
    temp <- rm_miss(read_excel("data/raw/BOU_PSC.xls", sheet = i[[2]], skip = 3))
    temp <- rm_miss(temp[nmn(temp[[grep("Sector", names(temp))]]), grep("Sector", names(temp)):length(temp)])
    if(nrow(temp) != nrow(BOU_PSC_labs)) stop("dimension mismatch")
    temp[[1]] <- BOU_PSC_labs[[1]]
    temp <- transpose(temp, keep.names = "Date", make.names = 1)
    gv(temp, -1) <- lapply(gv(temp, -1), as.numeric)
    attr(temp, "row.names") <- rep(i[[1]], nrow(temp))
    return(temp)
  }), idcols = "DAID", row.names = "Disaggregation")
  
  setcolorder(BOU_PSC, 3)
  vlabels(BOU_PSC) <- c("Date", "Disaggregation ID", "Disaggregation Name", BOU_PSC_labs[[2]])
  BOU_PSC <- dateexpand(BOU_PSC)
  attr(BOU_PSC, "url") <- link_BOU_PSC
  attr(BOU_PSC, "updated") <- Sys.Date()
  rm(sheets, BOU_PSC_labs)
  
  BOU_PSC_wide <- long2wide(BOU_PSC, id_cols = 1, names_from = 7, values_from = 9:length(BOU_PSC), labels_from = 8)
  BOU_PSC_wide <- collap(BOU_PSC_wide, ~ Year + Month) # Don't know why this is necessary, but there are some duplicated values
  attr(BOU_PSC_wide, "url") <- link_BOU_PSC
  attr(BOU_PSC_wide, "updated") <- Sys.Date()
  
    # Old Way:
    # BOU_PSC <- rm_miss(read_excel("data/raw/BOU_PSC.xls", sheet = 2, skip = 3))
    # BOU_PSC <- rm_miss(BOU_PSC[nmn(BOU_PSC[[1]]), ])
    # BOU_PSC_labs <- collapselabels(rm_miss(read_excel("data/raw/BOU_PSC_labels.xlsx")), 
    #                                pre = "Total Credit to Private Sector: ", post = " (million UGX)")
    # View(cbind(BOU_PSC[1], BOU_PSC_labs))
    # BOU_PSC[[1]] <- BOU_PSC_labs[[1]]
    # BOU_PSC <- transpose(BOU_PSC, keep.names = "Date", make.names = 1)
    # vlabels(BOU_PSC)[-1] <- BOU_PSC_labs[[2]]
    # nc <- ncol(BOU_PSC)
    # 
    # sheets <- list(CB = list("CB", "Commerical Bank", 3),
    #                CBUGX = list("CB_UGX", "Commerical Bank UGX", 6),
    #                CBFX = list("CB_FX", "Commerical Bank FOREX", 7),
    #                CI = list("CI", "Credit Institutions", 4),
    #                CIUGX = list("CI_UGX", "Credit Institutions UGX", 8),
    #                CIFX = list("CI_FX", "Credit Institutions FOREX", 9),
    #                MDI = list("MDI", "Microfinance Deposit Institutions", 5))
    # 
    # for(i in sheets) {
    #   cat(i[[2]], fill = TRUE)
    #   temp <- rm_miss(read_excel("data/raw/BOU_PSC.xls", sheet = i[[3]], skip = 3))
    #   temp <- rm_miss(temp[nmn(temp[[grep("Sector", names(temp))]]), grep("Sector", names(temp)):length(temp)])
    #   temp[[1]] <- gsub("PSC_",paste0("PSC_",i[[1]],"_"), BOU_PSC_labs[[1]])
    #   temp <- transpose(temp, keep.names = "Date", make.names = 1)
    #   vlabels(temp)[-1] <- gsub("Total Credit", paste(i[[2]], "Credit"), BOU_PSC_labs[[2]])
    #   if(ncol(temp) != nc) stop("dimension mismatch")
    #   BOU_PSC <- full_join(BOU_PSC, temp)
    # }
    # setcolorder(BOU_PSC, order(c(1,rep(1:(nc-1), 8))))
    # BOU_PSC <- collap(dateexpand(BOU_PSC), ~ Year + Month) 
    # attr(BOU_PSC, "url") <- link_BOU_PSC
    # rm(i, sheets, temp, nc, BOU_PSC_labs)
    
    
  # Financial Soundness Indicators --------------------------------------
  link_BOU_FS <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/MonetaryStatistics/Financial_Soundness_Indicators.xls"
  download.file(link_BOU_FS, destfile = "data/raw/BOU_FS.xls", mode = 'wb')
  
  # Quarterly
  BOU_FS <- rm_miss(read_excel("data/raw/BOU_FS.xls", sheet = 2, skip = 3))
  BOU_FS <- rm_miss(BOU_FS[,-1])
  BOU_FS_labs <- collapselabels(read_excel("data/raw/BOU_FS_labels.xlsx"))
  if(nrow(BOU_FS) != nrow(BOU_FS_labs)) stop("Names and size of data does not match")
  BOU_FS <- setNames(transpose(BOU_FS, keep.names = "Date"), c("Date", BOU_FS_labs$Variable))
  vlabels(BOU_FS)[-1] <- BOU_FS_labs$Label
  BOU_FS <- dateexpand(BOU_FS) 
  BOU_FS$Month <- NULL # data is quarterly
  attr(BOU_FS, "url") <- link_BOU_FS
  attr(BOU_FS, "updated") <- Sys.Date()
  rm(BOU_FS_labs)
  
  
  # # Annualized
  # BOU_FS_A <- rm_miss(read_excel("data/raw/Financial_Soundness_Indicators.xls", sheet = 3, skip = 3))
  # BOU_FS_A <- rm_miss(BOU_FS_A[,-1])
  # year <- as.numeric(names(BOU_FS_A))
  # if(nrow(BOU_FS_A) != nrow(BOU_FS_labs)) stop("Names and size of data does not match")
  # BOU_FS_A <- cbind(Year = year, setNames(transpose(BOU_FS_A), BOU_FS_labs$Variable))
  # vlabels(BOU_FS_A) <- c("Year",  BOU_FS_labs$Label)
  # attr(BOU_FS_A, "url") <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/MonetaryStatistics/Financial_Soundness_Indicators.xls"
  # rm(year)
  # 
  # # Annualized FY
  # BOU_FS_AF <- rm_miss(read_excel("data/raw/Financial_Soundness_Indicators.xls", sheet = 4, skip = 5))
  # BOU_FS_AF <- rm_miss(BOU_FS_AF[,-1])
  # FY <- qF(names(BOU_FS_AF))
  # if(nrow(BOU_FS_AF) != nrow(BOU_FS_labs)) stop("Names and size of data does not match")
  # BOU_FS_AF <- cbind(FY = FY, setNames(transpose(BOU_FS_AF), BOU_FS_labs$Variable))
  # vlabels(BOU_FS_AF) <- c("Fiscal Year",  BOU_FS_labs$Label)
  # attr(BOU_FS_AF, "url") <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/MonetaryStatistics/Financial_Soundness_Indicators.xls"
  # rm(FY, BOU_FS_labs)
  
  
  
# External Sector Statistics ------------------------------------
  
  # Terms of Trade -> monthly index included in selected macro indicators ! ----------------------------
  link_BOU_TOT <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/External_Sector_Statistics/Trade_Statistics/Terms-of-Trade.xls"
  download.file(link_BOU_TOT, destfile = "data/raw/BOU_TOT.xls", mode = 'wb')
  
  # Monthly
  BOU_TOT <- rm_miss(read_excel("data/raw/BOU_TOT.xls", sheet = 2, skip = 3))
  BOU_TOT <- transpose(rm_miss(BOU_TOT[,-1]), keep.names = "Date", make.names = 1)
  vlabels(BOU_TOT)[-1] <- paste(names(BOU_TOT)[-1], '(1999/2000 = 100)')
  names(BOU_TOT)[2:4] <- c("EXPI","IMPI","TOT")
  BOU_TOT <- dateexpand(BOU_TOT)
  attr(BOU_TOT, "url") <- link_BOU_TOT
  attr(BOU_TOT, "updated") <- Sys.Date()
  
  # Dont need this: less data and earlier rebase index !
  # temp <- rm_miss(read_excel("data/raw/Terms-of-Trade.xls", sheet = 3, skip = 3))
  # temp <- transpose(rm_miss(temp[,-(1:2)]), keep.names = "Date")
  # temp$Date <- as.Date.numeric(as.numeric(temp$Date), origin = "1899-12-30")
  # names(temp)[-1] <- c("EXPI_05","IMPI_05","TOT_05")
  # vlabels(temp)[-1] <- c("Export Price Index (2005 = 100)","Import Price Index (2005 = 100)","Terms of Trade (2005 = 100)")
  # BOU_TOT <- left_join(BOU_TOT, temp)
  # rm(temp)
  
  # Could also do annual and FY !
  
  # Composition of Exports - Values and Volumes ------------------------------------
  link_BOU_EX_C <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/External_Sector_Statistics/Trade_Statistics/Composition-of-Exports_Values-and-Volumes.xlsx"
  download.file(link_BOU_EX_C, destfile = "data/raw/BOU_EX_C.xlsx", mode = 'wb')
  
  # Monthly
  BOU_EX_C <- read_excel("data/raw/BOU_EX_C.xlsx", sheet = 2, skip = 2)
  BOU_EX_C_labs <- read_excel("data/raw/BOU_EX_C_labels.xlsx")
  # rows <- seq_row(BOU_EX_C_labs)
  # View(cbind(BOU_EX_C[rows, 1:2], BOU_EX_C_labs))
  rows <- which(!is.na(BOU_EX_C_labs$Variable))
  BOU_EX_C <- rm_miss(cbind(BOU_EX_C_labs[rows, 1], BOU_EX_C[rows, -(1:2)]))
  BOU_EX_C <- transpose(BOU_EX_C, keep.names = "Date", make.names = 1)
  BOU_EX_C[-1] <- dapply(BOU_EX_C[-1], as.numeric) #rm_miss(d
  vlabels(BOU_EX_C)[-1] <- collapselabels(BOU_EX_C_labs[rows, ])[[2]]
  BOU_EX_C <- dateexpand(BOU_EX_C)
  attr(BOU_EX_C, "url") <- link_BOU_EX_C
  attr(BOU_EX_C, "updated") <- Sys.Date()
  rm(BOU_EX_C_labs)
  
# NOTE: ON AGGREGATION THERE IS A TIME-OFFSET WITH THE QUARTERLY STATISTICS, CHECK ! (But otherwise aggregation works..)
  # -> It's ok, they define quarter for the fiscal qiel, so Q3 in CY is Q1 in FY.. -> Offset of 2 quarters
  # -> Note however that for the first and last year aggregation means leaving one quarter out -> can only aggregate full quarters, otherwise wrong numbers. 
  # View(collap(BOU_EX_C, ~ Year + Quarter, fsum))
  

  # Composition of Imports - Values and Volumes ------------------------------------
  link_BOU_IM_C <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/External_Sector_Statistics/Trade_Statistics/Composition-of-Imports_Values-and-Volume-Indices.xlsx"
  download.file(link_BOU_IM_C, destfile = "data/raw/BOU_IM_C.xlsx", mode = 'wb')
  
  # Monthly Decomposition by product
  BOU_IM_C <- rm_miss(read_excel("data/raw/BOU_IM_C.xlsx", sheet = 2, skip = 3))
  labs <- BOU_IM_C[[1]]
  BOU_IM_C <- rm_miss_row(get_vars(BOU_IM_C, -(1:2)))
  labs <- paste0(labs[attr(BOU_IM_C, "cc")], ": Value Imported (US$ millions)")
  BOU_IM_C <- transpose(BOU_IM_C, keep.names = "Date")
  names(BOU_IM_C) <- c("Date","IM_Animal_Products_VAL","IM_Vegetable_Products_VAL","IM_Prepared_Food_VAL",
                       "IM_Mineral_Products_VAL","IM_Petroleum_Products_VAL","IM_Chemical_Products_VAL",
                       "IM_Plastic_Products_VAL","IM_Wood_Products_VAL","IM_Textile_Products_VAL",
                       "IM_Misc_Manufactured_VAL","IM_Metal_Products_VAL","IM_Machinery_Vehicles_VAL",
                       "IM_Arms_VAL","IM_Electricity_VAL","IM_TOT_VAL")
  vlabels(BOU_IM_C)[-1] <- labs
  BOU_IM_C <- dateexpand(BOU_IM_C)
  attr(BOU_IM_C, "url") <- link_BOU_IM_C
  attr(BOU_IM_C, "updated") <- Sys.Date()
  
  # Monthly Decomposition Cost Insurance, Freight and Govn't Non-Govn't
  BOU_IM_CIF <- rm_miss(read_excel("data/raw/BOU_IM_C.xlsx", sheet = 3, skip = 3))
  BOU_IM_CIF <- BOU_IM_CIF[!is.na(BOU_IM_CIF[[1]]), ]
  labs <- BOU_IM_CIF[[1]]
  BOU_IM_CIF <- rm_miss_row(get_vars(BOU_IM_CIF, -1))
  labs <- paste0(labs[attr(BOU_IM_CIF, "cc")], ": Value Imported (US$ millions)")
  labs[2:26] <- paste0(c(rep("Cost: ",9), rep("Freight: ",8),rep("Insurance: ",8)), labs[2:26])
  labs <- gsub("1/, 2/|  ","",labs)
  BOU_IM_CIF <- transpose(BOU_IM_CIF, keep.names = "Date")
  names(BOU_IM_CIF) <- c("Date","IM_TOT_VAL","IM_C_TOT_VAL","IM_C_GOV_VAL","IM_C_GOV_PROJ_VAL","IM_C_GOV_OTH_VAL",
                         "IM_C_FPS_VAL","IM_C_FPS_OIL_VAL","IM_C_FPS_OTH_VAL","IM_C_PS_OTH_VAL","IM_C_PS_VAL",
                         "IM_F_TOT_VAL","IM_F_GOV_VAL","IM_F_GOV_PROJ_VAL","IM_F_GOV_OTH_VAL",
                         "IM_F_PS_VAL","IM_F_PS_OIL_VAL","IM_F_PS_OTH_VAL","IM_F_PS_FX_VAL",
                         "IM_I_TOT_VAL","IM_I_GOV_VAL","IM_I_GOV_PROJ_VAL","IM_I_GOV_OTH_VAL",
                         "IM_I_PS_VAL","IM_I_PS_OIL_VAL","IM_I_PS_OTH_VAL","IM_I_PS_FX_VAL",
                         "IM_OTH_PI","IM_OTH_VAL_I","IM_OTH_VOL_I","IM_OIL_PI","IM_OIL_VAL_I","IM_OIL_VOL_I",
                         "IM_PI","IM_VAL_I","IM_VOL_I")
  vlabels(BOU_IM_CIF)[-1] <- labs
  BOU_IM_CIF <- dateexpand(BOU_IM_CIF)
  attr(BOU_IM_CIF, "url") <- link_BOU_IM_C
  attr(BOU_IM_CIF, "updated") <- Sys.Date()
  
  # Notes: Perhaps better labels for the subcategories of GOV and PS / FPS (make it explicit!) and do convention for FOREX vs. FX ?? 
    
  # Destination of Exports - Values ------------------------------------
  link_BOU_EX_D <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/External_Sector_Statistics/Trade_Statistics/Direction-of-Trade_Exports.xlsx"
  download.file(link_BOU_EX_D, destfile = "data/raw/BOU_EX_D.xlsx", mode = 'wb')
  
  # Monthly
  BOU_EX_D <- rm_miss(read_excel("data/raw/BOU_EX_D.xlsx", sheet = 2, skip = 2))
  BOU_EX_D_labs <- collapselabels(rm_miss(read_excel("data/raw/BOU_EX_D_labels.xlsx")), 
                                  post = " (US$ million)", sep = " ")
  labs <- BOU_EX_D[[1]]
  BOU_EX_D <- rm_miss_row(get_vars(BOU_EX_D, -1))
  labs <- labs[attr(BOU_EX_D, "cc")]
  View(cbind(labs, BOU_EX_D_labs))
  BOU_EX_D <- transpose(BOU_EX_D, keep.names = "Date")
  names(BOU_EX_D)[-1] <- BOU_EX_D_labs$Variable
  vlabels(BOU_EX_D)[-1] <- BOU_EX_D_labs$Label
  BOU_EX_D <- dateexpand(BOU_EX_D)
  attr(BOU_EX_D, "url") <- link_BOU_EX_D
  attr(BOU_EX_D, "updated") <- Sys.Date()
  rm(BOU_EX_D_labs, labs)
  
  # Origin of Imports - Values ------------------------------------
  link_BOU_IM_O <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/External_Sector_Statistics/Trade_Statistics/Direction-of-Trade_Imports.xlsx"
  download.file(link_BOU_IM_O, destfile = "data/raw/BOU_IM_O.xlsx", mode = 'wb')
  
  # Monthly
  BOU_IM_O <- rm_miss(read_excel("data/raw/BOU_IM_O.xlsx", sheet = 2, skip = 2))
  labs <- BOU_IM_O[[1]]
  BOU_IM_O <- rm_miss_row(dapply(get_vars(BOU_IM_O, -1), as.numeric))
  labs <- labs[attr(BOU_IM_O, "cc")]
  BOU_IM_O_labs <- rm_miss(read_excel("data/raw/BOU_IM_O_labels.xlsx"))
  View(cbind(labs, BOU_IM_O_labs))
  BOU_IM_O_labs$Variable <- paste0("IM_", BOU_IM_O_labs$Variable, "_VAL")
  BOU_IM_O_labs <- collapselabels(BOU_IM_O_labs, pre = "Value Imported from ", post = " (US$ million)", sep = " ")
  BOU_IM_O_labs$Label[length(BOU_IM_O_labs$Label)] <- "Total Formal Imports (US$ million)"
  BOU_IM_O <- transpose(BOU_IM_O, keep.names = "Date")
  names(BOU_IM_O)[-1] <- BOU_IM_O_labs$Variable
  vlabels(BOU_IM_O)[-1] <- BOU_IM_O_labs$Label
  BOU_IM_O <- dateexpand(BOU_IM_O)
  attr(BOU_IM_O, "url") <- link_BOU_IM_O
  attr(BOU_IM_O, "updated") <- Sys.Date()
  rm(BOU_IM_O_labs, labs)  

# Real Sector Statistics ----------------------------------
  
  # CPI (Total and Decomposed) -------------------------------
  link_BOU_CPI <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/RealSector/CPI_Inflation-Rates_BOU-Website.xls"
  download.file(link_BOU_CPI, destfile = "data/raw/BOU_CPI.xls", mode = 'wb')
  
  # Monthly: Do 2009/10 = 100 -> better coverage !
  BOU_CPI <- rm_miss(read_excel("data/raw/BOU_CPI.xls", sheet = 4, skip = 5))[1:4,-1]
  BOU_CPI[[1]] <- paste0("Consumer Price Index (CPI), (2009/10 = 100): ",BOU_CPI[[1]]," (weight = ",round(BOU_CPI[[2]],2),")")
  BOU_CPI[[2]] <- NULL
  BOU_CPI <- transpose(rm_miss(BOU_CPI), keep.names = "Date", make.names = 1)
  vlabels(BOU_CPI) <- names(BOU_CPI)
  names(BOU_CPI)[-1] <- c("CPI_FOOD","CPI_CORE","CPI_EFU","CPI")
  setcolorder(BOU_CPI, c(1,5,3,2,4))
  temp <- rm_miss(read_excel("data/raw/BOU_CPI.xls", sheet = 5, skip = 5))[1:13, -2]
  temp[[1]] <- paste0("Consumer Price Index (CPI), (2009/10 = 100): ",temp[[1]]," (weight = ",round(temp[[2]],2),")")
  temp[[2]] <- NULL
  temp <- transpose(rm_miss(temp), keep.names = "Date", make.names = 1)
  vlabels(temp) <- names(temp)
  names(temp)[-1] <- c("CPI_Food_Bev","CPI_BEV_TOB","CPI_Clothing","CPI_HWEGF","CPI_FHER","CPI_HEA","CPI_TRA","CPI_COM","CPI_REC","CPI_EDU","CPI_REH","CPI_MISC","CPI")
  temp$CPI <- NULL # (duplicated)
  BOU_CPI <- full_join(BOU_CPI, temp)
  rm(temp)
  BOU_CPI <- collap(dateexpand(BOU_CPI), ~ Year + Month) # collap because some dates don't match
  attr(BOU_CPI, "url") <- link_BOU_CPI
  attr(BOU_CPI, "updated") <- Sys.Date()
  

  # Business Tendency Indicator (BTI) -------------------------------
  link_BOU_BTI <- "https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/RealSector/Business-Tendency-Indicators.xlsx"
  download.file(link_BOU_BTI, destfile = "data/raw/BOU_BTI.xlsx", mode = 'wb')
  
  # Monthly: Could also do 2009/10 = 100. Any difference ?? 
  BOU_BTI <- rm_miss(read_excel("data/raw/BOU_BTI.xlsx", skip = 2))
  temp <- BOU_BTI[[1]]
  BOU_BTI <- rm_miss_row(BOU_BTI[-1])
  temp <- temp[attr(BOU_BTI, "cc")]
  sec <-  c("Construction","Manufacturing","Wholesale Trade","Agriculture","Other Services")
  seci <- ckmatch(sec, temp)
  ind <- -c(1:seci[1],seci[-1])
  temp[ind] <- paste(rep(sec, diff(c(seci,length(temp)+1))-1), temp[ind], sep = ": ")
  BOU_BTI <- transpose(BOU_BTI, keep.names = "Date")
  vlabels(BOU_BTI)[-1] <- paste0("Business Tendency Indicator (BTI): ", temp)
  names(BOU_BTI)[-1] <- c("BTI","BTI_PBS","BTI_BS3","BTI_OVS","BTI_EMP","BTI_CO","BTI_ASP","BTI_FS","BTI_AC",
                          "BTI_CON","BTI_CON_PBS","BTI_CON_BS3","BTI_CON_OVS","BTI_CON_EMP",
                          "BTI_MAN","BTI_MAN_PBS","BTI_MAN_BS3","BTI_MAN_OVS","BTI_MAN_EMP",
                          "BTI_WOT","BTI_WOT_PBS","BTI_WOT_BS3","BTI_WOT_EMP",
                          "BTI_AGR","BTI_AGR_PBS","BTI_AGR_BS3","BTI_AGR_EMP",
                          "BTI_OSE","BTI_OSE_PBS","BTI_OSE_BS3","BTI_OSE_EMP")
  BOU_BTI <- dateexpand(BOU_BTI)
  attr(BOU_BTI, "url") <- link_BOU_BTI
  attr(BOU_BTI, "updated") <- Sys.Date()
  
  # Saving updated data ---------------------------------------------
  save(list = ls()[grep("^BOU_", ls())], file = "data/BOU.RData")
  
    

  
  
  
##############################################
# IMF
##############################################
  
  library(IMFData)
  # API Acces using the newer package:
  # library(imfr)
  # primcom <- imf_codelist("PCPS")
  # primcomc <- imf_codes("CL_INDICATOR_PCPS")
  # IMF_PCPS <- imf_data("PCPS", primcomc$codes,
  #                    country = "W00", start = 1991,
  #                    end = current_year(),
  #                    freq = "M",
  #                    print_url = TRUE)
  # datasets2 <- DataflowMethod() # ERROR !
  
  # Main data page: https://data.imf.org/
  # datasets <- imfr::imf_ids()
  
  transIMFAPI <- function(x, namlab, time = c("A","Q","M"), ids = c("TIME_PERIOD","INDICATOR"), indicator = "INDICATOR", vlab3did = NULL) {
    names(x) <- gsub("@", "", names(x))
    if(!is.data.frame(x)) x <- qDF(x)
    if(any(lc <- vapply(x, is.list, TRUE))) x[lc] <- NULL
    if(any(misst <- is.na(x$TIME_PERIOD))) x <- x[!misst, , drop = FALSE]
    x$OBS_VALUE <- as.numeric(x$OBS_VALUE)
    x <- tidyr::pivot_wider(x, id_cols = ids, values_from = "OBS_VALUE", names_from = indicator)
    if(!all(ccol <- fNobs(x) > 0 & fNdistinct(x) > 1)) x <- get_vars(x, ccol)
    nid <- length(ids)-1L
    if(!all(cc <- rowSums(!is.na(get_vars(x, -seq_len(nid)))) > 0)) x <- x[cc, , drop = FALSE]
    switch(time[1L],
           M = {
             x[[1L]] <- as.Date(paste0(x[[1L]],"-01"))
             names(x)[1L] <- "Date"
             setorderv(x, cols = "Date")
             matches <- ckmatch(names(x)[-seq_len(nid)], namlab[[1L]])
             vlabels(x) <- c("Date", vlab3did, namlab[[2L]][matches])
             setcolorder(x, order(c(rep(0, length(vlab3did)+1L), matches)))
             return(dateexpand(x))
           },
           Q = {
             x[["Quarter"]] <- qF(substr(x[[1L]], 6, 7))
             chary <- substr(x[[1L]], 1, 4)
             x[["Date"]] <- as.Date(paste0(chary, "-", as.integer(substr(x[[1L]], 7, 7))*3, "-1"))
             x[[1L]] <- as.numeric(chary)
             names(x)[1L] <- "Year"
             setorderv(x, cols = c("Year","Quarter"))
             lx <- length(x)
             setcolorder(x, c(lx, 1, lx-1))        
             add_vars(x, 4:5) <- fyfromcy(x$Year, x$Quarter)
             matches <- ckmatch(names(x)[-seq_len(nid+4L)], namlab[[1L]])
             vlabels(x) <- c("Date","Year","Quarter","Fiscal Year (July - June)","Quarter of Fiscal Year", vlab3did, namlab[[2L]][matches])
             setcolorder(x, order(c(rep(0, length(vlab3did)+5L), matches)))
             return(x)
           },
           A = {
             x[[1L]] <- as.numeric(x[[1L]])
             names(x)[1L] <- "Year"
             setorderv(x, cols = "Year")
             matches <- ckmatch(names(x)[-seq_len(nid)], namlab[[1L]])
             vlabels(x) <- c("Year", vlab3did, namlab[[2L]][matches])
             setcolorder(x, order(c(rep(0, length(vlab3did)+1L), matches)))
             return(x)
           }
    )
  }
  
  # Set country
  ctry <- "UG"
  
  # Set the end date to something beyond the current period:  
  end_date <- "2030-01-01"
  
  # Load Previous Data:
  load("data/IMF.Rdata")
  
# CROSS-CUTTING --------------------  
    
  # Sub-Saharan Africa Regional Economic Outlook (AFRREO):  -> Only Annual Updates ! ----------------------------
  afrreo.str <- DataStructureMethod('AFRREO')
  str(afrreo.str) # Only annual data !
  IMF_AFRREO <- CompactDataMethod('AFRREO', queryfilter = list(CL_FREQ = "A"),
                                  startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_AFRREO <- transIMFAPI(IMF_AFRREO, afrreo.str$CL_INDICATOR_AFRREO, ids = c("TIME_PERIOD","REF_AREA","INDICATOR"), 
                                       vlab3did = "ISO 2-digit country code")
  names(IMF_AFRREO)[2] <- "ISO2"
  IMF_AFRREO <- dplyr::left_join(IMF_AFRREO, setNames(afrreo.str$CL_AREA_AFRREO, c("ISO2","Country")))
  setcolorder(IMF_AFRREO, c(c(2,3,1), ncol(IMF_AFRREO)))
  setorder(IMF_AFRREO, ISO2, Year)
  vlabels(IMF_AFRREO[[3]]) <- "Name of country or entity"
  
  
  # International Financial Statistics: 
  ifs.str <- DataStructureMethod('IFS')
  str(ifs.str)
  IMF_IFS <- CompactDataMethod('IFS', queryfilter = list(CL_FREQ = "M", CL_AREA_IFS = ctry),
                               startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_IFS <- transIMFAPI(IMF_IFS, ifs.str$CL_INDICATOR_IFS, time = "M")
  IMF_IFSQ <- CompactDataMethod('IFS', queryfilter = list(CL_FREQ = "Q", CL_AREA_IFS = ctry),
                               startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_IFSQ <- transIMFAPI(IMF_IFSQ, ifs.str$CL_INDICATOR_IFS, time = "Q")
  # Quarterly we have additional BOP statistics available
  View(namlab(get_vars(IMF_IFSQ, setdiff(names(IMF_IFSQ), names(IMF_IFS)))))

  # Prices, Production, Labour and Trade --------------------------------------
    # Works, but weird format -> Need additional processing ...
    # pplt.str <- DataStructureMethod('PPLT')
    # str(pplt.str)
    # IMF_PPLT <- CompactDataMethod('PPLT', queryfilter = list(CL_FREQ = "M", CL_AREA_PPLT = ctry),
    #                              startdate = "1960-01-01", enddate = "2020-01-01", tidy = TRUE)
    # IMF_PPLT <- transIMFAPI(IMF_PPLT, pplt.str$CL_INDICATOR_PPLT, time = "M")
    # 
    # # 
    # # View(pplt.str$CL_INDICATOR_PPLT)

  
  
# EXTERNAL SECTOR STATISTICS ---------------------------------------------------------
   
  # Balance of Payment and International Investment Position: -> Quarterly Updates
  bop.str <- DataStructureMethod('BOP')
  str(bop.str) 
  IMF_BOP <- CompactDataMethod('BOP', queryfilter = list(CL_FREQ = "Q", CL_AREA_BOP = ctry),
                               startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_BOP <- transIMFAPI(IMF_BOP, bop.str$CL_INDICATOR_BOP, time = "Q")
  
  # Balance of Payments (BOP), World and Regional Aggregates
  bopagg.str <- DataStructureMethod('BOPAGG') # Only Annual data available
  IMF_BOPAGG <- CompactDataMethod('BOPAGG', queryfilter = list(CL_FREQ = "A", CL_AREA_BOPAGG = ctry),
                                   startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_BOPAGG <- transIMFAPI(IMF_BOPAGG, bopagg.str$CL_INDICATOR_BOPAGG)

  # Export Quality ------------------------------------------------------
     # Note: Only up to 2014 !, This does not need to be updated !
  eq.str <- DataStructureMethod('EQ')
  str(eq.str)
  names(eq.str)
  IMF_EQ <- CompactDataMethod('EQ', queryfilter = list(CL_FREQ = "A", CL_AREA_EQ = ctry, CL_PRODUCT_EQ = "", CL_INDICATOR_EQ = c("qual","value","uv")),
                               startdate = "1960-01-01", enddate = "2020-01-01", tidy = TRUE)
  IMF_EQ <- transIMFAPI(IMF_EQ, eq.str$CL_INDICATOR_EQ, ids = c("TIME_PERIOD","PRODUCT","INDICATOR"), 
                        vlab3did = "Product Code: 4-digit SITC (Rev. 1)")
  names(IMF_EQ)[2L] <- "SITC1"
  IMF_EQ <- left_join(IMF_EQ, setNames(eq.str$CL_PRODUCT_EQ, c("SITC1","Product")))
  setcolorder(IMF_EQ, c(1:2, ncol(IMF_EQ)))
  setorder(IMF_EQ, SITC1, Year)
  vlabels(IMF_EQ[[3]]) <- "Product name"

  # Export Diversification ------------------------------------------------------
     # Note: Only up to 2014 ! This does not need to be updated !
  ed.str <- DataStructureMethod('ED')
  str(ed.str)
  IMF_ED <- CompactDataMethod('ED', queryfilter = list(CL_FREQ = "A", CL_AREA_ED = ctry),
                              startdate = "1960-01-01", enddate = "2020-01-01", tidy = TRUE)
  IMF_ED <- transIMFAPI(IMF_ED, ed.str$CL_INDICATOR_ED)

  # Currency Composition of Official Foreign Exchange Reserves ------------------------
    # cofer.str <- DataStructureMethod('COFER')
    # Only for the Wholw World or Advanced and Emerging economies. Not country specific !
  
  # International Reserves and Foreign Currency Liquidity (IRFCL) ----------------------
    # irfcl.str <- DataStructureMethod('IRFCL')
    # # No data Available !
    # IMF_IRFCL <- CompactDataMethod('IRFCL', queryfilter = list(CL_FREQ = "B", CL_AREA_IRFCL = ctry, CL_INDICATOR_IRFCL = "", CL_SECTOR_IRFCL = ""),
    #                             startdate = "1960-01-01", enddate = "2020-01-01", tidy = TRUE)

  
  # Coordinated Direct Investment Survey (CDIS) -----------------------
    # cdis.str <- DataStructureMethod('CDIS')
    # str(cdis.str)
    # names(cdis.str) # Only Annual Data 
    # IMF_CDIS <- CompactDataMethod('CDIS', queryfilter = list(CL_FREQ = "A", CL_AREA_CDIS = ctry, CL_INDICATOR_CDIS = ""),
    #                               startdate = "1960-01-01", enddate = "2020-01-01", tidy = TRUE)
    # # Entity requested too large !

  # Coordinated Portfolio Investment Survey (CPIS) -----------------------
    # Annual Download works, but needs more procesing !
    # cpis.str <- DataStructureMethod('CPIS')
    # str(cpis.str)
    # names(cpis.str)  
    # IMF_CPIS <- CompactDataMethod('CPIS', queryfilter = list(CL_FREQ = "M", CL_AREA_CPIS = ctry),
    #                               startdate = "1960-01-01", enddate = "2020-01-01", tidy = TRUE)
    # names(IMF_CPIS) <- gsub("@","",names(IMF_CPIS))
    # IMF_CPIS$OBS_VALUE <- as.numeric(IMF_CPIS$OBS_VALUE)
    # IMF_CPIS <- dplyr::filter(IMF_CPIS, !is.na(TIME_PERIOD))
    # IMF_CPIS <- tidyr::pivot_wider(IMF_CPIS, id_cols = c("TIME_PERIOD","INDICATOR"), 
    #                                values_from = "OBS_VALUE", names_from = "INDICATOR")
    # IMF_CPIS[[1]] <- as.numeric(IMF_CPIS[[1]])
    # names(IMF_CPIS)[1] <- "Year"
    # vlabels(IMF_CPIS) <- c("Year", cpis.str$CL_INDICATOR_CPIS[[2]][anyNAerror(match(names(IMF_CPIS)[-1], cpis.str$CL_INDICATOR_CPIS[[1]]), "Unknown columns")])
  
  
  
# REAL SECTOR STATISTICS: --------------------------------------------------------
  
  # Primary Commodity Prices (Global): -> Monthly Updates  ------------------------------------------------------
  pcps.str <- DataStructureMethod('PCPS') 
  str(pcps.str)
  IMF_PCPS <- CompactDataMethod('PCPS', queryfilter = list(CL_FREQ = "M", CL_AREA_PCPS = "", CL_INDICATOR_PCPS = "", CL_UNIT_PCPS = c("USD","IX")),
                              startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
    namlab <- pcps.str$CL_INDICATOR_PCPS
    namlab2 <- namlab
    namlab[[1]] <- paste0(namlab[[1]], "_IX")
    namlab[[2]] <- paste0(namlab[[2]], " (Index, 2016 = 100)") # still right ?? 
    namlab2[[1]] <- paste0(namlab2[[1]], "_USD")
    namlab2[[2]] <- paste0(namlab2[[2]], " (US$)")
    namlab <- rbind(namlab, namlab2)[order(rep(seq_row(namlab), 2)), ]
  IMF_PCPS <- transIMFAPI(IMF_PCPS, namlab, ids = c("TIME_PERIOD","COMMODITY"), 
                          indicator = c("COMMODITY","UNIT_MEASURE"), time = "M")
  rm(namlab, namlab2)
  
  # Commodity Terms of Trade (Country Specific):  -> Monthly Updates ! ------------------------------------------------------
    # https://data.imf.org/?sk=2CDDCCB8-0B59-43E9-B6A0-59210D5605D2
  pctot.str <- DataStructureMethod('PCTOT') 
  str(pctot.str)
  IMF_PCTOT <- CompactDataMethod('PCTOT', queryfilter = list(CL_FREQ = "M", CL_Country_PCTOT = ctry, CL_Indicator_PCTOT = "", CL_Type_PCTOT = "R_RW_IX"),
                                 startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_PCTOT <- transIMFAPI(IMF_PCTOT, pctot.str$CL_Indicator_PCTOT, time = "M")
  names(IMF_PCTOT) <- Recode(names(IMF_PCTOT), "m" = "PCTOT_IM", "x" = "PCTOT_EX", "xm" = "PCTOT_NEX",
                             "m_gdp" = "PCTOT_IM_GDP", "x_gdp" = "PCTOT_EX_GDP", "xm_gdp" = "PCTOT_NEX_GDP")

  # Direction of Trade Statistics:  -> Monthly Updates ! ------------------------------------------------------
  dot.str <- DataStructureMethod('DOT')
  str(dot.str)
  IMF_DOT <- CompactDataMethod('DOT', queryfilter = list(CL_FREQ = "M", CL_AREA_DOT = ctry),
                               startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_DOT <- transIMFAPI(IMF_DOT, dot.str$CL_INDICATOR_DOT, time = "M", ids = c("TIME_PERIOD","COUNTERPART_AREA","INDICATOR"), 
                         vlab3did = "ISO 2-digit code of countries and aggregate entities")
  names(IMF_DOT) <- gsub("COUNTERPART_AREA","Partner_ISO2", names(IMF_DOT))
  vl <- vlabels(IMF_DOT)
  IMF_DOT <- left_join(IMF_DOT, setNames(dot.str[[4]], c("Partner_ISO2","Partner")))
  vlabels(IMF_DOT) <- c(vl, "Countries and aggregate entities")
  setcolorder(IMF_DOT, c(1:7, ncol(IMF_DOT)))
  setorder(IMF_DOT, Partner_ISO2, Date)
  
  # Consumer Price Index (CPI):  -> Monthly Updates ! ----------------------------------------------------
  cpi.str <- DataStructureMethod('CPI')
  str(cpi.str) 
  IMF_CPI <- CompactDataMethod('CPI', queryfilter = list(CL_FREQ = "M", CL_AREA_CPI  = ctry), 
                               startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_CPI <- transIMFAPI(IMF_CPI, cpi.str$CL_INDICATOR_CPI, time = "M")
  IMF_CPI <- get_vars(IMF_CPI, !grepl("_PP_PT$|_A_PT$|_WT$", names(IMF_CPI)))
  # TODO: Need weights ??? 
    
  # System of National Accounts (SNA) ------------------------------
    # sna.str <- DataStructureMethod('SNA')
    # str(sna.str)
    # names(sna.str) # Only Quarterly Data 
    # IMF_SNA <- CompactDataMethod('SNA', queryfilter = list(CL_FREQ = "Q", CL_AREA_SNA  = ctry), 
    #                              startdate = "1960-01-01", enddate = "2020-01-01", tidy = TRUE)
    # # Not really worth it -> QGDP only for a few years ...
  
# FISCAL SECTOR STATISTICS: --------------------------------------------------------
  
  # Government Finance Statistics:  -> Annual data, DO not Update.. --------------------------------------------------
  #gfs.str <- DataStructureMethod('GFSR')
  # Disaggregated into 7 databses
    getnlGFS <- function(x, str) {
      namlab <- unique(get_vars(x, c("@UNIT_MEASURE","@CLASSIFICATION","@INDICATOR_CODE")))
      namlab <- left_join(namlab, str[[5]], by = c("@CLASSIFICATION" = "CodeValue"))
      namlab <- left_join(namlab, str[[4]], by = c("@UNIT_MEASURE" = "CodeValue"))
      # namlab <- namlab[match(namlab[[2]], gfs_mab.str$CL_INDICATOR_GFSMAB$CodeValue), ][[2]]
      return(collapselabels(namlab[-(1:2)]))
    }
    # (1) Main Aggregaes and Balances
    gfs_mab.str <- DataStructureMethod('GFSMAB')
    str(gfs_mab.str) # Only Annual Data Available, and I only download Budgetary central government data 
    IMF_GFSMAB <- CompactDataMethod('GFSMAB', queryfilter = list(CL_FREQ = "A", CL_AREA_GFSMAB = ctry, CL_SECTOR_GFSMAB = "S1311B"),
                                   startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
    namlab <- getnlGFS(IMF_GFSMAB, gfs_mab.str)
    IMF_GFSMAB <- transIMFAPI(IMF_GFSMAB, namlab, ids = c("TIME_PERIOD","INDICATOR_CODE"), indicator = "INDICATOR_CODE")
    # TODO: Better order ?? 
    
    # (2) Revenue 
    gfsr.str <- DataStructureMethod('GFSR')
    str(gfsr.str) # Only Annual Data Available, and I only download Budgetary central government data
    IMF_GFSR <- CompactDataMethod('GFSR', queryfilter = list(CL_FREQ = "A", CL_AREA_GFSR = ctry, CL_SECTOR_GFSR = "S1311B"),
                                    startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
    namlab <- getnlGFS(IMF_GFSR, gfsr.str)
    IMF_GFSR <- transIMFAPI(IMF_GFSR, namlab, ids = c("TIME_PERIOD","INDICATOR_CODE"), indicator = "INDICATOR_CODE")
    # TODO: Better order ??
    
    # (3) Expenditure 
    gfse.str <- DataStructureMethod('GFSE')
    str(gfse.str) # Only Annual Data Available, and I only download Budgetary central government data
    IMF_GFSE <- CompactDataMethod('GFSE', queryfilter = list(CL_FREQ = "A", CL_AREA_GFSE = ctry, CL_SECTOR_GFSE = "S1311B"),
                                    startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
    namlab <- getnlGFS(IMF_GFSE, gfse.str)
    IMF_GFSE <- transIMFAPI(IMF_GFSE, namlab, ids = c("TIME_PERIOD","INDICATOR_CODE"), indicator = "INDICATOR_CODE")
    # TODO: Better order ??
    
    # ... Could load more ...
    
  # Fiscal Monitor:  -> Annual Data (Do not update) --------------------------------------------------
  fm.str <- DataStructureMethod('FM')
  str(fm.str) # Only Annual Data Available !
  IMF_FM <- CompactDataMethod('FM', queryfilter = list(CL_FREQ = "A", CL_AREA_FM = ctry, CL_INDICATOR_FM = ""),
                               startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_FM <- transIMFAPI(IMF_FM, fm.str$CL_INDICATOR_FM)
  
  # Investment and Capital Stock (ICSD) --------------------------------------------------
    # No data for Uganda !
    # icsd.str <- DataStructureMethod('ICSD')
    # str(icsd.str) 
    # IMF_ICSD <- CompactDataMethod('ICSD', queryfilter = list(CL_FREQ = "A", CL_AREA_ICSD = ctry),
    #                             startdate = "1960-01-01", enddate = "2020-01-01", tidy = TRUE)
  
  # Private and Public Capital Stock Dataset (PGCS):  -> Annual Updates, Do not Update ! ----------------------
  pgcs.str <- DataStructureMethod('PGCS')
  str(pgcs.str) # Only Annual Data !
  IMF_PGCS <- CompactDataMethod('PGCS', queryfilter = list(CL_FREQ = "A", CL_Country_PGCS = "746"),
                              startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_PGCS <- transIMFAPI(IMF_PGCS, pgcs.str$CL_Indicator_PGCS)
  names(IMF_PGCS)[-1] <- toupper(names(IMF_PGCS)[-1])
  
  # Public Sector Balance Sheet (PSBSFAD) ----------------------
    # Only Gives 1 series: GDP in National Currency !
    # psbs.str <- DataStructureMethod('PSBSFAD')
    # str(psbs.str) # Only Annual Data !
    # IMF_PSBS <- CompactDataMethod('PSBSFAD', queryfilter = list(CL_FREQ = "A", `CL_PSBS Country_PSBSFAD` = "746"),
    #                               startdate = "1960-01-01", enddate = "2020-01-01", tidy = TRUE)
    # IMF_PSBS <- transIMFAPI(IMF_PSBS, psbs.str[[3L]])
    # names(IMF_PGCS)[-1] <- toupper(names(IMF_PGCS)[-1])
  
  # Revenue Administration Fiscal Information Tool (RA-FIT) ----------------------------------
    # No data for Uganda !
    # rafit.str <- DataStructureMethod('RAFIT2AGG')
    # str(rafit.str) 
    # IMF_RAFIT <- CompactDataMethod('RAFIT2AGG', queryfilter = list(CL_AREA_RAFIT2AGG = ctry))

  # World Commodity Exporters (WCED) ----------------------------------
    # Uganda is not part of this dataset ! (not a major commodity exporter)
    # wced.str <- DataStructureMethod('WCED')

  # World Revenue Longitudinal Data (WoRLD):  -> Annual Data - Do not update! --------------------------------
  world.str <- DataStructureMethod('WoRLD')
  str(world.str) # Only Annual Data !
  IMF_WoRLD <- CompactDataMethod('WoRLD', queryfilter = list(CL_FREQ = "A", CL_AREA_WoRLD = ctry),
                                 startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_WoRLD <- transIMFAPI(IMF_WoRLD, world.str$CL_INDICATOR_WoRLD)
    
# FINANCIAL SECTOR STATISTICS: --------------------------------------------------------

  # Financial Access Survey:  -> Annual Data - Do not update! --------------------------------------------
  fas.str <- DataStructureMethod('FAS')
  str(fas.str) # Only Annual Data Available
  IMF_FAS <- CompactDataMethod('FAS', queryfilter = list(CL_FREQ = "A", CL_AREA_FAS  = ctry),
                               startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_FAS <- transIMFAPI(IMF_FAS, fas.str$CL_INDICATOR_FAS)

  # Financial Development Index:  -> Annual Data - Do not update! --------------------------------------------
  fdi.str <- DataStructureMethod('FDI')
  str(fdi.str) # Only Annual data
  IMF_FDI <- CompactDataMethod('FDI', queryfilter = list(CL_FREQ = "A", CL_AREA_FDI  = ctry),
                               startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_FDI <- transIMFAPI(IMF_FDI, fdi.str$CL_INDICATOR_FDI)
  
  # Financial Soundness Indicators:  -> Annual Data - Do not update! --------------------------------------------
     # Note: Only up to 2015 !
  fsi.str <- DataStructureMethod('FSI')
  str(fsi.str) 
  IMF_FSI <- CompactDataMethod('FSI', queryfilter = list(CL_FREQ = "M", CL_AREA_FSI  = ctry),
                               startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_FSI <- transIMFAPI(IMF_FSI, fsi.str$CL_INDICATOR_FSI, time = "M")
  
  # Monetary and Financial Statistics (MFS):  -> Monthly Updates ! --------------------------------------------
  mfs.str <- DataStructureMethod('MFS')
  str(mfs.str)
  IMF_MFS <- CompactDataMethod('MFS', queryfilter = list(CL_FREQ = "M", `CL_International Financial Statistics (IFS) Country_MFS` = ctry),
                               startdate = "1960-01-01", enddate = end_date, tidy = TRUE)
  IMF_MFS <- transIMFAPI(IMF_MFS, mfs.str[[3]], time = "M")
  
  
  
  save(list = ls()[grep("^IMF_", ls())], file = "data/IMF.RData")
  
###############################################
# TRADE STATISTICS
###############################################
  
  # TH <- haven::read_dta("C:/Users/Sebastian Krantz/Documents/Geneva Graduate Institute/EI064 - International Trade A/Project/Financial Shocks/Trade Data/Tradehist V4/TRADHIST_v4.dta")
  # THUG <- qDT(TH)
  library(tradestatistics)
  
  # Bilateral Trade -----------------------------------------
  OTS_UN_COMT_BIL <- ots_create_tidy_data(years = 1965:2018, 
                                    reporters = "Uganda", 
                                    table = "yrp")
  
  # temp <- ots_create_tidy_data(years = 2019, 
  #                              reporters = "Uganda", 
  #                              table = "yrp")
  
  INFA <- ots_inflation_adjustment(OTS_UN_COMT_BIL, reference_year = 2010)
  identical(INFA[1:5], OTS_UN_COMT_BIL[1:5])
  add_vars(OTS_UN_COMT_BIL) <- add_stub(get_vars(INFA, 6:7), "_CT", FALSE)
  rm(INFA)
  get_vars(OTS_UN_COMT_BIL, c("reporter_iso","reporter_fullname_english")) <- NULL
  names(OTS_UN_COMT_BIL) <- c("Year","ISO","Country","EX_VAL","IM_VAL","EX_VAL_CT","IM_VAL_CT")
  OTS_UN_COMT_BIL$ISO <- toupper(OTS_UN_COMT_BIL$ISO)
  vlabels(OTS_UN_COMT_BIL) <- c("Year","ISO 3-digit country code","Country name","Export Value in current US$","Import Value in current US$","Export Value in constant 2010 US$","Import Value in constant 2010 US$")

  # Product level trade -------------------------------------------
  View(ots_products)
  OTS_UN_COMT <- data.frame()
  for(i in 1965:2018) {
    temp <- ots_create_tidy_data(years = i, 
                                 table = "yrpc",
                                 reporters = "Uganda", 
                                 include_shortnames = TRUE, 
                                 include_communities = TRUE)
    OTS_UN_COMT <- rbind(OTS_UN_COMT, temp)
  }
  
  INFA <- ots_inflation_adjustment(OTS_UN_COMT, reference_year = 2010)
  identical(INFA[1:13], OTS_UN_COMT[1:13])
  add_vars(OTS_UN_COMT) <- add_stub(get_vars(INFA, 14:15), "_CT", FALSE)
  rm(INFA)
  get_vars(OTS_UN_COMT, c("reporter_iso","reporter_fullname_english")) <- NULL
  names(OTS_UN_COMT) <- c("Year","HS4","Product","Product_Short","HS2","Group","COM","Community","COMCOL","ISO","Country","IM_VAL","EX_VAL","IM_VAL_CT","EX_VAL_CT")
  get_vars(OTS_UN_COMT, c("COMCOL","ISO")) <- lapply(get_vars(OTS_UN_COMT, c("COMCOL","ISO")), toupper)
  vlabels(OTS_UN_COMT) <- c("Year","UN Harmonized System (HS) rev. 2007 4-digit product code","Full product name","Product short name (From The OEC, with modifications)",
                            "Product group code: UN Harmonized System (HS) rev. 2007 2-digit product code","Product group name","Product community code (From CID at Harvard University)","Product community name (From CID at Harvard University)","Product community colour (From CID at Harvard University)",
                            "ISO 3-digit country code","Country name","Import Value in current US$","Export Value in current US$","Import Value in constant 2010 US$","Export Value in constant 2010 US$")
    
  
  save(list = ls()[grep("^OTS_UN_COMT", ls())], file = "data/OTS_UN_COMT.RData")
  
###############################################
# WORLD BANK
###############################################
  
  library(wbstats)
  View(wb_cachelist$countries)
  View(wb_cachelist$indicators)
  View(wb_cachelist$sources)
  View(wb_cachelist$datacatalog)
  View(wb_cachelist$topics)
  View(wb_cachelist$income)
  View(wb_cachelist$lending)
  
  transWBAPI <- function(x, timevar = "date", vv = "value", newtv = c("Year","Date")) {
    newtv <- match.arg(newtv)
    naml <- unique(get_vars(x, c("indicatorID","indicator")))  
    res <- dcast(qDT(x), as.formula(paste0(timevar, " ~ indicatorID")), value.var = vv)
    res[[1L]] <- switch(newtv, Year = as.numeric(res[[1L]]), as.Date(res[[1L]]))
    get_vars(res, fNobs(res) <= 1L) <- NULL
    vlabels(res) <- c(newtv, naml[[2L]][collapse:::ckmatch(names(res)[-1], naml[[1L]], "Unmatched series:")])
    names(res) <- c(newtv, gsub("\\.", "_", names(res)[-1]))
    return(switch(newtv, Date = dateexpand(res), res))
  }
  
  # Download new list of indicators... 
  datacatalog <- wbdatacatalog()
  indlist <- wbindicators()
  
  country <- "UGA"
  enddate <- 2030
  
  load("data/WB.RData")
  
  # WDI for Uganda --------------------------------------------------------------------------
  ind <- fsubset(indlist, source == "World Development Indicators")[[1]]
  WB_WDI <- wb(country = country, indicator = ind, startdate = 1960, enddate = enddate)
  WB_WDI <- transWBAPI(WB_WDI)
  
  # Satistical Capacity: Exists ----------------------------------
  
  # Education Statistics: Exists ---------------------------------
  ind <- fsubset(indlist, source == "Education Statistics")[[1]]
  WB_EDU <- wb(country = country, indicator = ind, startdate = 1960, enddate = enddate)
  WB_EDU <- transWBAPI(WB_EDU)
  
  # Health, Nutrition and Population Statistics: Exists ----------
  ind <- fsubset(indlist, source == "Health Nutrition and Population Statistics")[[1]]
  WB_HEA <- wb(country = country, indicator = ind, startdate = 1960, enddate = enddate)
  WB_HEA <- transWBAPI(WB_HEA)
  
  # Poverty and Equity: Exists -----------------------------------
  # ERROR !
  # ind <- fsubset(indlist, source == "Poverty and Equity")[[1]]
  # WB_POV <- wb(country = country, indicator = ind, startdate = 1960, enddate = enddate)
  # WB_POV <- transWBAPI(WB_POV)
  
  # GEM Commodities ----------------------------------------------
  # No indicators !
  # ind <- fsubset(indlist, source == "GEM Commodities")[[1]]
  # WB_QPSD <- wb(country = country, indicator = ind, freq = "M", startdate = "1960M01", enddate = paste0(enddate,"M01"), POSIXct = TRUE)
  # WB_QPSD <- transWBAPI(WB_QPSD, timevar = "date_ct", newtv = "Date")
  
  # International Debt Statistics ------------------------------
  # No data returned !
  # ind <- fsubset(indlist, source == "International Debt Statistics")[[1]]
  # WB_IDS <- wb(country = country, indicator = ind, startdate = 1960, enddate = enddate)
  # WB_IDS <- transWBAPI(WB_IDS)
  
  # Quarterly Public Sector Debt ---------------------------------
  # No data !
  # ind <- fsubset(indlist, source == "Quarterly Public Sector Debt")[[1]]
  # WB_QPSD <- wb(country = country, indicator = ind, freq = "Q", startdate = "1960Q1", enddate = paste0(enddate,"Q1"), POSIXct = TRUE)
  # WB_QPSD <- transWBAPI(WB_QPSD, timevar = "date_ct", newtv = "Date")
  
  # Global Economic Monitor --------------------------------------------------------------------------
  gemind <- fsubset(indlist, source == "Global Economic Monitor")[[1]]
  WB_GEM <- wb(country = country, indicator = gemind, freq = "M", startdate = "1960M01", enddate = paste0(enddate,"M01"), POSIXct = TRUE)
  WB_GEM <- transWBAPI(WB_GEM, timevar = "date_ct", newtv = "Date")

  # Global Economic Monitor Commodities --------------------------------------------------------------------------
    # gemindc <- fsubset(indlist, source == "Commodity Prices- History and Projections")[[1]]
    # WB_GEMC <- wb(indicator = gemindc, freq = "M", mrv = 300, POSIXct = TRUE)
    # # labels <- WB_GEMC$indicator  
    # WB_GEMC <- dcast(qDT(WB_GEMC), date_ct ~ indicatorID, value.var = "value")
    # matches <- anyNAerror(match(names(WB_GEMC)[-1], wb_cachelist$indicators$indicatorID), "Unmatched series")
    # vlabels(WB_GEMC) <- c("Date", wb_cachelist$indicators$indicator[matches])
    # vlabels(WB_GEMC, "url")[-1] <- wb_cachelist$indicators$sourceOrg[matches]
    # WB_GEMC <- dateexpand(WB_GEMC)

  # Global Financial Development --------------------------------------------------------------------------
  ind <- fsubset(indlist, source == "Global Financial Development")[[1]]
  WB_GFD <- wb(country = country, indicator = ind, startdate = 1960, enddate = enddate)
  WB_GFD <- transWBAPI(WB_GFD)
  
  # Global Financial Inclusion (Global Findex) Database --------------------------------------------------------------------------
  # ERROR !
  # ind <- fsubset(indlist, source == "Global Financial Inclusion")[[1]]
  # WB_GFI <- wb(country = country, indicator = ind, startdate = 1960, enddate = 2020)
  # WB_GFI <- transWBAPI(WB_GFI)

  # Quarterly External Debt Statistics SDDS --------------------------------------------------------------------------
  # ERROR !
  # ind <- fsubset(indlist, source == "Quarterly External Debt Statistics SDDS")[[1]]
  # WB_QED <- wb(country = country, indicator = ind, freq = "Q", startdate = "1960Q1", enddate = "2020Q1", POSIXct = TRUE)
  
  # Doing Business --------------------------------------------------------------------------------------------------------
  # Error: ndicator parameter has no valid values.
  # ind <- fsubset(indlist, source == "Doing Business")[[1]]
  # WB_DB <- wb(country = country, indicator = ind, startdate = 1960, enddate = enddate)
  # WB_DB <- transWBAPI(WB_DB)
  
  # Enterprise Surveys ----------------------------------------------------------------------------
  # ERROR !
  # ind <- fsubset(indlist, source == "Enterprise Surveys")[[1]]
  # WB_ES <- wb(country = country, indicator = ind, startdate = 1960, enddate = 2020)
  
  # Joint External Debt Hub -----------------------------------------------------------------------
  # ERROR !
  # ind <- fsubset(indlist, source == "Joint External Debt Hub")[[1]]
  # WB_JED <- wb(country = country, indicator = ind, startdate = 1990, enddate = 2020)

  # Worldwide Governance Indicators -----------------------------------------------------------------
  # ERROR !
  # ind <- fsubset(indlist, source == "Worldwide Governance Indicators")[[1]]
  # WB_WGI <- wb(country = country, indicator = ind, startdate = 1990, enddate = 2020)
  # WB_WGI <- transWBAPI(WB_WGI)
  
  
  
  save(list = ls()[grep("^WB_", ls())], file = "data/WB.RData")
  
############################################
# ILO
############################################
  
  # library(Rilostat)

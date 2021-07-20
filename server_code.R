##############################
# Data Portal Server-Side Code
##############################
# Note: Some helper functions are defined in app.R

# This changes the list of available datasets, depending on the selected dataset source
observe({ 
  updateSelectInput(session, "dataset", 
  choices = switch(input$source, 
                   `Bank of Uganda (BoU)` = list(`Selected Macroeconomic Indicators` = "Selected Macroeconomic Indicators",
                                           `Monetary and Financial Sector Statistics` = c("Daily Interest Rates","Monthly Interest Rates","Treasury Bond Interest Rates","Daily Exchange Rates","Monthly Exchange Rates","Credit to the Private Sector","Financial Soundness Indicators"),
                                           `External Sector Statistics` = c("Terms of Trade","Composition of Exports (Values and Volumes)","Composition of Imports","Destination of Exports","Origin of Imports"),
                                           `Real Sector Statistics` = c("Consumer Price Index","Business Tendency Indicator (BTI)")),
                   `International Monetary Fund (IMF)` = list(`Cross-Cutting Datasets` = c("World Economic Outlook (WEO) (Full)","Sub-Saharan Africa Regional Economic Outlook (AFRREO) (Full)","International Financial Statistics (IFS)"),
                                                              `External Sector Statistics` = c("Balance of Payment and International Investment Position (BOP)","Direction of Trade Statistics (DOT)","Commodity Terms of Trade (PCTOT)","Export Quality (EQ)","Export Diversification (ED)"),
                                                              `Real Sector Statistics` = c("Primary Commodity Prices (PCPS) (Global)","Consumer Price Index (CPI)"),
                                                              `Fiscal Sector Statistics` = c("Government Finance Statistics (GFS)","Fiscal Monitor (FM)","Private and Public Capital Stock (PGCS)","World Revenue Longitudinal Data (WoRLD)"),
                                                              `Financial Sector Statistics` = c("Financial Access Survey (FAS)","Financial Development Index (FDI)","Financial Soundness Indicators (FSI)","Monetary and Financial Statistics (MFS)")),
                   `World Bank (WB)` = c("Global Economic Monitor", "World Development Indicators", "Education Statistics", "Health, Nutrition and Population Statistics", "Global Financial Development"),
                   `International Labour Organization (ILO)` = c("Sectoral Employment", "Sectoral Employment Shares"),
                   `UN Comtrade / Open Trade Statistics` = "Bilateral Trade")
  )
})

# This fetches the right dataset, for any dataset of any source requested. 
# For some datasets, we can provide various versions. This needs to be implemented in ui.R as well through a conditional panel. 
datasetInput <- reactive({ # RAW INPUT DATSAET
  switch(input$dataset,
         "Selected Macroeconomic Indicators" = switch(input$SMSfrequency, Monthly = BOU_MMI, `Annual CY` = BOU_MMI_A, `Annual FY` = BOU_MMI_AF),
         "Daily Interest Rates" = BOU_I,
         "Monthly Interest Rates" = BOU_I_M,
         "Treasury Bond Interest Rates" = BOU_I_TB,
         "Daily Exchange Rates" = BOU_E,
         "Monthly Exchange Rates" = BOU_E_M,
         "Credit to the Private Sector" = switch(input$PSCformat, Long = BOU_PSC, Wide = BOU_PSC_wide), 
         "Financial Soundness Indicators" = BOU_FS,
         "Terms of Trade" = BOU_TOT,
         "Composition of Exports (Values and Volumes)" = BOU_EX_C,
         "Composition of Imports" = BOU_IM_C,
         "Destination of Exports" = BOU_EX_D,
         "Origin of Imports" = BOU_IM_O,
         "Consumer Price Index" = BOU_CPI,
         "Business Tendency Indicator (BTI)" = BOU_BTI,
         "World Economic Outlook (WEO) (Full)" = IMF_WEO,
         "Sub-Saharan Africa Regional Economic Outlook (AFRREO) (Full)" = IMF_AFRREO,
         "International Financial Statistics (IFS)" = switch(input$IFSfrequency, Monthly = IMF_IFS, Quarterly = IMF_IFSQ),
         "Balance of Payment and International Investment Position (BOP)" = IMF_BOP,
         "Direction of Trade Statistics (DOT)" = IMF_DOT,
         "Commodity Terms of Trade (PCTOT)" = IMF_PCTOT,
         "Export Quality (EQ)" = IMF_EQ,
         "Export Diversification (ED)" = IMF_ED,
         "Primary Commodity Prices (PCPS) (Global)" = IMF_PCPS,
         "Consumer Price Index (CPI)" = IMF_CPI,
         "Government Finance Statistics (GFS)" = switch(input$GFSdataset, `Main Aggregates and Balances` = IMF_GFSMAB, Revenue = IMF_GFSR, Expenditure = IMF_GFSE),
         "Fiscal Monitor (FM)" = IMF_FM,
         "Private and Public Capital Stock (PGCS)" = IMF_PGCS,
         "World Revenue Longitudinal Data (WoRLD)" = IMF_WoRLD,
         "Financial Access Survey (FAS)" = IMF_FAS,
         "Financial Development Index (FDI)" = IMF_FDI,
         "Financial Soundness Indicators (FSI)" = IMF_FSI,
         "Monetary and Financial Statistics (MFS)" = IMF_MFS,
         "World Development Indicators" = WB_WDI,
         "Global Economic Monitor" = WB_GEM,
         "Education Statistics" = WB_EDU,
         "Health, Nutrition and Population Statistics" = WB_HEA, 
         "Global Financial Development" = WB_GFD, 
         "Bilateral Trade" = OTS_UN_COMT_BIL,
         "Sectoral Employment" = ILO_EMP_T,
         "Sectoral Employment Shares" = ILO_EMP_S)
})


# The dataset selected is in datasetInput(). This gets the variables and updates the select variables box. 
observe({ 
  
   nam <- names(datasetInput())
   updateSelectizeInput(session, "vars", label = "Select Variables", choices = c("All Variables" = "", nam), selected = "All")
   
})

# If the checkbox 'Aggregate Data' is clicked, this updates the list of variables you can select as id-variables with the variables selected above
observe({
  
  if(input$aggregate) {
    
    nam <- if(!input$applyselect || is.null(input$vars)) # Note that observe() environemnts are independent, so we can use the same variable 'nam' here without affecting the previous observe()
        names(datasetInput()) else if(input$excselect) 
        setdiff(names(datasetInput()), input$vars) else input$vars

    updateSelectizeInput(session, "aggIDvars", label = "Aggregator ID Variables",
                      choices = c("Choose variables to aggregate by" = "", nam))
    
  }
  
})

# This generates the final dataset after selecting from, filtering and aggregating the raw data. If none of this is done, the raw data is returned. 
DATA <- reactive({  # DATASET WITH SELECTED VARIABLES
         
    # Select Variables
    out <- if(!input$applyselect || is.null(input$vars)) datasetInput() else 
           selectvars(datasetInput(), input$vars, input$excselect)
     
    # Filter
    if(input$applyfilter && input$filter) {
      
      if(!is.null(input$filterInput)) out <- eval(parse(text = paste0("fsubset.data.frame(out, ",input$filterInput,")")))
      
    }
    
    # Aggregate
    if(input$applyaggregate && input$aggregate && !is.null(input$aggIDvars)) {
      
        out <- collapv(out, 
                       by = input$aggIDvars, 
                       FUN = input$aggfuns, 
                       catFUN = fmode, # If applicable, categorical data is aggregated with the statistical mode (which defaults to the first value if all elements are distinct or identical)
                       na.rm = input$aggregatenarm,
                       cols = if(input$aggregatedropcat) setdiff(num_vars(out, "names"), input$aggIDvars) else NULL, # Option to drop categorical data
                       keep.col.order = input$aggregatekeepcolord,
                       return = if(length(input$aggfuns) == 1L || !input$aggregate_long) # When multiple functions are selected, a long format can be returned. categorical data is duplicated if applicable.
                         "wide" else if(!input$aggregatedropcat && length(setdiff(cat_vars(out, "names"), input$aggIDvars))) "long_dupl" else "long") 
        
        if(length(input$aggfuns) > 1L) {
          
          if(input$aggregate_long) {
            
            out$Function <- substr(out$Function, 2L, 10000L) 
            
          } else {
            
            id <- match(input$aggIDvars, names(out))
            names(out)[-id] <- substr(names(out)[-id], 2L, 10000L)
            
          }
        }
    }
    
    # Return the result
    out
})

# This generates the table of summary statistics
output$summary <- renderTable(mysummary(DATA()), 
                              sanitize.text.function = function(x) x) #, style = "overflow-y: scroll;overflow-x: scroll;")

# This generates main data table, teh first thing you see when the app starts
output$DATAtable <- DT::renderDataTable(namlabHTML(DATA()),
                                # column filter on the top
                                filter = 'top', server = TRUE, escape = FALSE,
                                style = 'bootstrap',
                                options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE)) 

# This generates the 'Variables' table
output$Vinf <- DT::renderDataTable(vlabclsrc(DATA()),
                                    filter = 'top', server = TRUE, escape = FALSE,
                                    style = 'bootstrap',
                                    options = list(paginate = FALSE)) 

# This generates a static correlations plot
output$corrmat <- renderPlot({ # GGCORRPLOT
  
  cleandat <- num_vars(DATA())
  if(!all(varl <- varying(cleandat))) get_vars(cleandat, !varl) <- NULL # Removing invariant variables
  if(fncol(cleandat) > 200L) stop("Cannot plot correlation matrix greater than 200 x 200. Please use the interactive matrix or reduce the number of variables in the data.")
  
  if(input$vlabs) names(cleandat) <- vlabels(cleandat)
  
  r <- pwcor(cleandat, P = TRUE, array = FALSE)
  
  if(anyNA(r$r)) {
    rna <- is.na(r$r)
    r$r[rna] <- 0
    r$P[rna] <- 1
  } 
  
  diag(r$P) <- 1
  
  ggcorrplot(r$r, 
             p.mat = r$P, 
             lab = TRUE,
             hc.order = !input$corrclust, 
             type = 'full', 
             outline.col = "white", 
             lab_size = input$corfontsize, 
             pch.cex = input$corfontsize + 1L,
             insig = if(isTRUE(input$insigblank)) "blank" else "pch") + 
   coord_fixed(0.65) # show.diag = TRUE
  
}) #, height = 850)


# This generates an interactive version of the plot, with scatterplot attached
output$iplorcorr <- iplot_render({ # INTERACTIVE CORRPLOT
  
  cleandat <- num_vars(DATA())
  
  if(!all(varl <- varying(cleandat))) get_vars(cleandat, !varl) <- NULL # Removing invariant variables
  if(fnrow(cleandat) > 10000L) stop("Cannot compute interactive correlation matrix for data with more than 10,000 rows. Please choose a static matrix, or filter or aggregate to reduce the size of the data.")
  if(fncol(cleandat) > 500L) stop("Cannot plot correlation matrix greater than 500 x 500. Please reduce the number of variables in the data.")
  
  if(input$vlabs) names(cleandat) <- vlabels(cleandat)
  
  iplotCorr(cleandat, 
            group = NULL, 
            reorder = !input$corrclust2, 
            chartOpts = list(cortitle = "Correlation Matrix",
                             scattitle = "Scatterplot",
                             corcolors = c("blue", "white", "red"),
                             rectcolor = "white"))
})

# This prints the pairwise correlations, together with observation count and 5% significance star
output$Cmat <- renderPrint({ 
  
  cleandat <- num_vars(DATA())
  
  if(!all(varl <- varying(cleandat))) get_vars(cleandat, !varl) <- NULL # Removing invariant variables
  if(fncol(cleandat) > 200L) stop("Cannot print correlation matrix greater than 200 x 200. Please use the interactive matrix or reduce the number of variables in the data.")
  
  if(input$vlabs) names(cleandat) <- vlabels(cleandat)
  
  print(pwcor(cleandat, N = TRUE, P = TRUE))
  
}, width = 150)
  


# Download Handler
output$downloadData <- downloadHandler(
  
  filename = function() {
    paste0(input$dataset, 
          switch(input$DLformat,
                 Excel = ".xlsx",
                 CSV = ".csv",
                 TAB = ".tsv",
                 R = ".rds",
                 STATA = ".dta",
                 SPSS = ".sav",
                 SAS = ".sas"
                 ))
  },
  
  content = function(file) {
    switch(input$DLformat,
           Excel = write_xlsx(list(data = DATA(), labels = namlab(DATA())), file),
           CSV = write_csv(DATA(), file),
           TAB = write_tsv(DATA(), file),
           R = saveRDS(DATA(), file),
           STATA = write_dta(renameSTATA(DATA()), file, version = input$STATAversion), # STATA version 12 default for better compatibility.
           SPSS = write_sav(DATA(), file),
           SAS = write_sas(DATA(), file)
    )
  }
  
)




#######################################
# Data Catalog
#######################################

output$datasources <- DT::renderDataTable(DS_SRC,
                                          filter = 'top', server = TRUE, escape = FALSE,
                                          style = 'bootstrap',
                                          options = list(pageLength = 15, scrollX = TRUE, # , autoWidth = TRUE
                                                         # https://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width
                                                         columnDefs = (list(list(width = '50%', targets = 3), 
                                                                            list(width = '30%', targets = 4)))))
output$datasetdesc <- DT::renderDataTable(DS_DESC_UPDATED,
                                          filter = 'top', server = TRUE, escape = FALSE,
                                          style = 'bootstrap',
                                          options = list(pageLength = 50, scrollX = TRUE, # , autoWidth = TRUE
                                                         columnDefs = (list(list(width = '40%', targets = 4)))))
                                                           

output$dataoverview <- DT::renderDataTable(IND_OV,
                                           filter = 'top', server = TRUE, escape = FALSE,
                                           style = 'bootstrap', 
                                           options = list(pageLength = 15, scrollX = TRUE)) # , autoWidth = TRUE
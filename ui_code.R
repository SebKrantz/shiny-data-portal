####################################################
# Data Portal User Interface (the 'Data Access' tab)
####################################################

# Note: All of this code generates HTML from R

div(class="outer",
    
    # This includes a CSS stylesheet 'style.css' setting the appearance of the app. This sheet can be edited separately.
    includeCSS("style.css"), 
    
    # This is the sidebar panel (left-panel) of the application. 
    sidebarLayout(               # These appearance controls were outsourced to style sheet:
      absolutePanel(id = "side", # width = "20%", height = "auto", top = NULL, left = NULL, bottom = NULL, right = "auto", #94.5%
                    
                    # This provides some controls for the correlation matrix displayed. Only active if user clicks on the 'Correlations' tab. 
                    conditionalPanel(condition = "input.tabs=='Correlations'",
                                     tabsetPanel(
                                       tabPanel("Interactive",
                                                h4("Interactive Correlation Matrix"),
                                                checkboxInput("corrclust2","Don't Cluster Correlation Matrix")),
                                       tabPanel("Matrix/Plot",
                                                h4("Correlation Matrix"),
                                                checkboxInput("corrclust","Don't Cluster Correlation Matrix"),
                                                checkboxInput('insigblank','Remove Coefficients Insignificant at 5% Level'),
                                                checkboxInput("showCMat","Show Classical Matrix"),
                                                sliderInput('corfontsize','Font Size', 2, 7, value = 4, step = 0.5)), id = "tabsIN"
                                     ),
                                     checkboxInput("vlabs", "Show Variable Labels"),
                                     tags$hr()),
                    
                    # These are the main controls for selecting, filtering and aggregating data.  
                    conditionalPanel(condition = "input.tabs=='Table' || input.tabs=='Variables' || input.tabs=='Summary'",
                                     h3("Download Data"),
                                     # Enter Data Sources:
                                     selectInput("source", "Data Source",
                                                 choices = c("Bank of Uganda (BoU)", 
                                                             "International Monetary Fund (IMF)", 
                                                             "World Bank (WB)", 
                                                             "International Labour Organization (ILO)",
                                                             "UN Comtrade / Open Trade Statistics")),
                                     # This is for selecting a dataset from a source. It will be updated once the source was selected.
                                     selectInput("dataset", "Select Dataset", choices = NULL),
                                     # Special cases: For some datasets we could present different versions or sub-datasets, available through additional controls visible only if the dataset is selected. 
                                       conditionalPanel(condition = "input.dataset=='Selected Macroeconomic Indicators'",
                                                        radioButtons('SMSfrequency', 'Frequency', 
                                                                     choices = list('Monthly', "Annual CY", "Annual FY"), selected = 'Monthly', inline = TRUE)),
                                       conditionalPanel(condition = "input.dataset=='Credit to the Private Sector'",
                                                        radioButtons('PSCformat', 'Format', 
                                                                     choices = list('Long', 'Wide'), selected = 'Long', inline = TRUE)),
                                       conditionalPanel(condition = "input.dataset=='International Financial Statistics (IFS)'",
                                                        radioButtons('IFSfrequency', 'Frequency', 
                                                                     choices = list('Monthly', "Quarterly"), selected = 'Monthly', inline = TRUE)),
                                       conditionalPanel(condition = "input.dataset=='Government Finance Statistics (GFS)'",
                                                        radioButtons('GFSdataset', 'Dataset', 
                                                                     choices = list('Main Aggregates and Balances', 'Revenue', 'Expenditure'), selected = 'Main Aggregates and Balances'))
                    ),
                    
                    # Select Variables Controls are always shown
                    selectizeInput("vars", "Select Variables", choices = c("All" = ""), multiple = TRUE),
                    checkboxInput("excselect", "Select Inverse (Exclude)", FALSE),
                    bsButton("applyselect", "Apply Selection", type = "toggle", style = "primary"),
                    tags$hr(),
                    
                    # Filter, aggregate and download data
                    conditionalPanel(condition = "input.tabs=='Table' || input.tabs=='Variables' || input.tabs=='Summary'",
                                     
                                     # Filter data (subset rows)
                                     checkboxInput("filter", "Filter Data"),
                                     conditionalPanel(condition = "input.filter",
                                                      bsPopover(id = "filterhelp", title = "Specifying a Filter Query in R", 
                                                                content =  'Valid queries contain variable names and logical comparison operators: >, <, ==, >=, <=, & [and], | [or], ! [negate], %in% [contains], %!in% [does not contain]. For Example Var1 > 100 & Var2 < Var3 means select rows where Var1 is greater that 100 and Var2 is lower than Var3. Conditions can be grouped with braces e.g. (Var1 == 1 | Var2 < 3) & Var3 == "Yes". R is case-sensitive, and character values must be supplied in quotes e.g. Country == "Italy". Multiple character matches must be conactenated e.g. Varx %in% c("A","B","D").',
                                                                placement = "right", 
                                                                trigger = "focus", 
                                                                options = list(container = "body")
                                                      ),
                                      textInput("filterInput", HTML("Filter Query    <button id='filterhelp' type='button' class='btn btn-primary action-button btn-xs'><i class='fa fa-question'></i></button>"), placeholder = 'e.g. Year > 2000 & Quarter == "Q4"'),
                                      # Need an extra 'Apply Filter' checkbox, otherwise application reacts immediately while we are typing the filter query.
                                      bsButton("applyfilter", "Apply Filter", type = "toggle", style = "primary"),
                                     ),
                                     tags$hr(), # horizontal rule
                                     # Aggregate data by id variables
                                     checkboxInput("aggregate", "Aggregate Data"),
                                     conditionalPanel(condition = "input.aggregate",
                                                      # List of available variables will be updated based on data selected.
                                                      selectizeInput("aggIDvars", "Aggregator ID Variables", choices = c("Choose variables to aggregate by" = ""), multiple = TRUE),
                                                      # List of available functions to aggregate numeric data ('collapse' package fast functions). Multiple can be selected. Any categorical data will be aggregated with the statistical mode (default set on the server side).
                                                      bsPopover(id = "aggfunshelp", title = "Aggregator Functions and Formats", 
                                                                content =  'Functions chosen will be applied to numeric columns in the data. Unless the box "Drop Categorical Variables" is checked, categorical columns are aggregated using the statistical mode i.e. the most frequently ocurring value, which selects the first value if all elements are equal or distinct. Multiple aggregator functions can be selected. In that case either columns will be prefixed with the function names, or stacked if the box "Long Format" is checked. If "Remove Missing Values" is checked, statistics are computed on the non-missing values in each group.',
                                                                placement = "right", 
                                                                trigger = "focus", 
                                                                options = list(container = "body")
                                                      ),
                                                      selectizeInput("aggfuns", HTML("Aggregator Functions    <button id='aggfunshelp' type='button' class='btn btn-primary action-button btn-xs'><i class='fa fa-question'></i></button>"), 
                                                                         choices = list(mean = "fmean",
                                                                                        sum = "fsum",
                                                                                        product = "fprod",
                                                                                        median = "fmedian",
                                                                                        mode = "fmode",
                                                                                        minimum = "fmin",
                                                                                        maximum = "fmax",
                                                                                        `first value` = "ffirst",
                                                                                        `last value` = "flast",
                                                                                        `variance` = "fvar",
                                                                                        `standard deviation` = "fsd",
                                                                                        `number of observations` = "fnobs",
                                                                                        `number of distinct values` = "fndistinct"), selected = "fmean", multiple = TRUE),
                                                      conditionalPanel(condition = "input.aggfuns.length > 1", # this is a JavaScript expression: If more than one aggregator function, display option to reshape data to long format.
                                                                       checkboxInput("aggregate_long", "Long Format")),
                                                      checkboxInput("aggregatenarm", "Remove Missing Values", value = TRUE),
                                                      checkboxInput("aggregatedropcat", "Drop Categorical Variables"),
                                                      checkboxInput("aggregatekeepcolord", "Preserve Column Order", value = TRUE),
                                                      bsButton("applyaggregate", "Aggregate", type = "toggle", style = "primary")
                                     ),
                                     tags$hr(), # horizontal rule
                                     # This Gives the download options
                                     radioButtons('DLformat', 'Choose Format', choices=list(Excel = 'Excel', 
                                                                                          CSV = 'CSV', 
                                                                                          TAB = 'TAB', 
                                                                                          R = 'R',
                                                                                          STATA = 'STATA',
                                                                                          SAS = 'SAS', 
                                                                                          SPSS = 'SPSS'
                                     ), selected = 'Excel', inline = TRUE), # , inline = TRUE
                                     conditionalPanel(condition = "input.DLformat == 'STATA'",
                                                      sliderInput("STATAversion","STATA Version", 8, 15, 12, step = 1)
                                     ),
                                     # Download button
                                     downloadButton("downloadData", "Download"))
                    ),
                    
      # This generates the main (right) panel with the data table and statistical output                                               
      absolutePanel(id= "main" , # width = "80%", height = "100%", top = NULL, left = "20%", bottom = NULL, right = NULL,
                    tabsetPanel(type = "tabs",
                                tabPanel('Table',
                                         p(" "),
                                         div(class = "indicatortable",
                                         DT::dataTableOutput("DATAtable"))), 
                                
                                tabPanel('Variables', 
                                         div(class = "summarytable",
                                             p(" "),
                                             DT::dataTableOutput("Vinf"))),
                                
                                tabPanel('Summary', 
                                         div(class = "summarytable",
                                             tableOutput("summary")
                                         )),
                                         
                                tabPanel('Correlations', 
                                         conditionalPanel(condition = "input.tabsIN=='Matrix/Plot' && !input.showCMat",
                                                          plotOutput("corrmat")), 
                                         conditionalPanel(condition = "input.tabsIN=='Interactive'",
                                                          iplotCorr_output("iplorcorr")),
                                         conditionalPanel(condition = "input.tabsIN=='Matrix/Plot' && input.showCMat",
                                                          verbatimTextOutput("Cmat"))
                                         ), id = "tabs")
                    )
)) 

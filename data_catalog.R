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
legislatureInfoTabContent <- tabPanel("Infos zur Legislatur", value = 2,
                                      tabsetPanel(
                                        tabPanel("Allgemein",
                                                 br(),
                                                 uiOutput("dynamicInfoText"),
                                                 br(),
                                                 withSpinner(plotlyOutput("parliamentPlot"),type = 3),
                                                 br(),
                                                 #HTML("<h3>Tabelle: Anzahl Gesetze pro Themenbereich</h3>"),
                                                 #actionButton("toggleTable", "Tabelle ein-/ausklappen"),
                                                 #shinyjs::hidden(
                                                 #  div(id = "tableContainer", DT::dataTableOutput("ergebnisse_tabelle"))
                                                 #),
                                                 #hr(),
                                                # DT::dataTableOutput("ergebnisse_tabelle"),
                                                 #withSpinner(plotlyOutput("histogram7"), type = 3)
                                        ),
                                        tabPanel("Politisches Koordinatensystem",
                                                   tabPanel("Erklärung - Voreingestellt",
                                                            br(),
                                                            HTML(politicalSystemText),  # Annahme, dass du bereits eine Variable `politicalSystemText` definiert hast
                                                            br(),
                                                            plotlyOutput("koordinatensystem2")
                                                   )
                                                 )
                                        
                                      )
)


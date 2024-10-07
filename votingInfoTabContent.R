votingInfoTabContent <- tabPanel("Infos zur Abstimmung",
                                 tabsetPanel(
                                   tabPanel("Allgemein",
                                            br(),
                                            tags$div(uiOutput("TitelGesetz"), style = "margin-bottom: 30px;"),
                                            br(),
                                            plotlyOutput("abstimmungsergebnis", height = "230px", width = "100%"),
                                            br(),
                                            uiOutput("voteOutcome")
                                   ),
                                   tabPanel("Nach Parteien",
                                            br(),
                                            plotlyOutput("stackedBarPlot"),
                                            br()
                                   ),
                                   tabPanel("Nach Geschlecht",
                                            br(),
                                            plotlyOutput("histogram5")
                                   ),
                                   tabPanel("Disziplin",
                                            br(),
                                            plotlyOutput("histogram6"),
                                            br(),
                                            verbatimTextOutput("anovaResults")
                                   ),
                                   tabPanel("Berufe", value = 5,
                                            br(),
                                            HTML("<h3>Wordcloud der Bildungshintergründe jedes Abgeordneten</h3>"),
                                            wordcloud2Output("wordcloud")
                                   ),
                                   tabPanel("Regionale Verteilung", value = 4,
                                            br(),
                                            HTML("<h3>Wohnorte/Wahlbezirke der Abgeordneten</h3>"),
                                            #leafletOutput("leaflet_map", width = "80%", height = "550px"),
                                            br(),
                                            leafletOutput("leaflet_map1", width = "80%", height = "550px")
                                   ),
                                   tabPanel("Nominate-Scores", value = 5,
                                            br(),
                                            HTML("<h3>Auswertung Politischer Positionenen</h3>"),
                                            plotlyOutput("nominate_plot"),
                                   ),
                                   tabPanel("Datentabelle", value = 3,
                                            br(),
                                            HTML("<h3>Rohdaten Tabelle</h3>"),
                                            DT::dataTableOutput("table")
                                   )
                                 )
)

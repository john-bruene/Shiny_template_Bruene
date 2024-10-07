sidebarContent <- sidebarPanel(
  id = "sidebar", width = 4,
  h4("Option Panel", style = "font-weight: 700;"),
  
  withMathJax(), # for latex-ish mathjax code (how? Look into the input widgets)
  
  # Text input
  
  # Select Box / Dropdown Input
  selectInput("data",
              label = "Legislaturperiode", 
              choices = list("2021-2025" = 1, "2017-2021" = 2, "2013-2017" = 3, "2009-2013" = 4),
              selected = 1),
  
  hidden(div(id = "poliOptions",
  
  textInput("search_input", "Suche nach einer Abstimmung:", value = ""),
  selectInput("selected_variable", "Zeige Ergebnisse für Abstimmung:", choices = NULL),
  
  HTML("<hr>"),
  
  # Slider for Age
  sliderInput("age_slider", "Alter der Abgeordneten", min = 18, max = 99, value = c(18, 99), step = 1),
  uiOutput("filtered_obs_info"),
  HTML("<hr>"),
  
  )),
  
  # Versteckte Clusteranalyse-Einstellungen
  hidden(div(id = "clusterOptions",
             tags$div(
               tags$h4("Einstellungen Clusteranalyse")),
             numericInput("numClusters", "Anzahl der Cluster:", value = 4, min = 2, max = 10),
             
             selectInput("linkage", 
                         "Linkage Methode auswählen:", 
                         choices = c("Single" = "single", 
                                     "Complete" = "complete", 
                                     "Average" = "average", 
                                     "Ward.D2" = "ward.D2"),
                         selected = "ward.D2"),
             
             dropMenu(
               actionButton("go1", "Variablenauswahl"),
               checkboxGroupInput("selectedVars", "Variablen auswählen:",
                                  choices = c("Geschlecht" = "sex", 
                                              "Stimme in akt. Abstimmung " = "vote", 
                                              "Loyalitätsprozentsatz" = "loyalty_percentage", 
                                              "Antwortprozentsatz" = "answered_percentage", 
                                              "Alter" = "age", 
                                              "Wirtschaftliche Position" = "economic_position", 
                                              "Soziale Position" = "social_position",
                                              "Außenpolitik" = "foreign_policy_score", 
                                              "Verteidigung" = "defense_score",
                                              "Gesundheit" = "health_score", 
                                              "Energie" = "energy_score",
                                              "Migration und Aufenthaltsrecht" = "migration_score",
                                              "Arbeit und Beschäftigung" = "employment_score",
                                              "Umwelt" = "environment_score",
                                              "Recht" = "law_score"),
                                  
                                  selected = c("economic_position", "social_position", "defense_score", "migration_score")),
               selectInput("selectedVariable", "Variable für Boxplots auswählen (Tab 'Cluster Profile'):", choices = NULL), 
               theme = "light-border",
               placement = "right",
               arrow = FALSE
             ),
             HTML("<hr>")
  )),
  
  HTML("<hr>"),
  
  
  HTML(introText),
  dropMenu(
    actionButton("go0", "Experimentelle Features"),
    tags$div(
      tags$h4("Experimentelle Features"),
      checkboxInput("checkboxUnion", label = "Kombiniere CDU und CSU zu Union", value = NULL),
      textInput("VoteID", label = "Auswahl über Abstimmungs-ID (Abg. Watch API)", value = "0000"),
      # Select Box / Dropdown Input for variable (to be updated dynamically)
      selectInput("selected_topic_x",
                  label = "Politisches Koordinatensystem X-Achse:",
                  choices = NULL), 
      selectInput("selected_topic_y",
                  label = "Politisches Koordinatensystem Y-Achse:",
                  choices = NULL), 
      
      
      downloadButton("downloadTable", "Aktuellen Datensatz herunterladen"),
    ),
    theme = "light-border",
    placement = "right",
    arrow = FALSE
  )
)

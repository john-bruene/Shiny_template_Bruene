# Call the helpers file
source("helpers.R")


# Define UI for random distribution app ----
ui <- fluidPage(
  
  includeCSS("style.css"),
  

  # App title ----
  titlePanel("Abgeordnetenwatch"),
  
  # Sidebar layout defined ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      id = "sidebar", width = 4,
      h3("Bundestag", style = "font-weight: 700;"),
      
      withMathJax(), # for latex-ish mathjax code (how? Look into the input widgets)
      
      br(), # fill in space
      
      # Text input

      
      # Select Box / Dropbdown Input
      selectInput("data",
                  label = "Legislaturperiode", 
                  choices = list("2021-2025" = 1, "2017-2021" = 2,"2013-2017" = 3,"2009-2013" = 4),
                  selected = 1),
      
      
    
      
      textInput("search_input", "Suche nach einer Abstimmung:", value = ""),
      selectInput("selected_variable", "Zeige Ergebnisse für Abstimmung:", choices = NULL),

      HTML("<hr>"),
      
      # Slider for Age
      sliderInput("age_slider", "Alter der Abgeordneten", min = 18, max = 99, value = c(18, 99), step = 1),
      uiOutput("filtered_obs_info"),
      HTML("<hr>"),
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
      HTML("<hr>"),

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
      ),
      
      
      # for further widgets see: https://shiny.rstudio.com/gallery/widget-gallery.html
      # or to render an input Widget in the server and call "uiOutput("...") in the ui: https://shiny.rstudio.com/reference/shiny/1.4.0/renderui
      # see other shinys for Code if the examples do not help
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # tags$head(tags$script(src="template.js")),
      
      # Output: tabsetPanel only needed for several tabs
      tabsetPanel(type = "tabs", id = "tabs",
                  tabPanel("Willkommen", value = 1,
                           br(),
                           HTML("
  <h3>Anleitung</h3>
  <p>Hier einige Tipps, um die App effektiv zu nutzen:</p>
  <ol>
    <li>Beginnen Sie damit, oben links eine <strong>Legislaturperiode auszuwählen</strong>. Es stehen vier Optionen zur Verfügung.</li>
    <li>Im ersten Tab <strong><em>Infos zur Legislatur</em></strong> lassen sich einige allgemeine Informationen finden, wie z.B. eine Tabelle, die die Anzahl der Gesetze pro Themenbereich enthält. Außerdem ein politisches Koordinatensystem der Abgeordneten.</li>
    <li>Der Tab <strong><em>Infos zur Abstimmung</em></strong> widmet sich einer konkret ausgewählten Abstimmung im Bundestag. Diese kann links ausgewählt und mit dem Suchfeld verfeinert werden.</li>
    <li>Innerhalb dieses Info-Tabs kann dann das <strong>Abstimmungsverhalten</strong> genauer betrachtet werden, wie z.B. das Abstimmungsverhalten der Fraktionen oder eine regionale Verteilung über Deutschland.</li>
    <li>Der <strong><em>Analyse</em></strong>-Tab kann unabhängig von einer konkreten Abstimmung betrachtet werden. Hier findet das Clustering innerhalb einer gewählten Legislatur statt. Ziel ist es, mithilfe der Auswahlmöglichkeiten links sinnvolle Cluster zu identifizieren.</li>
  </ol>
   <p>Achtung: Der Analyse Teil braucht ggf. mehr Rechenzeit, wenn zwischen den Daten oder der Clusteranzahl gewechselt wird. </p>
   
   <p>Aktuelles Feedback und dessen Umsetzungsstand ist <a href='https://docs.google.com/spreadsheets/d/1jy6AsNn5nPMMuD6fSeVRstj05TJU3U946OfEOJS3PBk/edit?usp=sharing' target='_blank'>hier</a> einsehbar. </p>
   
  <p>Für weitere Informationen, Fragen und Feedback freue ich mich über eine E-Mail: <a href='mailto:jfbruene@stud.uni-goettingen.de'>jfbruene@stud.uni-goettingen.de</a></p>
")
            
                  ),
                  tabPanel("Infos zur Legislatur", value = 1,
                           tabsetPanel(
                           tabPanel("Allgemein",
                             br(),
                             HTML("
                              <h3>Bundestag </h3>
                              <p>Hier finden Sie einige allgemeine Infos zur ausgewählten Legislatur</p>
                              <ol>
                              <li>Eine Tabelle über die Anzahl der Gesetze pro Thema</li>
                              <li>Ein Graph mit der Gesetzesanzahl pro Themenbereich sowie der Quote an fehlenden Abgeordnenten</li>
                              <li>Im nächsten Tab ein Politisches Koordinatensystem</li>
                              </ol>
                              "),
                             
                             br(),

                           DT::dataTableOutput("ergebnisse_tabelle"),
                           hr(),
                           withSpinner(plotlyOutput("histogram7"), type = 3),
                           ),
                           tabPanel("Politisches Koordinatensystem",
                                    tabsetPanel(
                                    tabPanel("Erklärung - Voreingestellt",
                                    br(),
                                    HTML('<div>
                              <h3>Politisches Koordinatensystem </h3>
                              <p>Hier sehen Sie beispielshaft die Politische Landschaft der Parteien zur Bundestagswahl 2017. Dieses Koordinatensystem illustriert die Positionierung deutscher Parteien nach wirtschafts- und gesellschaftspolitischen Ausrichtungen. Die horizontale Achse zeigt die wirtschaftspolitische Ausrichtung von links (sozialistisch) zu rechts (kapitalistisch). Die vertikale Achse veranschaulicht die gesellschaftspolitische Ausrichtung, von progressiv (oben) zu konservativ (unten). Die Einordnung der Parteien basiert auf Daten des Bundeswahlkompasses 2017.</p>
                              <img src="https://upload.wikimedia.org/wikipedia/commons/e/ea/Politische_Landschaft_der_Parteien_zur_Bundestagswahl_2017.png" height="500px" width="auto">
                              </ol>
                              </div>'),
                                    br(),
                                    plotlyOutput("koordinatensystem2"),
                                    
                                  ),
                                  tabPanel("Anpassbares Koordinatensystem (Experimentell)",
                                           br(),
                                           plotlyOutput("koordinatensystem3")
                                  )
                                  )
                           )
                           )
                  ),

                  tabPanel("Infos zur Abstimmung", 
                           tabsetPanel(
                             tabPanel("Allgemein",
                                      br(),
                                      tags$div(uiOutput("TitelGesetz"),
                                               style = "margin-bottom: 30px;"  
                                      ),
                                      br(),
                                      plotlyOutput("abstimmungsergebnis", height = "230px", width = "100%"),
                                      br(),
                                      uiOutput("voteOutcome")
                             ),

                             tabPanel("Nach Parteien",
                                      br(),
                                      plotlyOutput("stackedBarPlot"),
                                      br(),
                                      ),
                             tabPanel(
                               "Nach Geschlecht",
                               br(),
                               plotlyOutput("histogram5")
                             ),
                             tabPanel("Disziplin",
                                      br(),
                             plotlyOutput("histogram6")
                             ),
                             
                             tabPanel("Berufe", value = 5,
                                      br(),
                                      HTML("
                              <h3>Wordcloud der Bildungshintergründe jedes Abgeordneten</h3>"),
                                      wordcloud2Output("wordcloud")
                             ),
                             tabPanel("Regionale Verteilung", value = 4,
                                      br(),
                                      HTML("
                              <h3>Wohnorte/Wahlbezirke der Abgeordneten</h3>"),
                                      leafletOutput("leaflet_map", width = "80%", height = "550px")
                             ),
                             
                             tabPanel("Datentabelle", value = 3,
                                      br(),
                                      HTML("
                              <h3>Rohdaten Tabelle</h3>"),
                                      DT::dataTableOutput("table")
                             )
                           )
                  ),


                  tabPanel("Analyse", value = "analysis",
                           tabsetPanel(
                             tabPanel(
                               "Erklärung Methoden",
                               br(),
                               HTML("
                              <h1>Hierarchische Clusteranalyse</h1>
                              <h3>Erstellung des Dendrograms</h3>
                              <ol>
                              <li>Start der Clusteranalyse: Wir beginnen mit einer Verteilung von Datenpunkten, die in einem Koordinatensystem angeordnet sind. Jeder Punkt repräsentiert ein Objekt mit individuellen Eigenschaften.</li>
                              <li>Erste Clusterbildung: Nun startet der Prozess des Clusterns, bei dem erste Gruppen erkennbar werden. Objekte werden basierend auf ihrer Ähnlichkeit zusammengefasst.</li>
                               <li>Hervorhebung der Distanz: Die Analyse schreitet fort und zeigt auf, wie weit zwei spezifische Cluster voneinander entfernt sind, was auf ihre Unterschiedlichkeit hinweist.</li>
                              <li>Verfeinerung der Cluster: Die Clusterstruktur wird deutlicher. Gruppen von Objekten vereinen sich zu größeren Einheiten.</li>
                              </ol>
                              "),
                               br(),
                               div(
                                 style = "max-width: 700px; margin: auto;", 
                                 slickROutput("dendoCarousel")
                               ),
                               HTML("<hr>"),
                               HTML("
                              <h3>Welche Linkage Methode?</h3>
                              <ol>
                              <li><strong>complete:</strong> Maximale Distanz zwischen zwei Gruppen.</li>
                              <li><strong>single:</strong> Minimale Distanz zwischen zwei Gruppen.</li>
                              <li><strong>average:</strong> Durchschnittliche Distanz zwischen zwei Gruppen.</li>
                              <li><strong>Ward.D2:</strong> Diese Methode minimiert die Summe der quadrierten Differenzen innerhalb aller Gruppen. Es handelt sich um einen agglomerativen hierarchischen Clustering-Ansatz, der darauf abzielt, die Varianz innerhalb der Cluster zu minimieren. Bei jedem Schritt des Clustering-Prozesses wird das Paar von Clustern ausgewählt, dessen Zusammenführung die Zunahme der Gesamtvarianz innerhalb des Clusters minimiert.</li>
                              </ol>
                              "),
                               tags$img(src = "linkage1.png", style = "width: 60%; height: auto;"),
                               HTML("<hr>"),
                               HTML("
      <h3>Tree Cutting</h3>
      <p>Das Schneiden eines Dendrogramms in der hierarchischen Clusteranalyse ermöglicht es, eine komplexe Datenstruktur in eine überschaubare Anzahl von Gruppen zu unterteilen, die aufgrund ihrer Ähnlichkeit gebildet werden. Dieser Vorgang bietet mehrere Vorteile:</p>
      <ol>
      <li><strong>Übersichtlichkeit:</strong> Es reduziert die Komplexität der Daten durch Bildung einer handhabbaren Anzahl von Clustern.</li>
      <li><strong>Interpretierbarkeit:</strong> Cluster fassen ähnliche Objekte zusammen und erleichtern somit die Interpretation der Daten.</li>
      <li><strong>Anpassungsfähigkeit:</strong> Je nach Bedarf können unterschiedlich viele Cluster gebildet werden, um spezifische Analysebedürfnisse zu erfüllen.</li>
      <li><strong>Entdeckung von Mustern:</strong> Durch das Clustern können inhärente Muster und Beziehungen in den Daten entdeckt werden, die sonst verborgen bleiben würden.</li>
      <li><strong>Entscheidungsfindung:</strong> Die Ergebnisse des Tree Cuttings können in verschiedenen Bereichen zur Gruppenbildung und zur Unterstützung von Entscheidungsprozessen verwendet werden.</li>
      </ol>
      "),
                               
                               tags$head(
                                 tags$style(HTML("
            #image-row {
                display: flex; /* Verwendet Flexbox für das Layout */
                justify-content: center; /* Zentriert die Bilder horizontal */
                align-items: center; /* Zentriert die Bilder vertikal */
                margin-bottom: 20px; /* Fügt einen unteren Abstand hinzu */
            }
            .row-img {
                width: 100%; /* Stellen Sie die Bildbreite ein */
                max-width: 400px; /* Maximale Bildbreite */
                margin: 0 10px; /* Fügt horizontale Abstände zwischen den Bildern hinzu */
            }
        "))
                               ),
                               
                               # Container für die Bilder
                               div(id = "image-row",
                                   img(src = "cutting1.png", class = "row-img"),
                                   img(src = "cutting2.png", class = "row-img")
                               ),
                               HTML("<hr>"),
                               HTML("
  <h3>Der 'elbow'-Plot</h3>
  <p>Der elbow-Plot ist ein wesentliches Instrument, um die optimale Anzahl von Clustern für die Clusteranalyse zu bestimmen. Hierbei wird die Gesamtsumme der quadrierten Abstände innerhalb der Cluster für verschiedene Clusteranzahlen kalkuliert und grafisch dargestellt.</p>
  <ol>
    <li>Bei k = 1 starten wir mit nur einem Cluster, was bedeutet, dass alle Datenpunkte als eine einzige Gruppe betrachtet werden. Dies führt zu einer hohen Summe der quadrierten Abstände, da die Diversität innerhalb des Clusters ignoriert wird.</li>
    <li>Erhöhen wir die Clusteranzahl auf k = 2, fällt die Summe der quadrierten Abstände deutlich. Zwei Gruppen werden identifiziert, was eine bessere Repräsentation der Datenstruktur ermöglicht.</li>
    <li>Bei k = 3 wird die Abnahme der Summe weniger stark. Jedes hinzugefügte Cluster führt zu einem geringeren Rückgang, was auf eine Annäherung an die optimale Clusterzahl hinweist.</li>
    <li>Der 'Ellbogen' tritt oft bei einem spezifischen k-Wert auf, hier bei k = 4, wo die Verbesserung durch zusätzliche Cluster nur noch marginal ist. Dieser Punkt ist oft ein guter Kandidat für die ideale Anzahl von Clustern.</li>
  </ol>
  <p>Die Wahl der Clusteranzahl an der 'Ellbogen'-Stelle führt zu einer ausgewogenen Lösung, die eine angemessene Gruppentrennung bei gleichzeitiger Vermeidung einer Über-Clusterung bietet.</p>
"),
                               div(
                                 style = "max-width: 700px; margin: auto;",  
                                 tags$head(
                                   tags$style(HTML("
            #image-grid {
                display: grid;
                grid-template-columns: auto auto; /* Zwei Spalten */
                grid-gap: 10px; /* Abstand zwischen den Bildern */
                justify-content: center; /* Zentriert das Raster horizontal */
                align-items: center; /* Zentriert das Raster vertikal */
            }
            .grid-img {
                width: 100%; /* Stellen Sie die Bildbreite ein */
                max-width: 1000px; /* Maximale Bildbreite */
                margin: auto; /* Zentriert das Bild im Grid-Item */
            }
        "))
                                 ),
                                 
                                 # Rastercontainer
                                 div(id = "image-grid",
                                     # Erste Reihe
                                     div(img(src = "elbow1.png", class = "grid-img")),
                                     div(img(src = "elbow2.png", class = "grid-img")),
                                     
                                     # Zweite Reihe
                                     div(img(src = "elbow3.png", class = "grid-img")),
                                     div(img(src = "elbow4.png", class = "grid-img"))
                                 )
                               ),
                               img(src = "elbow.jpeg", style = "width: 60%; height: auto;", class = "d-block w-100"),
                               HTML("<hr>"),
                               HTML("
                                 <div>
                                   <h3>Multiple Korrespondenzanalyse (MCA)</h3>
                                   <p>Die Multiple Korrespondenzanalyse (MCA) ist ein Verfahren der statistischen Analyse, das genutzt wird, um Zusammenhänge in kategorialen Daten aufzudecken und zu veranschaulichen. Sie ist ideal, wenn Sie mit vielen kategorialen Variablen arbeiten und Einblick in deren gegenseitige Beziehungen gewinnen möchten.</p>
                                   <p>MCA bildet die kategorialen Daten in einem Raum ab, in dem jede Kategorie als Punkt repräsentiert wird. Dabei werden ähnliche Kategorien räumlich näher zueinander und unähnliche weiter voneinander entfernt angezeigt. Dies erleichtert es, Verbindungen zwischen den Kategorien rasch zu identifizieren und zu deuten.</p>
                                   <p>Insbesondere kann sie für folgendes genutzt werden:</p>
                                   <ul>
                                   <li><strong>Datenexploration:</strong> Entdeckung von verborgenen Mustern in den Daten als Ausgangspunkt für tiefere Analysen.</li>
                                   <li><strong>Dimensionalitätsreduktion:</strong> Komprimierung der Anzahl an Variablen auf eine überschaubare Anzahl von Dimensionen, die wesentliche Informationen enthalten.</li>
                                   <li><strong>Visualisierung:</strong> Illustration von Beziehungen zwischen Kategorien auf eine Weise, die schnell und intuitiv erfassbar ist.</li>
                                   </ul>
                                   </div>
                                 
                               ")
                               
   
                             ),
                             
                             tabPanel(
                               "Hierarch. Clusteranalyse",
                               br(),
                               HTML("
                              <h3>Hierarchische Clusteranalyse mittels Dendogram</h3>"),
                               plotlyOutput("dendrogramPlot"),
                               br(),
                               HTML("
                              <h3>Erzeugter 'elbow'-Plot</h3>"),
                               plotOutput("dendrogramPlot2")
                             ),

                             tabPanel(
                               "Cluster-Profile",
                               br(),
                               HTML("
                              <h3>Cluster Profile</h3>"),
                               tableOutput("clusterSummary"),
                               plotOutput("boxplot")
                             ),
                             
                             tabPanel(
                               "Multidimensionale Skalierung (MDS)",
                               br(),
                               HTML("
                              <h3>MDS Plots</h3>"),
                               withSpinner(plotlyOutput("mdsPlot"), type =3),
                               uiOutput("silhouetteScore"),
                               uiOutput("adjustedRandIndex"),
                               br(),
                               withSpinner(plotlyOutput("mdsPlotCoa"), type=3),
                               br(),
                               withSpinner(plotlyOutput("mdsPlotParty"), type =3),
                               tableOutput("clusterMetrics")
                              # br(),
                               #plotlyOutput("metricsPlot")
                             ),
                             
                             
                           )
                  )

      )
    )
  ),
  uiOutput("fusszeile")
)


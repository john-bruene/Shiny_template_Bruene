analysisTabContent <- tabPanel("Analyse", value = "analysis",
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
                              <h3>Hierarchische Clusteranalyse mittels Dendrogram</h3>"),
                                                     withSpinner(plotlyOutput("dendrogramPlot_sample", height = "600px"), type = 3),
                                                     br(),
                                                     withSpinner(plotlyOutput("dendrogramPlot_full", height = "800px"), type = 3),
                                                     br(),
                                                     withSpinner(plotlyOutput("clusterChart"), type = 3),
                                                     br(),
                                                     HTML("
                              <h3>Erzeugter 'elbow'-Plot</h3>"),
                                                     withSpinner(plotOutput("dendrogramPlot2"), type = 3),
                                                   ),
                                                   
                                                   tabPanel(
                                                     "Cluster-Profile",
                                                     br(),
                                                     HTML("
                              <h3>Cluster Profile</h3>"),
                                                     tableOutput("clusterSummary"),
                                                     plotOutput("boxplot"),
                                                     plotOutput("spider")
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
                                                     dataTableOutput("selectedInfo"),
                                                     plotOutput("pcaPlot"), 
                                                     br(),
                                                     withSpinner(plotlyOutput("pcaHeatmap"), type =3),
                                                     br(),
                                                     withSpinner(plotlyOutput("loadingsBarplot"), type =3),
                                                     br(),
                                                     withSpinner(plotlyOutput("pcaBiplot"), type =3),
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

# Shiny-specific packages
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(shinydisconnect)
library(shinyjs)

# Data manipulation and visualization
library(tidyverse)
library(patchwork)
library(gridExtra)
library(RColorBrewer)

# Spatial data handling and mapping
library(sf)
library(leaflet)

# Clustering, dimension reduction, and statistical analysis
library(FactoMineR)
library(factoextra)
library(cluster)
library(mclust)
library(dendextend)
library(ggdendro)
library(e1071)

# Machine learning and model evaluation
library(caret)

# visualization and plotting
library(wordcloud2)
library(ggparliament)
library(slickR)

# Political analysis
library(wnominate)

# wordcloud
library(labourR)

# radar chart
library(fmsb)

options(spinner.color="#153268", spinner.color.background="#ffffff", spinner.size=2)





    # Benutzerdefinierte Farbpalette für Parteien
get_color <- function(party) {
  party_colors <- c("CDU" = "#1a1a1a", 
                    "SPD" = "#cd1b01", 
                    "FDP" = "yellow", 
                    "Bündnis 90/Die Grünen" = "#2bd800",
                    "BÜNDNIS 90/DIE GRÜNEN" = "#2bd800",
                    "DIE GRÜNEN" = "#2bd800",
                    "Die Linke." = "#b52269", 
                    "DIE LINKE" = "#b52269",
                    "BSW" = "#650164",
                    "AfD" = "#01d1ff", 
                    "CSU" = "darkblue",
                    "FREIE WÄHLER" = "orange",
                    "Die PARTEI" = "pink",
                    "PIRATEN" = "lightblue",
                    "NPD" = "brown",
                    "Tierschutzpartei" = "darkgreen",
                    "Familien-Partei" = "lightgreen",
                    "ÖDP" = "gold",
                    "Volt" = "purple",
                    "SSW" = "orange",
                    "Fraktionslos" = "grey",
                    "fraktionslos" = "grey",
                    "parteilos" = "grey",
                    "CDU/CSU" = "black",
                    "Union (CDU/CSU)" = "black")
  
  return(party_colors[party])
}



get_coa_color <- function(party, year){
  if (year == 1) {
    party_coa_colors <- c("CDU" = "orange", 
                          "SPD" = "darkblue", 
                          "FDP" = "darkblue", 
                          "Bündnis 90/Die Grünen" = "darkblue", 
                          "DIE LINKE" = "orange",
                          "BSW" = "orange",
                          "AfD" = "orange", 
                          "CSU" = "orange",
                          "FREIE WÄHLER" = "orange",
                          "Die PARTEI" = "orange",
                          "PIRATEN" = "orange",
                          "NPD" = "orange",
                          "Tierschutzpartei" = "orange",
                          "Familien-Partei" = "orange",
                          "ÖDP" = "orange",
                          "Volt" = "orange",
                          "SSW" = "orange",
                          "Fraktionslos" = "orange",
                          "parteilos" = "orange",
                          "Union (CDU/CSU)" = "orange")  # Erste Farbpalette
  } else if (year == 2) {
    party_coa_colors <- c("CDU" = "darkblue", 
                          "SPD" = "darkblue", 
                          "FDP" = "orange", 
                          "Bündnis 90/Die Grünen" = "orange", 
                          "DIE LINKE" = "orange",
                          "BSW" = "orange",
                          "AfD" = "orange", 
                          "CSU" = "darkblue",
                          "FREIE WÄHLER" = "orange",
                          "Die PARTEI" = "orange",
                          "PIRATEN" = "orange",
                          "NPD" = "orange",
                          "Tierschutzpartei" = "orange",
                          "Familien-Partei" = "orange",
                          "ÖDP" = "orange",
                          "Volt" = "orange",
                          "SSW" = "orange",
                          "Fraktionslos" = "orange",
                          "parteilos" = "orange",
                          "Union (CDU/CSU)" = "darkblue")  # Zweite Farbpalette
  } else if (year == 3) {
    party_coa_colors <- c("CDU" = "darkblue", 
                          "SPD" = "darkblue", 
                          "FDP" = "orange", 
                          "Bündnis 90/Die Grünen" = "orange", 
                          "DIE LINKE" = "orange",
                          "BSW" = "orange",
                          "AfD" = "orange", 
                          "CSU" = "darkblue",
                          "FREIE WÄHLER" = "orange",
                          "Die PARTEI" = "orange",
                          "PIRATEN" = "orange",
                          "NPD" = "orange",
                          "Tierschutzpartei" = "orange",
                          "Familien-Partei" = "orange",
                          "ÖDP" = "orange",
                          "Volt" = "orange",
                          "SSW" = "orange",
                          "Fraktionslos" = "orange",
                          "parteilos" = "orange",
                          "Union (CDU/CSU)" = "darkblue")  # Dritte Farbpalette
  } else {
    party_coa_colors <- c("CDU" = "darkblue", 
                          "SPD" = "orange", 
                          "FDP" = "darkblue", 
                          "Bündnis 90/Die Grünen" = "orange", 
                          "DIE LINKE" = "orange",
                          "BSW" = "orange",
                          "AfD" = "orange", 
                          "CSU" = "darkblue",
                          "FREIE WÄHLER" = "orange",
                          "Die PARTEI" = "orange",
                          "PIRATEN" = "orange",
                          "NPD" = "orange",
                          "Tierschutzpartei" = "orange",
                          "Familien-Partei" = "orange",
                          "ÖDP" = "orange",
                          "Volt" = "orange",
                          "SSW" = "orange",
                          "Fraktionslos" = "orange",
                          "parteilos" = "orange",
                          "Union (CDU/CSU)" = "darkblue")  # Vierte Farbpalette
  }
  
  return(party_coa_colors[party])
}






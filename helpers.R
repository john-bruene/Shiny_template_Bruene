# call all the necessary packages
library(shiny)
library(DT)
library(plotly)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(shinydisconnect)
library(patchwork)
library(httr)
library(jsonlite)
library(tidyverse)
library(sf)
library(ggspatial)
library(maps)
library(gridExtra)
library(leaflet)
library(wordcloud2)
library(labourR)
library(FactoMineR)
library(factoextra)
library(cluster)
library(caret)
library(slickR)
library(dendextend)
library(ggtree)
library(ggdendro)
library(mclust)

options(spinner.color="#153268", spinner.color.background="#ffffff", spinner.size=2)





    # Benutzerdefinierte Farbpalette für Parteien
get_color <- function(party) {
  party_colors <- c("CDU" = "black", 
                    "SPD" = "red", 
                    "FDP" = "yellow", 
                    "Bündnis 90/Die Grünen" = "green", 
                    "DIE LINKE" = "purple",
                    "BSW" = "purple",
                    "AfD" = "blue", 
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
                    "parteilos" = "grey",
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






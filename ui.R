# Call the helpers and text files
source("helpers.R")
source("analysisTabContent.R") 
source("votingInfoTabContent.R")
source("legislatureInfoTabContent.R")
source("sidebarContent.R")
load("introText.RData")
load("legislatureInfoText.RData")
load("votingInfoText.RData")
load("analysisInfoText.RData")
load("politicalSystemText.RData")

# Define UI for random distribution app ----
ui <- fluidPage(
  includeCSS("style.css"),
  theme = bs_theme(version = 4),
  useShinyjs(),
  
  # App title ----
  titlePanel("ParliamentWatch"),
  
  # Sidebar layout defined ----
  sidebarLayout(
    
    sidebarContent,  # Verwende die ausgelagerte Sidebar
    
    # Main panel for displaying outputs ----
    mainPanel(
      # tags$head(tags$script(src="template.js")),
      
      # Output: tabsetPanel only needed for several tabs
      tabsetPanel(type = "tabs", id = "tabs",
#                  tabPanel("Willkommen", value = 1, br(), HTML(introText)),
            
                  legislatureInfoTabContent, # Verwendung des 2. ausgelagerten Inhalts

                  votingInfoTabContent, # Verwendung des 3. ausgelagerten Inhalts

                  analysisTabContent  # Verwendung des 4. ausgelagerten Inhalts

      )
    )
  ),
  uiOutput("fusszeile")
)


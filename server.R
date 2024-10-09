# Call the helpers file
source("helpers.R")


# Define server logic for app ----
server <- function(input, output, session) {
  
  fusszeile <- div(class = "containerlow",
                   tags$text(paste0("University of Goettingen, Faculty of Business and 
                                    Economics, Chair of Spatial Data Science and Statistical Learning")),
                   #tags$br(),
                   tags$text(paste0("Project: Abgeordnetenwatch - Clustering of voting behaviour by John F. Brüne"))
  )
  
  output$fusszeile <- renderUI({
    fusszeile
  })
  
  
  # adjust the template from here on
  # loading the data
  politician_data2125 <- read.csv("data/all_politicians_bt_21_25_new.csv")
  politician_data1721 <- read.csv("data/all_politicians_bt_17_21_new.csv")
  politician_data1317 <- read.csv("data/all_politicians_bt_13_17_new.csv")
  politician_data0913 <- read.csv("data/all_politicians_bt_09_13_new.csv")
  votes_2125_df<- read_csv("data/all_votes_data_2125_df_new.csv", col_types = cols(vote = col_factor(levels = c("yes", "no", "no_show", "abstain")), mandate.id = col_integer(), reason_no_show = col_character(),  reason_no_show_other = col_character(), poll.id = col_integer(), fraction.id = col_integer()))
  votes_1721_df<- read_csv("data/all_votes_data_1721_df_new.csv", col_types = cols(vote = col_factor(levels = c("yes", "no", "no_show", "abstain")), mandate.id = col_integer(), reason_no_show = col_character(),  reason_no_show_other = col_character(), poll.id = col_integer(), fraction.id = col_integer()))
  votes_1317_df<- read_csv("data/all_votes_data_1317_df_new.csv", col_types = cols(vote = col_factor(levels = c("yes", "no", "no_show", "abstain")), mandate.id = col_integer(), reason_no_show = col_character(),  reason_no_show_other = col_character(), poll.id = col_integer(), fraction.id = col_integer()))
  votes_0913_df<- read_csv("data/all_votes_data_0913_df_new.csv", col_types = cols(vote = col_factor(levels = c("yes", "no", "no_show", "abstain")), mandate.id = col_integer(), reason_no_show = col_character(),  reason_no_show_other = col_character(), poll.id = col_integer(), fraction.id = col_integer()))
  polls_2125_df<- read.csv("data/all__poll_2125_df_new.csv")
  polls_1721_df<- read.csv("data/all__poll_1721_df_new.csv")
  polls_1317_df<- read.csv("data/all__poll_1317_df_new.csv")
  polls_0913_df<- read.csv("data/all__poll_0913_df_new.csv")
  
  votes_2125_df <- left_join(votes_2125_df, polls_2125_df, by = "poll.id")
  votes_1721_df <- left_join(votes_1721_df, polls_1721_df, by = "poll.id")
  votes_1317_df <- left_join(votes_1317_df, polls_1317_df, by = "poll.id")
  votes_0913_df <- left_join(votes_0913_df, polls_0913_df, by = "poll.id")
  
  wahlkreise21 <- st_read("wahlkreise/btw21_geometrie_wahlkreise_shp/Geometrie_Wahlkreise_20DBT.shp")
  wahlkreise17 <- st_read("wahlkreise/btw17_geometrie_wahlkreise_geo_shp/Geometrie_Wahlkreise_19DBT_geo.shp")
  wahlkreise13 <- st_read("wahlkreise/btw13_geometrie_wahlkreise_etrs89_geo_shp/Geometrie_Wahlkreise_18DBT.shp")
  wahlkreise09 <- st_read("wahlkreise/btw09_geometrie_wahlkreise_shp/Geometrie_Wahlkreise_17DBT.shp")
  
  current_year <- format(Sys.Date(), "%Y")
  
  # change for politician_data1721, politician_data1317 and politician_data0913 all $party.label from "BSW" to "DIE LINKE"
  
  politician_data1721$party.label[which(politician_data1721$party.label == "BSW")] <- "DIE LINKE"
  politician_data1317$party.label[which(politician_data1317$party.label == "BSW")] <- "DIE LINKE"
  politician_data0913$party.label[which(politician_data0913$party.label == "BSW")] <- "DIE LINKE"

  

  
  #reactive part
  dat_react <- reactive({
    poll.id <- as.numeric(input$VoteID)
    
    if (input$data == 1) {
      politician_data <- politician_data2125
      votes_df <- votes_2125_df
      wahlkreise <- wahlkreise21
    } else if (input$data == 2) {
      politician_data <- politician_data1721
      votes_df <- votes_1721_df
      wahlkreise <- wahlkreise17
    } else if (input$data == 3) {
      politician_data <- politician_data1317
      votes_df <- votes_1317_df
      wahlkreise <- wahlkreise13
    } else if (input$data == 4) {
      politician_data <- politician_data0913
      votes_df <- votes_0913_df
      wahlkreise <- wahlkreise09
    }
    
    
    # filter for topics
    economic_votes <- votes_df %>%
      filter(Label1 == "Öffentliche Finanzen, Steuern und Abgaben")
    
    social_votes <- votes_df %>%
      filter(Label1 == "Gesellschaftspolitik, soziale Gruppen")
    
    foreign_policy_votes <- votes_df %>%
      filter(Label1 == "Außenpolitik und internationale Beziehungen")
    
    energy_votes <- votes_df %>%
      filter(Label1 == "Energie")    
    
    defense_votes <- votes_df %>%
      filter(Label1 == "Verteidigung")
    
    health_votes <- votes_df %>%
      filter(Label1 == "Gesundheit")
    
    migration_votes <- votes_df %>%
      filter(Label1 == "Migration und Aufenthaltsrecht")
    
    employment_votes <- votes_df %>%
      filter(Label1 == "Arbeit und Beschäftigung")
    
    environment_votes <- votes_df %>%
      filter(Label1 == "Umwelt")
    
    law_votes <- votes_df %>%
      filter(Label1 == "Recht")
    
    
    # Scores for political positions
    economic_positions <- economic_votes %>%
      mutate(economic_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  # abstain or no_show
      )) %>%
      group_by(mandate.id) %>%
      summarize(economic_position = mean(economic_score, na.rm = TRUE)) %>%
      ungroup()
    
    social_positions <- social_votes %>%
      mutate(social_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(social_position = mean(social_score, na.rm = TRUE)) %>%
      ungroup()
    
    foreign_policy_positions <- foreign_policy_votes %>%
      mutate(foreign_policy_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(foreign_policy_position = mean(foreign_policy_score, na.rm = TRUE)) %>%
      ungroup()
    
    defense_positions <- defense_votes %>%
      mutate(defense_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(defense_position = mean(defense_score, na.rm = TRUE)) %>%
      ungroup()
    
    health_positions <- health_votes %>%
      mutate(health_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(health_position = mean(health_score, na.rm = TRUE)) %>%
      ungroup()
    
    energy_positions <- energy_votes %>%
      mutate(energy_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(energy_position = mean(energy_score, na.rm = TRUE)) %>%
      ungroup()
    
    migration_positions <- migration_votes %>%
      mutate(migration_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(migration_position = mean(migration_score, na.rm = TRUE)) %>%
      ungroup()
    
    employment_positions <- employment_votes %>%
      mutate(employment_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(employment_position = mean(employment_score, na.rm = TRUE)) %>%
      ungroup()
    
    environment_positions <- environment_votes %>%
      mutate(environment_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(environment_position = mean(environment_score, na.rm = TRUE)) %>%
      ungroup()
    
    law_positions <- law_votes %>%
      mutate(law_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(law_position = mean(law_score, na.rm = TRUE)) %>%
      ungroup()
    
    
    

    
    
    
    # combine in one dataframe
    
    political_positions <- full_join(economic_positions, social_positions, by = "mandate.id") %>%
      full_join(foreign_policy_positions, by = "mandate.id") %>%
      full_join(defense_positions, by = "mandate.id") %>%
      full_join(energy_positions, by = "mandate.id") %>%
      full_join(health_positions, by = "mandate.id") %>%
      full_join(migration_positions, by = "mandate.id") %>%
      full_join(employment_positions, by = "mandate.id") %>%
      full_join(environment_positions, by = "mandate.id") %>%
      full_join(law_positions, by = "mandate.id")
    
    votes_df <- left_join(votes_df, political_positions, by = "mandate.id")

    
    ######################
    #Add the fraction.vote
    votes_df_modified <- votes_df %>%
      group_by(poll.id, fraction.id, vote) %>%
      tally(name = "n") %>%
      arrange(poll.id, fraction.id, desc(n)) %>%
      group_by(poll.id, fraction.id) %>%
      summarize(
        fraction.vote = first(vote)
      ) %>%
      ungroup()
    
    
    # Merge the result back to the original dataset
    votes_df <- left_join(votes_df, votes_df_modified, by = c("poll.id", "fraction.id"))
    
    #Adding loyalty_percentage
    votes_df_modified1 <- votes_df %>%
      mutate(party.loyalty = ifelse(vote == fraction.vote, "loyal", "dissenter")) %>%
      group_by(mandate.id) %>%
      summarize(loyal_count = sum(party.loyalty == "loyal", na.rm = TRUE),
                total_votes = n()) %>%
      ungroup() %>%
      mutate(loyalty_percentage = loyal_count / total_votes * 100)
    
    votes_df <- left_join(votes_df, votes_df_modified1, by = "mandate.id")
    
    
    ########################
    

    specific_vote <- input$selected_variable
    filtered_votes <- votes_df[votes_df$poll.label == specific_vote, ]
    mandate_ids <- filtered_votes$mandate.id
    matching_politicians <- politician_data[politician_data$id.mandat %in% mandate_ids, ]
    matching_politicians$vote <- filtered_votes$vote[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$fraction.vote <- filtered_votes$fraction.vote[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$loyalty_percentage <- round(filtered_votes$loyalty_percentage[match(matching_politicians$id.mandat, filtered_votes$mandate.id)], 2)
    
    matching_politicians$result <- filtered_votes$Result[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$committee1 <- filtered_votes$Ausschuss1[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$committee2 <- filtered_votes$Ausschuss2[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic1 <- filtered_votes$Label1[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic2 <- filtered_votes$Label2[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic3 <- filtered_votes$Label3[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]

    matching_politicians$economic_position <- round(filtered_votes$economic_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$social_position <- round(filtered_votes$social_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    
    matching_politicians$foreign_policy_score <- round(filtered_votes$foreign_policy_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$defense_score <- round(filtered_votes$defense_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$energy_score <- round(filtered_votes$energy_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$health_score <- round(filtered_votes$health_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$migration_score <- round(filtered_votes$migration_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$employment_score <- round(filtered_votes$employment_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$environment_score <- round(filtered_votes$environment_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$law_score <- round(filtered_votes$law_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    
  
    matching_politicians$answered_percentage <- round(ifelse(matching_politicians$statistic_questions == 0 | is.na(matching_politicians$statistic_questions_answered), 0, matching_politicians$statistic_questions_answered / matching_politicians$statistic_questions * 100), 2)
    matching_politicians <- matching_politicians %>%
      mutate(age = as.numeric(current_year) - year_of_birth)
    
    
    max_age <- input$age_slider[2]
    min_age <- input$age_slider[1]
    
      matching_politicians <- matching_politicians %>%
      filter(age >= min_age, age <= max_age)
      

    
    # Union Checkbox
    if(input$checkboxUnion) {
      matching_politicians <- matching_politicians %>%
        mutate(party.label = ifelse(party.label %in% c("CDU", "CSU"), "Union (CDU/CSU)", party.label))
    }
    

    return(matching_politicians)
  })
  
  #reactive part
  wahlkreise_react <- reactive({

    
    if (input$data == 1) {

      wahlkreise <- wahlkreise21
    } else if (input$data == 2) {

      wahlkreise <- wahlkreise17
    } else if (input$data == 3) {

      wahlkreise <- wahlkreise13
    } else if (input$data == 4) {

      wahlkreise <- wahlkreise09
    }
    
    return(wahlkreise)
    
  })
  

  
  #reactive part for polls
  poll_react <- reactive({
    poll.id <- as.numeric(input$VoteID)
    
    if (input$data == 1) {
      votes_df <- votes_2125_df
    } else if (input$data == 2) {
      votes_df <- votes_1721_df
    } else if (input$data == 3) {
      votes_df <- votes_1317_df
    } else if (input$data == 4) {
      votes_df <- votes_0913_df
    } 
    return(votes_df)
  })
  
  desired_order_reactive <- reactive({
    if (input$data == "1") {
      if(input$checkboxUnion) {
        return(c("SPD", "Union (CDU/CSU)", "Bündnis 90/Die Grünen", "FDP", "AfD", "DIE LINKE", "BSW" ,"SSW", "Fraktionslos", "parteilos"))
      } else {
        return(c("SPD", "CDU", "Bündnis 90/Die Grünen", "FDP", "AfD", "CSU", "DIE LINKE", "BSW", "SSW", "Fraktionslos", "parteilos"))
      }
    } else if (input$data == "2") {
      if(input$checkboxUnion) {
      c("Union (CDU/CSU)", "SPD", "FDP", "AfD", "DIE LINKE", "Bündnis 90/Die Grünen", "Fraktionslos") 
      } else {
        return(c("CDU", "SPD", "FDP", "AfD","DIE LINKE", "Bündnis 90/Die Grünen", "CSU", "Fraktionslos"))
        } 
    } else if (input$data == "3") {
      if(input$checkboxUnion) {
      c("Union (CDU/CSU)", "SPD", "Bündnis 90/Die Grünen","DIE LINKE", "Fraktionslos") 
      } else {
        return(c("CDU", "SPD", "Bündnis 90/Die Grünen", "DIE LINKE", "CSU","Fraktionslos"))
      } 
    } else if (input$data == "4") {
      if(input$checkboxUnion) {
      c("Union (CDU/CSU)", "SPD", "FDP", "Bündnis 90/Die Grünen", "DIE LINKE", "Fraktionslos") 
      } else {
        return(c("CDU", "SPD", "FDP", "Bündnis 90/Die Grünen", "DIE LINKE", "CSU", "Fraktionslos"))
      } 
        } else {
      c() #empty
    }
  })
  

 
  ################
  
  observe({
    selected_term <- input$data
    
    # Choose dataset 
    selected_dataset <- switch(selected_term,
                               "1" = polls_2125_df,
                               "2" = polls_1721_df,
                               "3" = polls_1317_df,
                               "4" = polls_0913_df)
    
    # Getting column names of the selected dataset
    variable_choices <- unique(selected_dataset$Label1)
    
    # Update the choices of the second selectInput dynamically
    updateSelectInput(session, "selected_topic_x", choices = variable_choices)
  })
  
  observe({
    selected_term <- input$data
    
    # Choosing the appropriate dataset based on the selected term
    selected_dataset <- switch(selected_term,
                               "1" = polls_2125_df,
                               "2" = polls_1721_df,
                               "3" = polls_1317_df,
                               "4" = polls_0913_df)
    
    # Get the column names of the selected dataset
    variable_choices_y <- unique(selected_dataset$Label1)
    
    # Update the choices of the second selectInput dynamically
    updateSelectInput(session, "selected_topic_y", choices = variable_choices_y)
  })
  
  


  #############################
  
  # Reactive expression to return the filtered dataset based on the search
  reactive_dataset <- reactive({
    selected_term <- input$data
    

    selected_dataset <- switch(selected_term,
                               "1" = votes_2125_df,
                               "2" = votes_1721_df,
                               "3" = votes_1317_df,
                               "4" = votes_0913_df)
    
    # Filter based on search input if there is any input, otherwise return all
    if (input$search_input != "") {
      return(selected_dataset[grep(input$search_input, selected_dataset$poll.label, ignore.case = TRUE), ])
    } else {
      return(selected_dataset)
    }
  })
  
  # Observe the reactive dataset and update the choices of the selectInput dynamically
  observe({
    variable_choices <- unique(reactive_dataset()$poll.label)
    updateSelectInput(session, "selected_variable", choices = variable_choices)
  })
  
  
###############
  output$welcometext <- renderText({
   "Legislaturperiode auswählen" 
  })
  
  ###
  
  
  output$ergebnisse_tabelle <- DT::renderDataTable({
    dat_r <- poll_react()
    
    anzahl_abstimmungen <- dat_r %>%
      group_by(poll.id) %>%
      summarise(anzahl_abstimmungen = n()) %>%
      ungroup()
    
    dat_r <- left_join(dat_r, anzahl_abstimmungen, by = "poll.id")
    
    polls_per_topic <- dat_r %>%
      group_by(Label1) %>%
      summarise(total_polls = n_distinct(poll.id)) %>%
      ungroup()
    
    polls_per_topic2 <- dat_r %>%
      group_by(Label2) %>%
      summarise(total_polls = n_distinct(poll.id)) %>%
      ungroup()
    
    colnames(polls_per_topic2)[colnames(polls_per_topic2) == "Label2"] <- "Label1"
    
    polls_per_topic3 <- dat_r %>%
      group_by(Label3) %>%
      summarise(total_polls = n_distinct(poll.id)) %>%
      ungroup()
    
    colnames(polls_per_topic3)[colnames(polls_per_topic3) == "Label3"] <- "Label1"
    
    # Quote of "no_show"s 
    quote_no_show <- dat_r %>%
      group_by(poll.id) %>%
      summarise(fehlend = mean(vote == "no_show", na.rm = TRUE)) %>%
      ungroup()
    
    dat_r <- left_join(dat_r, quote_no_show, by = "poll.id")
    
    # "fehlend"-Quote for topic1
    ergebnisse_nach_topic <- dat_r %>%
      filter(!is.na(Label1)) %>%
      group_by(Label1) %>%
      summarise(durchschnitt_fehlend = mean(fehlend, na.rm = TRUE))
    
    ergebnisse_nach_topic <- left_join(ergebnisse_nach_topic, polls_per_topic, by = "Label1")
    ergebnisse_nach_topic <- left_join(ergebnisse_nach_topic, polls_per_topic2, by = "Label1")
    ergebnisse_nach_topic <- left_join(ergebnisse_nach_topic, polls_per_topic3, by = "Label1")
    
    ergebnisse_nach_topic$total_polls <- rowSums(ergebnisse_nach_topic[, c("total_polls.x", "total_polls.y", "total_polls")], na.rm = TRUE)
    
    # deleting unused columns
    ergebnisse_nach_topic <- ergebnisse_nach_topic %>% select(-total_polls.x, -total_polls.y)
    ergebnisse_nach_topic$total_polls <- as.integer(round(ergebnisse_nach_topic$total_polls))
    
    ergebnisse_nach_topic$durchschnitt_fehlend <- round(ergebnisse_nach_topic$durchschnitt_fehlend * 100, 2)
    
    ergebnisse_nach_topic <- ergebnisse_nach_topic %>%
      rename(
        Gesetzgebungsbereich = Label1,
        `Fehlende Abgeordnete (%)` = durchschnitt_fehlend,
        `Anzahl Gesetze (n)` = total_polls
      ) %>%
      arrange(desc(`Anzahl Gesetze (n)`))
    
    
    # rendering table with options
    DT::datatable(ergebnisse_nach_topic, options = list(paging = FALSE, pageLength = nrow(ergebnisse_nach_topic)))
  })
  
  observeEvent(input$toggleTable, {
    shinyjs::toggle(id = "tableContainer")
  })
  
  
  ##############
  
  # First Plotly-Diagramm
  
  output$histogram7 <-  renderPlotly({
    
    dat_r <- poll_react()
    
    anzahl_abstimmungen <- dat_r %>%
      group_by(poll.id) %>%
      summarise(anzahl_abstimmungen = n()) %>%
      ungroup()
    
    dat_r <- left_join(dat_r, anzahl_abstimmungen, by = "poll.id")
    
    polls_per_topic <- dat_r %>%
      group_by(Label1) %>%
      summarise(total_polls = n_distinct(poll.id))%>%
      ungroup()
    
    polls_per_topic2 <- dat_r %>%
      group_by(Label2) %>%
      summarise(total_polls = n_distinct(poll.id))%>%
      ungroup()
    
    colnames(polls_per_topic2)[colnames(polls_per_topic2) == "Label2"] <- "Label1"
    
    polls_per_topic3 <- dat_r %>%
      group_by(Label3) %>%
      summarise(total_polls = n_distinct(poll.id))%>%
      ungroup()
    
    colnames(polls_per_topic3)[colnames(polls_per_topic3) == "Label3"] <- "Label1"
    
    
    # "no_show"s per vote
    quote_no_show <- dat_r %>%
      group_by(poll.id) %>%
      summarise(fehlend = mean(vote == "no_show", na.rm = TRUE)) %>%
      ungroup()
    
    dat_r <- left_join(dat_r, quote_no_show, by = "poll.id")
    
    #"fehlend"-Quote for topic1
    ergebnisse_nach_topic <- dat_r %>%
      filter(!is.na(Label1)) %>%
      group_by(Label1) %>%
      summarise(durchschnitt_fehlend = mean(fehlend, na.rm = TRUE))
    
    ergebnisse_nach_topic <- left_join(ergebnisse_nach_topic, polls_per_topic, by = "Label1")
    ergebnisse_nach_topic <- left_join(ergebnisse_nach_topic, polls_per_topic2, by = "Label1")
    ergebnisse_nach_topic <- left_join(ergebnisse_nach_topic, polls_per_topic3, by = "Label1")
    
    ergebnisse_nach_topic$total_polls <- rowSums(ergebnisse_nach_topic[, c("total_polls.x", "total_polls.y", "total_polls")], na.rm = TRUE)
    
    # deleting unused columns
    ergebnisse_nach_topic <- ergebnisse_nach_topic %>% select(-total_polls.x, -total_polls.y)
    ergebnisse_nach_topic$total_polls <- as.integer(round(ergebnisse_nach_topic$total_polls))
    
    ergebnisse_nach_topic$durchschnitt_fehlend <- ergebnisse_nach_topic$durchschnitt_fehlend*100
    
    # Plotly-Diagramm
    fig <- plot_ly(ergebnisse_nach_topic, x = ~Label1, y = ~durchschnitt_fehlend, type = 'scatter', mode = 'lines', name = 'Durchschnitt Fehlend') %>%
      add_trace(y = ~total_polls, type = 'bar', name = 'Anzahl der Polls') %>%
      layout(
        xaxis = list(title = "Gesetzgebungsbereich"),
        yaxis = list(title = ""),
        legend = list(title = list(text = '<b> Legende </b>'))
      )
    
    fig
  
  })
  
##########################

  
  output$TitelGesetz <- renderUI({
    text <- input$selected_variable
    
    # Wrapping the text in a <div> tag with styles for text-align and font-size
    formatted_text <- paste0('<div style="text-align: center; font-size: 20px;">', text, '</div>')
    
    # HTML() to interpret the string as HTML content
    return(HTML(formatted_text))
  })
  
  
  ####
  

  
  output$histogram9 <- renderPlotly({
    dat_r <- dat_react()
    vote_party_counts <- dat_r %>%
      count(party.label, vote) %>%
      complete(party.label, vote, fill = list(n = 0)) %>%
      group_by(party.label) %>%
      summarise(total_votes = sum(n)) %>%
      arrange(desc(total_votes)) %>%
      ungroup() %>%
      left_join(dat_r %>% count(party.label, vote), by = "party.label")
    
    # Define the color palette
    my_colors <- c("yes" = "#a4d67a", "no" = "#c76f5e", "abstain" = "#e2e2e2", "no_show" = "#a6a6a6")
    
    # Get unique party labels sorted by total votes
    sorted_parties <- vote_party_counts %>%
      group_by(party.label) %>%
      summarise(total_votes = sum(n)) %>%
      arrange(desc(total_votes)) %>%
      pull(party.label)
    
    # Initialize an empty plotly object
    fig <- plot_ly()
    
    # Number of rows and columns for the grid
    n_rows <- ceiling(sqrt(length(sorted_parties)))
    n_cols <- ceiling(length(sorted_parties) / n_rows)
    
    # Annotations for party titles
    annotations <- list()
    
    # Margin or space between plots
    margin <- 0.02 
    
    # Adding pie charts
    for (i in seq_along(sorted_parties)) {
      party <- sorted_parties[i]
      party_data <- vote_party_counts[vote_party_counts$party.label == party,]
      
      domain_x <- c((i-1) %% n_cols / n_cols + margin, (i-1) %% n_cols / n_cols + (1/n_cols) - margin)
      domain_y <- c(1 - ceiling(i/n_cols) / n_rows + margin, 1 - (ceiling(i/n_cols) - 1) / n_rows - margin)
      
      fig <- fig %>% add_pie(
        data = party_data,
        labels = ~vote,
        values = ~n,
        name = party,
        domain = list(x = domain_x, y = domain_y),
        marker = list(colors = my_colors[party_data$vote]),
        textinfo = 'label+percent',
        insidetextorientation = 'radial'
      )
      
      # Annotations positions
      xpos <- mean(domain_x)
      ypos <- domain_y[2] + margin
      
      # Add title annotation for each pie chart
      annotations <- c(annotations, list(
        x = xpos,
        y = ypos,
        text = party,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 10)
      ))
    }
    
    # ayout for the grid and annotations
    fig <- fig %>% layout(
      title = "Vote Distribution by Party",
      showlegend = FALSE,
      annotations = annotations,
      # Hide axis lines and labels
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
  })
    
  #########################
  
  output$histogram11 <- renderPlotly({
    dat_r <- dat_react()
    
    vote_party_counts <- dat_r %>%
      count(party.label, vote) %>%
      complete(party.label, vote, fill = list(n = 0)) %>%
      arrange(party.label, vote)
    
    my_colors <- c("yes" = "#a4d67a", "no" = "#c76f5e", "abstain" = "#e2e2e2", "no_show" = "#a6a6a6")
    
    sorted_parties <- unique(vote_party_counts$party.label)
    
    fig <- plot_ly()
    
    n_rows <- ceiling(sqrt(length(sorted_parties)))
    n_cols <- ceiling(length(sorted_parties) / n_rows)
    
    annotations <- list()
    
    margin <- 0.02
    
    # bar plots for each party
    for (i in seq_along(sorted_parties)) {
      party <- sorted_parties[i]
      party_data <- vote_party_counts[vote_party_counts$party.label == party,]
      
      domain_x <- c((i-1) %% n_cols / n_cols + margin, (i-1) %% n_cols / n_cols + (1/n_cols) - margin)
      domain_y <- c(1 - ceiling(i/n_cols) / n_rows + margin, 1 - (ceiling(i/n_cols) - 1) / n_rows - margin)
      
      fig <- fig %>% add_bars(
        data = party_data,
        x = ~vote,
        y = ~n,
        name = party,
        marker = list(color = my_colors[party_data$vote]),
        domain = list(x = domain_x, y = domain_y)
      )
      
      xpos <- mean(domain_x)
      ypos <- domain_y[1] - margin
      
      annotations <- c(annotations, list(
        x = xpos,
        y = ypos,
        text = party,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 10)
      ))
    }
    
    fig <- fig %>% layout(
      title = "Vote Distribution by Party",
      showlegend = FALSE,
      annotations = annotations,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T),
      yaxis = list(showgrid = T, zeroline = T, showticklabels = T)
      
    )
  })
  
  ########################
  
  
  output$stackedBarPlot <- renderPlotly({
    dat_r <- dat_react() 
    
    vote_counts <- dat_r %>%
      group_by(party.label, vote) %>%
      tally() %>%
      spread(vote, n, fill = 0) %>%
      mutate(party.label = factor(party.label, levels = desired_order_reactive())) %>%
      arrange(match(party.label, desired_order_reactive())) %>%
      ungroup()
    
    vote_colors <- c("yes" = "#a4d67a", "no" = "#c76f5e", "abstain" = "#e2e2e2", "no_show" = "#a6a6a6")
    
    # stacked
    fig <- plot_ly(data = vote_counts, x = ~party.label, y = ~`yes`, name = "Dafür gestimmt", type = 'bar', marker = list(color = vote_colors["yes"]))
    
    if(any(vote_counts$no > 0)) {
      fig <- fig %>% add_trace(y = ~`no`, name = "Dagegen gestimmt", marker = list(color = vote_colors["no"]))
    }  
    
    if(any(vote_counts$abstain > 0)) {
      fig <- fig %>% add_trace(y = ~`abstain`, name = "Enthalten", marker = list(color = vote_colors["abstain"]))
    }
    if(any(vote_counts$no_show > 0)) {
      fig <- fig %>% add_trace(y = ~`no_show`, name = "Nicht beteiligt", marker = list(color = vote_colors["no_show"]))
    }
    
    # layout for stacked plot
    fig <- fig %>% layout(
      barmode = 'stack', 
      title = 'Stimmenverteilung nach Partei',
      xaxis = list(title = "Partei"),
      yaxis = list(title = "Anzahl der Stimmen"),
      showlegend = TRUE
    )
    
    fig
  })
  

  
  
  ###################
  
  output$scatterPlot <- renderPlotly({
    dat_r <- dat_react()
    
    # Summarize total votes per party and sorting
    party_sizes <- dat_r %>%
      count(party.label) %>%
      rename(total_votes = n) %>%
      arrange(desc(total_votes))
    
    max_size <- max(party_sizes$total_votes)
    
    fig <- plot_ly()
    
    fig <- fig %>% add_markers(
      data = party_sizes,
      x = ~party.label, 
      y = ~total_votes, 
      size = ~total_votes, 
      marker = list(
        sizemode = 'area',
        sizeref = max_size / (pi * 100 ^ 2), 
        opacity = 0.6,
        color = ~party.label, # Color markers by party
        colorscale = 'Viridis'
      )
    )
    
    fig <- fig %>% layout(
      title = "Total Votes by Party",
      xaxis = list(title = "Party"),
      yaxis = list(title = "Total Votes", showticklabels = FALSE),
      showlegend = FALSE
    )
  })
  
  ###############
  
 
  output$histogram5 <- renderPlotly({
    dat_r <- dat_react()
    
    vote_party_counts_sex <- dat_r %>%
      count(sex, vote) %>%
      complete(sex, vote, fill = list(n = 0)) %>%
      mutate(
        sex = ifelse(sex == "m", "Männer", ifelse(sex == "f", "Frauen", "Unbekannt")),  # Geschlecht umbenennen
        vote = factor(vote, 
                      levels = c("yes", "no", "abstain", "no_show"), 
                      labels = c("Dafür", "Dagegen", "Enthalten", "Nicht beteiligt"))  # Vote umbenennen
      )
    
    my_colors <- c("Dafür" = "#a4d67a", "Dagegen" = "#c76f5e", "Enthalten" = "#e2e2e2", "Nicht beteiligt" = "#a6a6a6")
    
    # Create a Plotly plot
    plot_ly(vote_party_counts_sex, x = ~sex, y = ~n, type = 'bar', color = ~vote, colors = my_colors) %>%
      layout(
        title = "Abstimmungsverhalten nach Geschlecht",
        legend = list(title = "Abstimmung"),
        xaxis = list(title = "Geschlecht"),
        yaxis = list(title = "Anzahl")
      )
  })
  
  
  #########################################
  
  
  output$abstimmungsergebnis <- renderPlotly({
    dat_r <- dat_react()  
    
    vote_counts <- dat_r %>%
      count(vote) %>%
      mutate(Prozent = n / sum(n) * 100,  
             CumCount = cumsum(n))
    
    # Umbenennen und Reihenfolge der Levels umkehren
    vote_counts$vote <- factor(vote_counts$vote, 
                               levels = rev(c("yes", "no", "no_show", "abstain")),
                               labels = rev(c("Dafür", "Dagegen", "Nicht beteiligt", "Enthalten")))
    
    # Sortieren der Daten nach den neuen Levels
    vote_counts <- vote_counts %>% arrange(factor(vote, levels = rev(c("Dafür", "Dagegen", "Enthalten", "Nicht beteiligt"))))
    
    # Stacked bar plot
    p <- ggplot(vote_counts, aes(x = "", y = n, fill = vote)) +
      geom_bar(stat = "identity", width = 0.6) +
      coord_flip() +  # Horizontal
      scale_fill_manual(values = c("Dafür" = "#a4d67a", "Dagegen" = "#c76f5e", "Enthalten" = "#e2e2e2", "Nicht beteiligt" = "#a6a6a6")) +
      geom_text(aes(y = CumCount - 0.5 * n, label = paste(sprintf("%.1f%%", Prozent))), hjust = 0.5, size = 3.5, check_overlap = TRUE) +
      labs(title = 'Abstimmungsverhalten (gesamt)', x = NULL, y = "Anzahl der Stimmen", fill = "Abstimmung") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "left")  # Position der Legende links setzen
    
    # Convert to plotly
    ggplotly(p)
  })
  
  
  
  
    
  # HTML-Text
  # nicht genutzt jetzt gerade
  output$voteOutcome <- renderUI({

    dat_r <- dat_react()
    
    if (any(dat_r$result == TRUE)) {
      HTML("Die Abstimmung über den Gesetzesvorschlag war im Bundestag <strong>erfolgreich</strong>.")
    } else {
      HTML("Die Abstimmung über den Gesetzesvorschlag war im Bundestag <strong>nicht erfolgreich</strong>.")
    }
  })

  
  # Leaflet map
  
  ############################

  output$leaflet_map1 <- renderLeaflet({
    
    wahlkreise <- wahlkreise_react()
    
    # Transformation der Koordinaten in das geografische Koordinatensystem (Längen- und Breitengrad)
    wahlkreise <- st_transform(wahlkreise, crs = 4326)
    
    location_data <- dat_react()
    
    # Erstelle eine neue Spalte "WKN_NR" 
    location_data$WKR_NR <- str_extract(location_data$electoral_data_constituency.label, "\\d{1,3}(?= - )")
    
    location_data$WKR_NR <- as.numeric(location_data$WKR_NR)
    # Merge der Tabellen location_data und wahlkreise über die Spalte "WKR_NR"
    merged_data <- merge(location_data, wahlkreise[, c("WKR_NR","WKR_NAME", "geometry")], by = "WKR_NR", all.x = TRUE)
    
    # Sicherstellen, dass location_data ein sf-Objekt ist
    if (!inherits(merged_data, "sf")) {
      merged_data <- st_as_sf(merged_data, crs = 4326)
    }
    
    location_data <- merged_data
    location_data <- location_data %>% filter(electoral_data_mandate_won == "constituency")
    
    # Überprüfung auf fehlende Wahlkreise
    all_wkr <- 1:299
    missing_wkr <- setdiff(all_wkr, location_data$WKR_NR)
    
    if (length(missing_wkr) > 0) {
      additional_data <- merged_data %>% filter(WKR_NR %in% missing_wkr)
      location_data <- bind_rows(location_data, additional_data)
    }
    
    location_data$name <- location_data$label.x
    
    # Umbenennen der `vote`-Werte
    location_data$vote_de <- case_when(
      location_data$vote == "yes" ~ "Dafür gestimmt",
      location_data$vote == "no" ~ "Dagegen gestimmt",
      location_data$vote == "abstain" ~ "Enthalten",
      location_data$vote == "no_show" ~ "Nicht beteiligt"
    )
    
    # Neue Farbzuordnung basierend auf den deutschen Labels
    myColors_1 <- c("Dafür gestimmt" = "#a4d67a", "Dagegen gestimmt" = "#c76f5e", "Enthalten" = "#e2e2e2", "Nicht beteiligt" = "#7c7b7d")
    pal <- colorFactor(myColors_1, domain = location_data$vote_de)
    
    # Erstellen der Leaflet-Karte
    leaflet() %>%
      addTiles() %>%
      setView(lng = 10, lat = 51.5, zoom = 6) %>%
      addPolygons(data = location_data, 
                  fillColor = ~pal(location_data$vote_de), 
                  fillOpacity = 0.9,
                  color = "black",  # Schwarze Umrandung
                  weight = 1,       # Dünne Umrandung
                  popup = paste(
                    "<strong>", location_data$vote_de, "</strong>: ", 
                    location_data$name, ", ", 
                    location_data$party.label, 
                    "<br><strong>Wahlkreis:</strong> ", location_data$WKR_NAME,
                    "<br><a href='", location_data$abgeordnetenwatch_url, "' target='_blank'>Profil</a>"
                  ))
  })
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- dat_react()
    columns_to_remove <- c("id", "first_name","last_name","abgeordnetenwatch_url", "result","committee1","committee2","topic1","topic2", "topic3","party.id", "field_title", "birth_name", "party_past", "id.mandat", "parliament_period_label", "politician_abgeordnetenwatch_url", "start_date", "end_date", "info", "electoral_data_id", "electoral_data_electoral_list.id", "electoral_data_electoral_list.label", "electoral_data_constituency.id", "electoral_data_constituency.label", "electoral_data_constituency_result_count","foreign_policy_score", "defense_score", "health_score", "energy_score", "migration_score", "employment_score", "environment_score", "law_score")
    data <- data %>% select(-columns_to_remove)
    data
  }))
  
  
  ########################


  output$histogram6 <- renderPlotly({
    dat_r <- dat_react()
    
    # Dominante Stimmen für jede Partei
    dominant_votes <- dat_r %>%
      group_by(party.label, vote) %>%
      summarize(count = n()) %>%
      arrange(party.label, desc(count)) %>%
      group_by(party.label) %>%
      slice(1) %>%
      ungroup() %>%
      select(party.label, vote)
    
    colnames(dominant_votes) <- c("party.label", "party.vote")
    
    # Hinzufügen der dominanten Stimme
    dat_r <- left_join(dat_r, dominant_votes, by = "party.label")
    
    dat_r <- dat_r %>%
      mutate(group = case_when(
        vote == "no_show" ~ "Nicht beteiligt",
        vote != party.vote ~ "Abweichler",
        TRUE ~ "Fraktionstreu"
      ),
      # Umbenennen von vote und party.vote
      vote_de = case_when(
        vote == "yes" ~ "Dafür",
        vote == "no" ~ "Dagegen",
        vote == "abstain" ~ "Enthalten",
        vote == "no_show" ~ "Nicht beteiligt"
      ),
      party_vote_de = case_when(
        party.vote == "yes" ~ "Dafür",
        party.vote == "no" ~ "Dagegen",
        party.vote == "abstain" ~ "Enthalten",
        party.vote == "no_show" ~ "Nicht beteiligt"
      ))
    
    # Plotly Boxplot
    fig <- plot_ly(dat_r, y = ~electoral_data_constituency_result, color = ~group,
                   type = 'box', boxpoints = 'all', jitter = 0.3,
                   hoverinfo = "text",
                   text = ~paste("Name: ", first_name, " ", last_name, "<br>",
                                 "Partei: ", party.label, "<br>",
                                 "Abgestimmt mit: ", vote_de, "<br>Fraktion hat abgestimmt mit: ", party_vote_de)) %>%
      layout(
        title = "Fraktionsdisziplin vs. Abweichler vs. Nicht Beteiligt",
        xaxis = list(title = "Gruppe"),
        yaxis = list(title = "Wahlkreis Ergebnis in Prozent"),
        boxmode = 'group' 
      )
    
    fig
  })
  
  
  output$anovaResults <- renderPrint({
    dat_r <- dat_react()
    
    # Daten für die ANOVA vorbereiten
    dominant_votes <- dat_r %>%
      group_by(party.label, vote) %>%
      summarize(count = n(), .groups = 'drop') %>%
      arrange(party.label, desc(count)) %>%
      group_by(party.label) %>%
      slice(1) %>%
      ungroup() %>%
      select(party.label, vote)
    
    colnames(dominant_votes) <- c("party.label", "party.vote")
    dat_r <- left_join(dat_r, dominant_votes, by = "party.label")
    dat_r <- dat_r %>%
      mutate(group = case_when(
        vote == "no_show" ~ "Nicht beteiligt",
        vote != party.vote ~ "Abweichler",
        TRUE ~ "Fraktionstreu"
      ))
    
    # ANOVA durchführen
    anova_model <- aov(electoral_data_constituency_result ~ group, data = dat_r)
    summary(anova_model)
  })
  

  ##################
  # wordcloud
  output$wordcloud <- renderWordcloud2({
    
    dat_r <- topic_react()
    
    # Processing possible, but not used
    processed_text <- lapply(dat_r$preferredLabel, function(text) {
      return(text)
    })
    
    # Combining all processed text into a single character vector
    all_text <- unlist(processed_text)
    
    word_freq_table <- table(all_text)
    
    # Sorting the words based on frequency in descending order
    sorted_word_freq <- sort(word_freq_table, decreasing = TRUE)
    
    top_30_words <- names(sorted_word_freq)
    
    # Creating a data frame for wordcloud2 with word and frequency
    wordcloud_data <- data.frame(
      word = top_30_words,
      freq = as.numeric(sorted_word_freq[top_30_words])
    )
    
    wordcloud2(data = wordcloud_data)
  })
  
  ###################
  
  output$dendoCarousel <- renderSlickR({
    slickR(c("dendo1.png", "dendo2.png", "dendo3.png", "dendo4.png"))
  })
  

  ############################
  
  
  output$koordinatensystem2 <- renderPlotly({
    
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
                      "Fraktionslos" = "grey",
                      "parteilos" = "grey",
                      "Union (CDU/CSU)" = "black")
    
    dat_r <- dat_react()
    

    dat_r$economic_position <- jitter(dat_r$economic_position)
    dat_r$social_position <- jitter(dat_r$social_position)
    
    # Plotly
    plot <- ggplot(dat_r, aes(x = economic_position, y = social_position)) +
      geom_point(aes(color = party.label, text = paste("Name: ", label.x)), 
                 size = 2, alpha = 0.6, position = position_jitter(width = 0.5, height = 0.5)) +
      scale_color_manual(values = party_colors) +
      labs(title = "Politische Koordinaten der Abgeordneten", 
           x = "Wirtschaftliche Position", 
           y = "Soziale Position", 
           color = "Partei") +
      theme_minimal() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      xlim(-1.0, 1.0) + 
      ylim(-1.0, 1.0) 
    
    # convert
    ggplotly(plot, tooltip = "text")
    
  })
  
  #########################
  
  output$koordinatensystem3 <- renderPlotly({
    
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
                      "Fraktionslos" = "grey",
                      "parteilos" = "grey",
                      "Union (CDU/CSU)" = "black")
    
    dat_r <-  topic_react()
    

    dat_r$economic_position <- jitter(dat_r$economic_position)
    dat_r$social_position <- jitter(dat_r$social_position)
    

    plot <- ggplot(dat_r, aes(x = economic_position, y = social_position)) +
      geom_point(aes(color = party.label, text = paste("Name: ", label.x)), 
                 size = 2, alpha = 0.6, position = position_jitter(width = 0.5, height = 0.5)) +
      scale_color_manual(values = party_colors) +
      labs(title = "Politische Koordinaten der Abgeordneten", 
           x = paste("Position:", input$selected_topic_x),
           y = paste("Position:", input$selected_topic_y),
           color = "Partei") +
      theme_minimal() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      xlim(-1.0, 1.0) + 
      ylim(-1.0, 1.0) 
    
    # convert
    ggplotly(plot, tooltip = "text")
    
  })
  
  
  #reactive part for polls
  nom_react <- reactive({
    
    if (input$data == 1) {
      load("data/wnom_results_2125.RData")
    } else if (input$data == 2) {
      load("data/wnom_results_1721.RData")
    } else if (input$data == 3) {
      load("data/wnom_results_1317.RData")
    } else if (input$data == 4) {
      load("data/wnom_results_0913.RData")
    } 
    return(wnom_results)
  })
  
  ############# NOMINATE SCORES #########
  
  output$nominate_plot <- renderPlotly({
    
    votes_df <- poll_react()
    
    wnom_results <- nom_react()
    
    party_colors <- get_color()
    
    duplicates <- votes_df |>
      dplyr::summarise(n = dplyr::n(), .by = c(mandate.id, poll.id)) |>
      dplyr::filter(n > 1L)
    
    # remeove duplicates
    
    votes_df <- votes_df |>
      dplyr::distinct(mandate.id, poll.id, .keep_all = TRUE)
    
    # Assuming votes_2125_df is your dataframe
    # Step 1: Pivot the dataframe to get the votes matrix
    votes_matrix <- votes_df %>%
      select(mandate.id, poll.id, vote) %>%
      pivot_wider(names_from = poll.id, values_from = vote, names_prefix = "vote_")
    
    
    # Convert the votes to numeric codes
    votes_matrix <- votes_matrix %>%
      mutate(across(starts_with("vote_"), ~ case_when(
        . == "yes" ~ 1,
        . == "no" ~ 6,
        . == "abstain" ~ 9,
        . == "no_show" ~ 0,
        is.na(.) ~ 0,  # Treat NA as notInLegis
        TRUE ~ NA_real_
      )))
    
    # Save the row names as the mandate labels
    row.names(votes_matrix) <- votes_matrix$mandate.id
    votes_matrix <- votes_matrix %>% select(-mandate.id)
    
    # Convert to matrix
    votes_matrix <- as.matrix(votes_matrix)
    
    specific_vote <- input$selected_variable
    filtered_votes <- votes_df[votes_df$poll.label == specific_vote, ]
    
    vote_ID <- unique(filtered_votes$poll.id)
    
    
    # Specific vote ID to be analyzed
    specific_vote_id <- paste0("vote_",vote_ID) # Replace with your actual vote ID
    
    # Extract the vote data for the specific vote
    
    P_rollcall <- votes_matrix
    vote_data <- P_rollcall[, as.character(specific_vote_id)]
    
    # Create a data frame for logistic regression, excluding rows with NAs
    vote_coords <- data.frame(coord1D = wnom_results$legislators$coord1D, 
                              coord2D = wnom_results$legislators$coord2D,
                              Party = as.factor(wnom_results$legislators$party),
                              vote = as.factor(vote_data))
    
    
    # Remove rows with NA values
    vote_coords <- na.omit(vote_coords)
    
    # Filter out the points with vote == 0
    vote_coords_filtered <- subset(vote_coords, vote != 0)
    
    # Replace 1 by yes, 6 by no and 9 by abstain and change it to factor
    vote_coords_filtered$vote <- factor(ifelse(vote_coords_filtered$vote == 1, "yes", 
                                               ifelse(vote_coords_filtered$vote == 6, "no", "abstain")))
    
    
    
    # Train SVM model
    svm_model <- svm(vote ~ coord1D + coord2D , data = vote_coords_filtered, kernel = "linear", scale = FALSE)
    
    # Create grid for prediction
    coord1D_range <- seq(min(vote_coords_filtered$coord1D) - 0.5, max(vote_coords_filtered$coord1D) + 0.5, length.out = 100)
    coord2D_range <- seq(min(vote_coords_filtered$coord2D) - 0.5, max(vote_coords_filtered$coord2D) + 0.5, length.out = 100)
    
    contour_data <- expand.grid(coord1D = coord1D_range, coord2D = coord2D_range)
    
    # Predict on the contour data
    contour_data$pred <- predict(svm_model, newdata = contour_data)
    
    # Plot with ggplot2 and plotly
    gg_svm <- ggplot(vote_coords_filtered, aes(x = coord1D, y = coord2D, color = Party)) +
      geom_point(aes(shape = factor(vote)), size = 1) +  # Größe der Punkte/Shape
      geom_contour(data = contour_data, aes(z = as.numeric(pred)), breaks = 1.5, color = "blue", size = 1) +
      scale_color_manual(values = party_colors) +
      scale_shape_manual(
        values = c("yes" = 17,  # Dreieck nach oben (gefüllt)
                   "no" = 4,    # Dreieck nach unten (gefüllt)
                   "abstain" = 21), # Kreis (gefüllt)
        labels = c("yes" = "Yes",
                   "no" = "No",
                   "abstain" = "Abstain")
      ) +
      theme_minimal() +
      labs(title = paste("NOMINATE Analyse für die Abstimmung:", specific_vote), 
           x = "DW-NOMINATE Dimension 1", 
           y = "DW-NOMINATE Dimension 2",
           shape = "Casted Vote") +
      theme(legend.position = "bottom")
    
    
    
    ggplotly(gg_svm)
    
  })
  
  
  
  ######################################
  
  topic_react <- reactive({
    poll.id <- as.numeric(input$VoteID)
    
    if (input$data == 1) {
      politician_data <- politician_data2125
      votes_df <- votes_2125_df
    } else if (input$data == 2) {
      politician_data <- politician_data1721
      votes_df <- votes_1721_df
    } else if (input$data == 3) {
      politician_data <- politician_data1317
      votes_df <- votes_1317_df
    } else if (input$data == 4) {
      politician_data <- politician_data0913
      votes_df <- votes_0913_df
    }
    
    economic_votes <- votes_df %>%
      filter(Label1 == input$selected_topic_x)
    
    social_votes <- votes_df %>%
      filter(Label1 == input$selected_topic_y)
    

    economic_positions <- economic_votes %>%
      mutate(economic_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  # 
      )) %>%
      group_by(mandate.id) %>%
      summarize(economic_position = mean(economic_score, na.rm = TRUE)) %>%
      ungroup()
    
    social_positions <- social_votes %>%
      mutate(social_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(social_position = mean(social_score, na.rm = TRUE)) %>%
      ungroup()
    
    political_positions <- full_join(economic_positions, social_positions, by = "mandate.id")
    
    votes_df <- left_join(votes_df, political_positions, by = "mandate.id")
    

    
    ######################
    #Add the fraction.vote
    votes_df_modified <- votes_df %>%
      group_by(poll.id, fraction.id, vote) %>%
      tally(name = "n") %>%
      arrange(poll.id, fraction.id, desc(n)) %>%
      group_by(poll.id, fraction.id) %>%
      summarize(
        fraction.vote = first(vote)
      ) %>%
      ungroup()
    
    
    votes_df <- left_join(votes_df, votes_df_modified, by = c("poll.id", "fraction.id"))
    
    #loyalty_percentage
    votes_df_modified1 <- votes_df %>%
      mutate(party.loyalty = ifelse(vote == fraction.vote, "loyal", "dissenter")) %>%
      group_by(mandate.id) %>%
      summarize(loyal_count = sum(party.loyalty == "loyal", na.rm = TRUE),
                total_votes = n()) %>%
      ungroup() %>%
      mutate(loyalty_percentage = loyal_count / total_votes * 100)
    
    votes_df <- left_join(votes_df, votes_df_modified1, by = "mandate.id")
    
    
    specific_vote <- input$selected_variable
    filtered_votes <- votes_df[votes_df$poll.label == specific_vote, ]
    mandate_ids <- filtered_votes$mandate.id
    matching_politicians <- politician_data[politician_data$id.mandat %in% mandate_ids, ]
    matching_politicians$vote <- filtered_votes$vote[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$fraction.vote <- filtered_votes$fraction.vote[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$loyalty_percentage <- round(filtered_votes$loyalty_percentage[match(matching_politicians$id.mandat, filtered_votes$mandate.id)], 2)
    
    matching_politicians$result <- filtered_votes$Result[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$committee1 <- filtered_votes$Ausschuss1[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$committee2 <- filtered_votes$Ausschuss2[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic1 <- filtered_votes$Label1[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic2 <- filtered_votes$Label2[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic3 <- filtered_votes$Label3[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    
    matching_politicians$economic_position <- round(filtered_votes$economic_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$social_position <- round(filtered_votes$social_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    
    
    matching_politicians$answered_percentage <- round(ifelse(matching_politicians$statistic_questions == 0 | is.na(matching_politicians$statistic_questions_answered), 0, matching_politicians$statistic_questions_answered / matching_politicians$statistic_questions * 100), 2)
    matching_politicians <- matching_politicians %>%
      mutate(age = as.numeric(current_year) - year_of_birth)
    
    
    max_age <- input$age_slider[2]
    min_age <- input$age_slider[1]
    
    matching_politicians <- matching_politicians %>%
      filter(age >= min_age, age <= max_age)
    
        d <- classify_occupation(corpus = matching_politicians, id_col = "id", text_col = "education", isco_level = NULL, lang = "de", num_leaves = 1)
        d <- subset(d, select = -conceptUri)
        d$id <- as.integer(d$id)
        matching_politicians <- left_join(matching_politicians, d, by = "id")
        
        if(input$checkboxUnion) {
          matching_politicians <- matching_politicians %>%
            mutate(party.label = ifelse(party.label %in% c("CDU", "CSU"), "Union (CDU/CSU)", party.label))
        }
    
    
    return(matching_politicians)
  })
  
  
  
# table download
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("ergebnisse_tabelle_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      
      data <- dat_react()
      columns_to_remove <- c("first_name","last_name", "result","committee1","committee2","topic1","topic2", "topic3","party.id", "field_title", "birth_name", "party_past", "id.mandat", "parliament_period_label", "politician_abgeordnetenwatch_url", "start_date", "end_date", "info", "electoral_data_id", "electoral_data_electoral_list.id", "electoral_data_electoral_list.label", "electoral_data_constituency.id", "electoral_data_constituency.label", "electoral_data_constituency_result_count")
      data <- data %>% select(-columns_to_remove)
      
      write.csv(data, file)
    }
  )
  

  
  
  ####################################################
  ############# CLUSTERING ###########################
  
  
  output$dendrogramPlot_sample <- renderPlotly({
    
    # Daten vorbereiten
    data <- prepared_data()
    
  
    # Zufällige Stichprobe von 5% der Daten
    sample_data <- data %>% sample_frac(0.05)
    
    numCluster <- input$numClusters
    # Hierarchisches Clustering
    hc <- hclust(dist(sample_data), method = input$linkage)
    
    # Erstellen des Dendrogramms
    p <- fviz_dend(hc, k = numCluster, 
                   main = "Hierarchisches Clustering (Reduziert mit 5% Sample)",
                   cex = 0.5,                 # label size
                   repel = TRUE,               # avoid text overlapping
                   rect = TRUE, # Add rectangle around groups
                   rect_fill = TRUE,
                   horiz = TRUE,
                   )
    
    params <- p$layers[[4]]$aes_params
    p$layers[[4]]$aes_params <- params[!names(params) %in% c('fill', 'colour')]
    p$layers[[4]]$mapping$fill <- quo_set_env(quo(cluster), 
                                                 quo_get_env(p$layers[[4]]$mapping$xmin))
    
    

    p
  })
  

  output$dendrogramPlot_full <- renderPlotly({
    
    data <- prepared_data()
    
    numCluster <- input$numClusters
    # Hierarchisches Clustering
    hc <- hclust(dist(data), method = input$linkage)
    
    
    p <- fviz_dend(hc, k = numCluster, 
                   main = "Hierarchisches Clustering (Kompletter Datensatz)",
                   cex = 0.5,                 # label size
                   repel = TRUE,               # avoid text overlapping
                   rect = TRUE, # Add rectangle around groups
                   rect_fill = TRUE,
                   horiz = TRUE)
    
    params <- p$layers[[4]]$aes_params
    p$layers[[4]]$aes_params <- params[!names(params) %in% c('fill', 'colour')]
    p$layers[[4]]$mapping$fill <- quo_set_env(quo(cluster), 
                                              quo_get_env(p$layers[[4]]$mapping$xmin))
    
  
    
    p
  })
  
  cluster_data <- reactive({
    data <- prepared_data()
    hc <- hclust(dist(data), method = input$linkage)
    clusters <- cutree(hc, k = input$numClusters)
    cluster_df <- data.frame(Name = rownames(data), Cluster = as.factor(clusters))
    cluster_df
  })
  
  output$clusterChart <- renderPlotly({
    cluster_data <- cluster_data()
    
    dat_r <- dat_react()
    
    # Merge dat_r with cluster_data by Name = label.x
    dat_r <- merge(dat_r, cluster_data, by.x = "label.x", by.y = "Name")
    
    # Define custom colors
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
    
    # Plot party.label by cluster
    p <- ggplot(dat_r, aes(x = Cluster, fill = party.label)) +
      geom_bar(position = "dodge") +
      labs(title = "Verteilung der Parteien in den Clustern", x = "Partei", y = "Anzahl") +
      scale_fill_manual(values = party_colors) +  # Apply custom colors
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })

  # Dendrogramm-Rendering mit Silhouettenwerten
  output$dendrogramPlot2 <- renderPlot({
    data <- prepared_data()
    hc <- hclust(dist(data), method = input$linkage)
    data_scaled <- scale(data)
    avg_sil_width <- sapply(2:10, function(k) {
      clustering <- cutree(hc, k = k)
      silhouette_width <- silhouette(clustering, dist(data_scaled))
      mean(silhouette_width[, 3])
    })
    plot(2:10, avg_sil_width, type = "b", pch = 19, frame = FALSE, 
         xlab = "Anzahl der Cluster", ylab = "Durchschnittliche Silhouettenbreite")
  })
  
  
  
  # Reaktive Expression für das Clustering
  clustering <- reactive({
    input$update
    num_clusters <- input$numClusters
    data_for_clustering <- prepared_data()
    hc <- hclust(dist(data_for_clustering), method = input$linkage)
    cluster_assignments <- cutree(hc, k = num_clusters)
    data_for_clustering$cluster <- cluster_assignments
    data_for_clustering
  })
  
  # Reaktive Expression für PCA
  pca_results <- reactive({
    data <- prepared_data()
    prcomp(data, scale. = TRUE)
  })
  
  
  
  # Aggregierte Cluster-Daten darstellen
  output$clusterSummary <- renderTable({
    data <- clustering()
    aggregate(data[, -ncol(data)], by = list(cluster = data$cluster), FUN = mean)
  })
  
  # Boxplot-Rendering
  output$boxplot <- renderPlot({
    data <- clustering()
    var <- input$selectedVariable
    ggplot(data, aes_string(x = "factor(cluster)", y = var, fill = "factor(cluster)")) +
      geom_boxplot() +
      labs(title = paste('Boxplot von', var, 'nach Cluster'), x = 'Cluster', y = var) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Boxplot-Rendering
  output$spider <- renderPlot({
    data <- clustering()
    cluster_data <- aggregate(data[, -ncol(data)], by = list(cluster = data$cluster), FUN = mean)
    
    # Maximal- und Minimalwerte für die Radar-Charts berechnen
    max_vals <- apply(cluster_data[, -1], 2, max)
    min_vals <- apply(cluster_data[, -1], 2, min)
    
    # Für jedes Cluster ein Radar-Chart erstellen
    par(mfrow = c(1, nrow(cluster_data)))  # Layout für mehrere Plots in einer Zeile
    
    for(i in 1:nrow(cluster_data)) {
      radar_data <- rbind(max_vals, min_vals, cluster_data[i, -1])
      rownames(radar_data) <- c("Max", "Min", paste("Cluster", cluster_data$cluster[i]))
      
      radarchart(radar_data, axistype = 1,
                 # Anpassung des Polygons
                 pcol = rgb(0.2, 0.5, 0.5, 0.9),
                 pfcol = rgb(0.2, 0.5, 0.5, 0.5),
                 plwd = 4,
                 
                 # Anpassung des Gitters
                 cglcol = "grey", cglty = 1, axislabcol = "grey",
                 caxislabels = seq(min(min_vals), max(max_vals), length.out = 5),
                 cglwd = 0.8,
                 
                 # Anpassung der Labels
                 vlcex = 0.8,
                 title = paste("Cluster", cluster_data$cluster[i])
      )
    }
    par(mfrow = c(1, 1))  # Layout zurücksetzen
  })
  

  # Dropdown-Menü aktualisieren
  observe({
    updateSelectInput(session, "selectedVariable", choices = names(prepared_data()[, -ncol(prepared_data())]))
  })
  
  
  output$mdsPlot <- renderPlotly({
    
    # Annahme, dass clustering() eine Funktion ist, die bereits definierte Daten liefert
    data_for_clustering <- clustering()  
    dat_r <- dat_react()
    
    # Berechnung der Distanzmatrix
    dist_matrix <- dist(data_for_clustering[, -ncol(data_for_clustering)])
    
    # MDS
    mds <- cmdscale(dist_matrix, k = 2)  
    mds_df <- as.data.frame(mds)
    mds_df$cluster <- as.factor(data_for_clustering$cluster)
    mds_df$party_label <- dat_r$party.label
    mds_df$name <- dat_r$label.x
    
    # Erzeuge das Plotly MDS Scatterplot
    p <- plot_ly(data = mds_df, x = ~V1, y = ~V2, type = 'scatter', mode = 'markers',
                 marker = list(size = 7, opacity = 0.8, color = ~cluster, colorscale = 'Set1'),
                 hoverinfo = 'text',
                 text = ~paste('Name:', name, '<br>Party:', party_label, '<br>Cluster:', cluster),
                 source = 'selectedPoint') %>% 
      layout(title = "MDS Scatterplot der Cluster",
             xaxis = list(title = "Dimension 1"),
             yaxis = list(title = "Dimension 2"),
             legend = list(title = list(text = 'Cluster'), font = list(size = 13)))
    
    return(p)
  })
  
  
  
  
  output$selectedInfo <- renderDataTable({
    eventdata <- event_data("plotly_click", source = "selectedPoint")
    
    if (!is.null(eventdata)) {
      point <- eventdata$pointNumber + 1
      mds_df <- dat_react()  # Aktuelle Daten
      selected <- mds_df[point, ]
      
      # Hinzufügen von Standardspalten und ausgewählten Spalten
      columns_to_show <- c("label.x", "party.label", input$selectedVars)
      datatable <- selected[columns_to_show]
      
      # Anpassung der Hintergrundfarbe
      datatable <- datatable %>%
        datatable(options = list(paging = FALSE, searching = FALSE)) %>%
        formatStyle(names(datatable), backgroundColor = styleEqual(datatable$label.x, c("lightgrey")))
      
      return(datatable)
    }
  }, escape = FALSE)
  
  # Plot für PCA Ladungen
  output$pcaPlot <- renderPlot({
    pca_res <- pca_results()
    fviz_pca_var(pca_res, col.var = "contrib", 
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE)  # Vermeidet Textüberlappung
  })
  
  

  
  output$pcaHeatmap <- renderPlotly({
    pca_res <- pca_results()  # Ihre Funktion, die prcomp ausführt
    loadings <- pca_res$rotation  # Die Ladungsmatrix
    
    # Die transponierte Matrix nutzen, um Hauptkomponenten als Spalten zu haben
    plot_ly(z = ~t(loadings), x = rownames(loadings), y = colnames(loadings), type = "heatmap",
            colorscale = "Blues") %>%
      layout(title = "PCA Loadings Heatmap",
             xaxis = list(title = "Principal Components"),
             yaxis = list(title = "Variables", autorange = "reversed"))  # Um die Variable auf der Y-Achse zu haben
  })
  
  output$loadingsBarplot <- renderPlotly({
    pca_res <- pca_results()  
    loadings <- pca_res$rotation[, 1:2]  # Nur die ersten zwei Hauptkomponenten
    
    # Runden der Ladungen auf zwei Nachkommastellen
    loadings_rounded <- round(loadings, 2)
    
    loadings_df <- as.data.frame(loadings_rounded)
    loadings_df$Variable <- rownames(loadings)
    loadings_long <- reshape2::melt(loadings_df, id.vars = "Variable")
    
    plot_ly(loadings_long, y = ~Variable, x = ~value, color = ~variable, type = 'bar',
            text = ~paste("Wert:", value), hoverinfo = 'text', orientation = 'h') %>%
      layout(xaxis = list(title = 'Ladung', tickformat = ",.2f"),
             yaxis = list(title = "Variable", automargin = TRUE),
             barmode = 'group',
             title = 'Ladungen der Variablen für die ersten zwei Hauptkomponenten')
  })
  
  
  output$pcaBiplot <- renderPlotly({
    pca_res <- pca_results()
    scores <- pca_res$x[, 1:2]
    loadings <- pca_res$rotation[, 1:2]
    

    # Umkehrung der Y-Achse
    scores[, "PC2"] <- -scores[, "PC2"]
    loadings[, "PC2"] <- -loadings[, "PC2"]
    
    
    scores_df <- as.data.frame(scores)
    scores_df$id <- rownames(scores)
    
    loadings_df <- as.data.frame(loadings)
    loadings_df$variable <- rownames(loadings)
    
    p <- plot_ly(scores_df, x = ~PC1, y = ~PC2, text = ~id, type = 'scatter', mode = 'markers',
                 marker = list(size = 12, opacity = 0.5)) %>%
      add_trace(data = loadings_df, x = ~PC1*3, y = ~PC2*3, text = ~variable, mode = 'lines+text',
                line = list(color = 'red'), textposition = 'top right')
    
    p
  })
  
  
  
  
  
  
  
  
  
  
  
  
  output$mdsPlotParty <- renderPlotly({
    data_for_clustering <- clustering()  # Deine vorbereitete Daten
    
    # Berechnung der Distanzmatrix und Durchführung der MDS-Analyse
    dist_matrix <- dist(data_for_clustering[, -ncol(data_for_clustering)])
    mds <- cmdscale(dist_matrix, k = 2)
    mds_df <- as.data.frame(mds)
    mds_df$cluster <- as.factor(data_for_clustering$cluster)
    
    # Zusätzliche Daten für Plotly
    dat_r <- dat_react()
    mds_df$party_label <- dat_r$party.label
    mds_df$name <- dat_r$label.x  # Angenommen, 'name' ist die Spalte mit den Namen
    
    # Wende die Mapping-Funktion auf den DataFrame an
    mds_df <- mds_df %>% 
      mutate(color = sapply(party_label, get_color))
    
    # Nun nutze die neue 'color'-Spalte in plot_ly
    p <- plot_ly(data = mds_df, x = ~V1, y = ~V2, type = 'scatter', mode = 'markers',
                 marker = list(size = 7, opacity = 0.8, color = ~color),  # Verwende die 'color'-Spalte für Farben
                 hoverinfo = 'text',
                 text = ~paste('Name:', name, '<br>Party:', party_label)) %>%
      layout(title = 'MDS Scatterplot nach Parteizugehörigkeit',
             xaxis = list(title = 'Dimension 1'),
             yaxis = list(title = 'Dimension 2'))
    return(p)
  })
  
  output$mdsPlotCoa <- renderPlotly({
    data_for_clustering <- clustering()  # Assume this fetches your clustering data
    
    # Calculate the distance matrix and perform MDS
    dist_matrix <- dist(data_for_clustering[, -ncol(data_for_clustering)])
    mds_results <- cmdscale(dist_matrix, k = 2)
    mds_df <- as.data.frame(mds_results)
    mds_df$cluster <- as.factor(data_for_clustering$cluster)
    
    dat_r <- dat_react()  # Assume this fetches the reactive data for your session
    
    # Map party labels to your MDS data frame
    mds_df$party_label <- dat_r$party.label
    mds_df$name <- dat_r$label.x  # Assuming 'name' column exists for hover info
    
    
    # Wende die Mapping-Funktion auf den DataFrame an
    mds_df$color <- get_coa_color(mds_df$party_label, input$data)
    
    # Nun nutze die neue 'color'-Spalte in plot_ly
    p <- plot_ly(data = mds_df, x = ~V1, y = ~V2, type = 'scatter', mode = 'markers',
                 marker = list(size = 7, opacity = 0.8, color = ~color),  # Verwende die 'color'-Spalte für Farben
                 hoverinfo = 'text',
                 text = ~paste('Name:', name, '<br>Party:', party_label)) %>%
      layout(title = 'MDS Scatterplot nach Koalitionszugehörigkeit (blau = KOA, orange = OPP)',
             xaxis = list(title = 'Dimension 1'),
             yaxis = list(title = 'Dimension 2'),
    legend = list(title = list(text = 'Party'), font = list(size = 13)))
    return(p)
  })
  
  
  

#######################################
  
  
  prepared_data <- reactive({
    
    poll.id <- as.numeric(input$VoteID)
    
    if (input$data == 1) {
      politician_data <- politician_data2125
      votes_df <- votes_2125_df
    } else if (input$data == 2) {
      politician_data <- politician_data1721
      votes_df <- votes_1721_df
    } else if (input$data == 3) {
      politician_data <- politician_data1317
      votes_df <- votes_1317_df
    } else if (input$data == 4) {
      politician_data <- politician_data0913
      votes_df <- votes_0913_df
    }
    

    economic_votes <- votes_df %>%
      filter(Label1 == "Öffentliche Finanzen, Steuern und Abgaben")
    
    social_votes <- votes_df %>%
      filter(Label1 == "Gesellschaftspolitik, soziale Gruppen")
    
    foreign_policy_votes <- votes_df %>%
      filter(Label1 == "Außenpolitik und internationale Beziehungen")
    
    energy_votes <- votes_df %>%
      filter(Label1 == "Energie")    
    
    defense_votes <- votes_df %>%
      filter(Label1 == "Verteidigung")
    
    health_votes <- votes_df %>%
      filter(Label1 == "Gesundheit")
    
    migration_votes <- votes_df %>%
      filter(Label1 == "Migration und Aufenthaltsrecht")
    
    employment_votes <- votes_df %>%
      filter(Label1 == "Arbeit und Beschäftigung")
    
    environment_votes <- votes_df %>%
      filter(Label1 == "Umwelt")
    
    law_votes <- votes_df %>%
      filter(Label1 == "Recht")
    

    economic_positions <- economic_votes %>%
      mutate(economic_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(economic_position = mean(economic_score, na.rm = TRUE)) %>%
      ungroup()
    
    social_positions <- social_votes %>%
      mutate(social_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(social_position = mean(social_score, na.rm = TRUE)) %>%
      ungroup()
    
    foreign_policy_positions <- foreign_policy_votes %>%
      mutate(foreign_policy_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(foreign_policy_position = mean(foreign_policy_score, na.rm = TRUE)) %>%
      ungroup()
    
    defense_positions <- defense_votes %>%
      mutate(defense_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(defense_position = mean(defense_score, na.rm = TRUE)) %>%
      ungroup()
    
    health_positions <- health_votes %>%
      mutate(health_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(health_position = mean(health_score, na.rm = TRUE)) %>%
      ungroup()
    
    energy_positions <- energy_votes %>%
      mutate(energy_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(energy_position = mean(energy_score, na.rm = TRUE)) %>%
      ungroup()
    
    migration_positions <- migration_votes %>%
      mutate(migration_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(migration_position = mean(migration_score, na.rm = TRUE)) %>%
      ungroup()
    
    employment_positions <- employment_votes %>%
      mutate(employment_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(employment_position = mean(employment_score, na.rm = TRUE)) %>%
      ungroup()
    
    environment_positions <- environment_votes %>%
      mutate(environment_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(environment_position = mean(environment_score, na.rm = TRUE)) %>%
      ungroup()
    
    law_positions <- law_votes %>%
      mutate(law_score = case_when(
        vote == "yes" ~ 1,
        vote == "no" ~ -1,
        TRUE ~ 0  
      )) %>%
      group_by(mandate.id) %>%
      summarize(law_position = mean(law_score, na.rm = TRUE)) %>%
      ungroup()

    
    
    
    # combine into one dataframe
    political_positions <- full_join(economic_positions, social_positions, by = "mandate.id") %>%
      full_join(foreign_policy_positions, by = "mandate.id") %>%
      full_join(defense_positions, by = "mandate.id") %>%
      full_join(energy_positions, by = "mandate.id") %>%
      full_join(health_positions, by = "mandate.id") %>%
      full_join(migration_positions, by = "mandate.id") %>%
      full_join(employment_positions, by = "mandate.id") %>%
      full_join(environment_positions, by = "mandate.id") %>%
      full_join(law_positions, by = "mandate.id")
    
    votes_df <- left_join(votes_df, political_positions, by = "mandate.id")
    

    
    ######################
    #Add the fraction.vote
    votes_df_modified <- votes_df %>%
      group_by(poll.id, fraction.id, vote) %>%
      tally(name = "n") %>%
      arrange(poll.id, fraction.id, desc(n)) %>%
      group_by(poll.id, fraction.id) %>%
      summarize(
        fraction.vote = first(vote)
      ) %>%
      ungroup()
    
    

    votes_df <- left_join(votes_df, votes_df_modified, by = c("poll.id", "fraction.id"))
    
    # loyalty_percentage
    votes_df_modified1 <- votes_df %>%
      mutate(party.loyalty = ifelse(vote == fraction.vote, "loyal", "dissenter")) %>%
      group_by(mandate.id) %>%
      summarize(loyal_count = sum(party.loyalty == "loyal", na.rm = TRUE),
                total_votes = n()) %>%
      ungroup() %>%
      mutate(loyalty_percentage = loyal_count / total_votes * 100)
    
    votes_df <- left_join(votes_df, votes_df_modified1, by = "mandate.id")
    

    
    specific_vote <- input$selected_variable
    filtered_votes <- votes_df[votes_df$poll.label == specific_vote, ]
    mandate_ids <- filtered_votes$mandate.id
    matching_politicians <- politician_data[politician_data$id.mandat %in% mandate_ids, ]
    matching_politicians$vote <- filtered_votes$vote[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$fraction.vote <- filtered_votes$fraction.vote[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$party.loyalty <- filtered_votes$party.loyalty[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$loyalty_percentage <- round(filtered_votes$loyalty_percentage[match(matching_politicians$id.mandat, filtered_votes$mandate.id)], 2)
    
    matching_politicians$result <- filtered_votes$Result[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$committee1 <- filtered_votes$Ausschuss1[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$committee2 <- filtered_votes$Ausschuss2[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic1 <- filtered_votes$Label1[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic2 <- filtered_votes$Label2[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic3 <- filtered_votes$Label3[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    
    matching_politicians$economic_position <- round(filtered_votes$economic_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$social_position <- round(filtered_votes$social_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    
    matching_politicians$foreign_policy_score <- round(filtered_votes$foreign_policy_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$defense_score <- round(filtered_votes$defense_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$energy_score <- round(filtered_votes$energy_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$health_score <- round(filtered_votes$health_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$migration_score <- round(filtered_votes$migration_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$employment_score <- round(filtered_votes$employment_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$environment_score <- round(filtered_votes$environment_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    matching_politicians$law_score <- round(filtered_votes$law_position[match(matching_politicians$id.mandat, filtered_votes$mandate.id)],3)
    
    
    matching_politicians$answered_percentage <- round(ifelse(matching_politicians$statistic_questions == 0 | is.na(matching_politicians$statistic_questions_answered), 0, matching_politicians$statistic_questions_answered / matching_politicians$statistic_questions * 100), 2)
    matching_politicians <- matching_politicians %>%
      mutate(age = as.numeric(current_year) - year_of_birth)
    
    
    max_age <- input$age_slider[2]
    min_age <- input$age_slider[1]
    
    matching_politicians <- matching_politicians %>%
      filter(age >= min_age, age <= max_age)

    if(input$checkboxUnion) {
      matching_politicians <- matching_politicians %>%
        mutate(party.label = ifelse(party.label %in% c("CDU", "CSU"), "Union (CDU/CSU)", party.label))
    }
    
 
    data <- matching_politicians
    
    rownames(data) <- data$label.x
    
    # Auswahl relevanter Spalten
    relevant_columns <- input$selectedVars
    data_for_clustering <- data[, relevant_columns, drop = FALSE]
    
    # Umwandlung kategorialer Daten in numerische Werte
    categorical_columns <- intersect(relevant_columns, c('sex', 'vote'))
    for (col in categorical_columns) {
      data_for_clustering[[col]] <- as.numeric(factor(data_for_clustering[[col]]))
    }
    
    # fehlende Werte
    numeric_columns <- setdiff(relevant_columns, categorical_columns)
    for (col in numeric_columns) {
      if(col %in% names(data_for_clustering)) {
        data_for_clustering[[col]][is.na(data_for_clustering[[col]])] <- median(data_for_clustering[[col]], na.rm = TRUE)
      }
    }
    
    return(data_for_clustering)
  })
  

  
  ####################
  
  output$silhouetteScore <- renderText({
    data <- clustering()
    dist_matrix <- dist(data[, -ncol(data)])
    hc <- hclust(dist_matrix, method = input$linkage)
    cluster_assignments <- cutree(hc, k = input$numClusters)
    silhouette_score <- silhouette(cluster_assignments, dist_matrix)
    avg_sil_width <- mean(silhouette_score[, "sil_width"])
    
    paste("Durchschnittliche Silhouettenbreite: ", round(avg_sil_width, 3))
  })
  

  
  output$adjustedRandIndex <- renderText({
    # Angenommen, Sie haben eine Variable `true_labels` in `data`
    data <- clustering()
    dat_r <- dat_react() 
    hc <- hclust(dist(data[, -ncol(data)]), method = input$linkage)
    cluster_assignments <- cutree(hc, k = input$numClusters)
    
    # Hier müssen Sie anpassen, woher Sie `true_labels` bekommen
    true_labels <- dat_r$party.label
    
    ari <- adjustedRandIndex(true_labels, cluster_assignments)
    paste("Adjusted Rand Index: ", round(ari, 3))
  })
  
  output$confusionMatrix <- renderTable({
    data <- clustering()
    dat_r <- dat_react() 
    hc <- hclust(dist(data[, -ncol(data)]), method = input$linkage)
    cluster_assignments <- cutree(hc, k = input$numClusters)
    true_labels <- dat_r$party.label  # Angenommen, diese Spalte existiert in Ihren Daten
    
    confusionMatrix(factor(cluster_assignments), factor(true_labels))
  })
  
  output$variableImportance <- renderTable({
    data <- clustering()
    data_summary <- aggregate(. ~ cluster, data, mean)
    data_summary
  })
  
  
  # Angenommen, 'data_for_clustering$cluster' enthält Ihre Cluster-Zuweisungen
  # und 'dat_r$party.label' enthält die Parteizugehörigkeiten
  
  output$check <- renderPlot({
    data_for_clustering <- clustering()
    dat_r <- dat_react() 
    hc <- hclust(dist(data[, -ncol(data_for_clustering)]), method = input$linkage)
    cluster_assignments <- cutree(hc, k = input$numClusters)
    
    # Hier müssen Sie anpassen, woher Sie `true_labels` bekommen
    true_labels <- dat_r$party.label
    
    table(dat_r$party.label, data_for_clustering$cluster)
    
    data_to_plot <- data.frame(Party = dat_r$party.label, Cluster = data_for_clustering$cluster)
    
    ggplot(data_to_plot, aes(x = Cluster, fill = Party)) +
      geom_bar(position = "dodge") +
      labs(title = "Verteilung der Parteien über Cluster", x = "Cluster", y = "Anzahl der Politiker") +
      theme_minimal()
  })
  

  

  output$clusterMetrics <- renderTable({
    metrics <- calculate_metrics(clustering)
    homogeneity_table <- metrics$homogeneity_scores
    completeness_table <- metrics$completeness_scores
    
    # Zusammenführen der Homogenitäts- und Vollständigkeitsscores in einer Tabelle
    final_table <- bind_rows(
      homogeneity_table %>% mutate(Type = "Homogeneity"),
      completeness_table %>% mutate(Type = "Completeness")
    ) %>% 
      select(Type, everything())
    
    final_table
  })
  
  calculate_metrics <- function(clustering_data) {
    # Clustering Data wird von deiner Clustering-Funktion bereitgestellt
    data <- clustering()
    dat_r <- dat_react() 
    data$party.label <- dat_r$party.label
    
    # Berechne den Homogenitätsscore jedes Clusters
    homogeneity_scores <- data %>% 
      group_by(cluster) %>%
      count(party.label) %>% 
      mutate(proportion = n / sum(n)) %>% 
      summarise(homogeneity_score = max(proportion)) %>%
      ungroup()
    
    # Berechne den Vollständigkeitsscore für jede Partei
    completeness_scores <- data %>% 
      group_by(party.label) %>%
      count(cluster) %>% 
      mutate(proportion = n / sum(n)) %>% 
      summarise(completeness_score = max(proportion)) %>%
      ungroup()
    
    list(homogeneity_scores = homogeneity_scores, completeness_scores = completeness_scores)
  }
  
  calculate_metrics2 <- function(clustering_data) {
    data <- clustering()
    dat_r <- dat_react() 
    data$party.label <- dat_r$party.label
    
    # Berechne die Homogenität jedes Clusters
    homogeneity_scores <- data %>%
      group_by(cluster) %>%
      count(party.label) %>%
      top_n(1, n) %>%
      ungroup() %>%
      mutate(homogeneity_score = n / sum(n)) %>%
      select(cluster, homogeneity_score) %>%
      distinct()
    
    # Berechne die Vollständigkeit für jede Partei
    completeness_scores <- data %>%
      group_by(party.label) %>%
      count(cluster) %>%
      top_n(1, n) %>%
      ungroup() %>%
      mutate(completeness_score = n / sum(n)) %>%
      select(party.label, completeness_score) %>%
      distinct()
    
    list(homogeneity = homogeneity_scores, completeness = completeness_scores)
  }
  
  
  # Visualisiere die Ergebnisse in Shiny mit Plotly
  output$metricsPlot <- renderPlotly({
    metrics <- calculate_metrics(clustering)
    
    # Homogenität
    homogeneity_plot <- plot_ly(metrics$homogeneity, x = ~cluster, y = ~homogeneity_score, type = 'bar', name = 'Homogenität') %>%
      layout(yaxis = list(title = 'Score'), barmode = 'group')
    
    # Vollständigkeit
    completeness_plot <- plot_ly(metrics$completeness, x = ~party.label, y = ~completeness_score, type = 'bar', name = 'Vollständigkeit') %>%
      layout(yaxis = list(title = 'Score'), barmode = 'group')
    
    subplot(homogeneity_plot, completeness_plot, nrows = 2, shareX = FALSE)
  })
  
  ###################################
  
  
  #reactive part
  obs_react <- reactive({
    poll.id <- as.numeric(input$VoteID)
    
    if (input$data == 1) {
      politician_data <- politician_data2125
      votes_df <- votes_2125_df
    } else if (input$data == 2) {
      politician_data <- politician_data1721
      votes_df <- votes_1721_df
    } else if (input$data == 3) {
      politician_data <- politician_data1317
      votes_df <- votes_1317_df
    } else if (input$data == 4) {
      politician_data <- politician_data0913
      votes_df <- votes_0913_df
    }
    
    

    
    specific_vote <- input$selected_variable
    filtered_votes <- votes_df[votes_df$poll.label == specific_vote, ]
    mandate_ids <- filtered_votes$mandate.id
    matching_politicians <- politician_data[politician_data$id.mandat %in% mandate_ids, ]
    matching_politicians$vote <- filtered_votes$vote[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
   
    matching_politicians$result <- filtered_votes$Result[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$committee1 <- filtered_votes$Ausschuss1[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$committee2 <- filtered_votes$Ausschuss2[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic1 <- filtered_votes$Label1[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic2 <- filtered_votes$Label2[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    matching_politicians$topic3 <- filtered_votes$Label3[match(matching_politicians$id.mandat, filtered_votes$mandate.id)]
    
    
    max_age <- input$age_slider[2]
    min_age <- input$age_slider[1]
    
    initial_obs <- nrow(matching_politicians)
    
    
    matching_politicians <- matching_politicians %>%
      mutate(age = as.numeric(current_year) - year_of_birth) %>%
      # Filter anwenden, aber Zeilen mit NA in `age` behalten
      filter(is.na(age) | (age >= min_age & age <= max_age))
    
    
    final_obs <- nrow(matching_politicians)
    
    # Anzahl der gefilterten Beobachtungen berechnen
    filtered_obs <- initial_obs - final_obs 
    
    # Union Checkbox
    if(input$checkboxUnion) {
      matching_politicians <- matching_politicians %>%
        mutate(party.label = ifelse(party.label %in% c("CDU", "CSU"), "Union (CDU/CSU)", party.label))
    }
    
    
    return(filtered_obs)
  })
  
  # Aktualisieren des dynamischen Textes
  output$filtered_obs_info <- renderUI({
    
    data <- obs_react()
    # Filterung durchführen und Anzahl der gefilterten Beobachtungen berechnen
    number_of_filtered_observations <- data
    
    # Dynamischen Text erzeugen
    HTML(paste("Anzahl der gefilterten Abgeordneten: ", number_of_filtered_observations))
  })
  
  output$dynamicInfoText <- renderUI({
    info <- legislatureInfo[[input$data]]
    infoContent <- paste(
      "<h1>Legislaturperiode", info$jahre, "</h1>",
      "<p>", info$text, "</p>",
      "<ul>
        <li>Anzahl der Abgeordneten: ", info$abgeordnete, "</li>
        <li>Anzahl der Parteien: ", info$parteien, "</li>
        <li>Koalitionen: ", info$koalitionen, "</li>
      </ul>"
    )
    HTML(infoContent)
  })
  
  ###############################################
  
  
#### Parliament Plot ######

  
  # Rendern des Plots in der Shiny-App
  output$parliamentPlot <- renderPlotly({
    
    
    # Lade die Daten
    data <- dat_react()
    
    # Wende die Farben an und konvertiere in tibble
    party_colors <- get_color()
    
    # Konvertiere Farben in einen tibble
    color_data <- as_tibble(party_colors, rownames = "party.label")
    colnames(color_data) <- c("party_name_short", "colour")
    
    # Links-Rechts-Reihenfolge für die deutschen Parteien
    left_right_order <- c(
      "Die Linke." = 2,
      "DIE LINKE" = 2,
      "BSW" = 1,
      "SPD" = 3,
      "Bündnis 90/Die Grünen" = 4,
      "BÜNDNIS 90/DIE GRÜNEN" = 4,
      "DIE GRÜNEN" = 4,
      "Volt" = 4,
      "FDP" = 5,
      "FREIE WÄHLER" = 6,
      "SSW" = 6,
      "CDU" = 7,
      "CSU" = 8,
      "CDU/CSU" = 8,
      "Union (CDU/CSU)" = 8,
      "AfD" = 9,
      "Fraktionslos" = 10,
      "fraktionslos" = 10,
      "parteilos" = 10
    )
    
    # Wähle und formatiere die election_data Daten
    election_data <- data %>%
      select("party.label", "label.x") %>%
      mutate(seats = 1, seats_total = nrow(data), left_right = left_right_order[party.label]) %>%
      rename(party_name_short = party.label, member_name = label.x) %>%
      mutate(left_right = factor(left_right, levels = 1:8))
    
    # Anzahl der Sitze und Zeilen bestimmen
    seats_election <- election_data %>% distinct(seats_total) %>% pull()
    parl_rows_df <- tibble(seats = seq(0, 700, 100), parl_rows = seq(5, 12, 1))
    parl_rows_nr <- parl_rows_df %>% filter(seats < seats_election) %>% filter(parl_rows == max(parl_rows)) %>% pull(parl_rows)
    
    # Anordnung nach Links-Rechts-Position
    election_lr_arranged <- election_data %>%
      arrange(left_right) %>%
      mutate(
        cum_seats = cumsum(seats),
        seat_share = seats / seats_total * 100,
        seat_share_label = paste0(round(seat_share, 1), "%"),
        cum_seats_position = if_else(cum_seats == min(cum_seats), cum_seats / 2, lag(cum_seats) + (seats / 2)),
        full_label = paste0(seats, " Seats (", seat_share_label, ")")
      )
    
    # Parlamentsdaten erstellen
    election_parliament <- parliament_data(
      election_data = election_lr_arranged,  
      parl_rows = parl_rows_nr,
      type = 'semicircle',
      party_seats = election_lr_arranged$seats
    ) %>%
      mutate(member_name = election_data$member_name) %>%
      left_join(color_data, by = "party_name_short")
    
    # Parlamentsplot erstellen
    seats_parl <- ggplot(election_parliament, 
                         aes(x, y, color = party_name_short, text = paste("Member:", member_name, "<br>Party:", party_name_short))) +
      geom_parliament_seats() +
      theme_ggparliament(legend = FALSE) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(nrow = 2)) +
      scale_color_manual("Party", values = setNames(color_data$colour, color_data$party_name_short))
    
    # Interaktiver Plot mit Plotly
    ggplotly(seats_parl, tooltip = "text")

  })
  
  # Überwachung des Tabwechsels
  observeEvent(input$tabs, {
    # Überprüfen, ob im Clusteranalyse-Tab oder im Tab 2 (politische Optionen)
    if (input$tabs == 4) { # Clusteranalyse-Tab
      shinyjs::show("clusterOptions")
    } else if (input$tabs == 3) { # Tab 2 für politische Optionen
      shinyjs::show("poliOptions") # Hier sollte poliOptions sichtbar sein
    } else {
      shinyjs::hide("clusterOptions")
      shinyjs::hide("poliOptions") # Versteckt poliOptions, wenn nicht in Tab 2
    }
  })
  
}


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
  politician_data2125 <- read.csv("all_politicians_bt_21_25.csv")
  politician_data1721 <- read.csv("all_politicians_bt_17_21.csv")
  politician_data1317 <- read.csv("all_politicians_bt_13_17.csv")
  politician_data0913 <- read.csv("all_politicians_bt_09_13.csv")
  votes_2125_df<- read_csv("all_votes_data_2125_df.csv", col_types = cols(vote = col_factor(levels = c("yes", "no", "no_show", "abstain")), mandate.id = col_integer(), reason_no_show = col_character(),  reason_no_show_other = col_character(), poll.id = col_integer(), fraction.id = col_integer()))
  votes_1721_df<- read_csv("all_votes_data_1721_df.csv", col_types = cols(vote = col_factor(levels = c("yes", "no", "no_show", "abstain")), mandate.id = col_integer(), reason_no_show = col_character(),  reason_no_show_other = col_character(), poll.id = col_integer(), fraction.id = col_integer()))
  votes_1317_df<- read_csv("all_votes_data_1317_df.csv", col_types = cols(vote = col_factor(levels = c("yes", "no", "no_show", "abstain")), mandate.id = col_integer(), reason_no_show = col_character(),  reason_no_show_other = col_character(), poll.id = col_integer(), fraction.id = col_integer()))
  votes_0913_df<- read_csv("all_votes_data_0913_df.csv", col_types = cols(vote = col_factor(levels = c("yes", "no", "no_show", "abstain")), mandate.id = col_integer(), reason_no_show = col_character(),  reason_no_show_other = col_character(), poll.id = col_integer(), fraction.id = col_integer()))
  polls_2125_df<- read.csv("all__poll_2125_df.csv")
  polls_1721_df<- read.csv("all__poll_1721_df.csv")
  polls_1317_df<- read.csv("all__poll_1317_df.csv")
  polls_0913_df<- read.csv("all__poll_0913_df.csv")
  
  votes_2125_df <- left_join(votes_2125_df, polls_2125_df, by = "poll.id")
  votes_1721_df <- left_join(votes_1721_df, polls_1721_df, by = "poll.id")
  votes_1317_df <- left_join(votes_1317_df, polls_1317_df, by = "poll.id")
  votes_0913_df <- left_join(votes_0913_df, polls_0913_df, by = "poll.id")
  
  


  
  #reactive part
  dat_react <- reactive({
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
    current_year <- format(Sys.Date(), "%Y")
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
        return(c("SPD", "Union (CDU/CSU)", "Bündnis 90/Die Grünen", "AfD", "FDP", "DIE LINKE", "SSW", "Fraktionslos", "parteilos"))
      } else {
        return(c("SPD", "CDU", "Bündnis 90/Die Grünen", "AfD","FDP", "CSU", "DIE LINKE", "SSW", "Fraktionslos", "parteilos"))
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
    mutate(sex = ifelse(sex == "m", "Männer", ifelse(sex == "f", "Frauen", "Unbekannt")))  # Geschlecht umbenennen

  my_colors <- c("yes" = "#a4d67a", "no" = "#c76f5e", "abstain" = "#e2e2e2", "no_show" = "#a6a6a6")
  
  # Create a Plotly plot
  plot_ly(vote_party_counts_sex, x = ~sex, y = ~n, type = 'bar', color = ~vote, colors = my_colors) %>%
    layout(
      title = "Abstimmungsverhalten nach Geschlecht",
      legend = list(title = "Geschlecht"),
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
    
    vote_counts$vote <- factor(vote_counts$vote, levels = rev(unique(vote_counts$vote)))
    
    # stacked bar plot
    p <- ggplot(vote_counts, aes(x = "", y = n, fill = vote)) +
      geom_bar(stat = "identity", width = 0.6) +
      coord_flip() +  # horizontal
      scale_fill_manual(values = c("yes" = "#a4d67a", "no" = "#c76f5e", "abstain" = "#e2e2e2", "no_show" = "#a6a6a6")) +
      theme(legend.position = "none") +  
      geom_text(aes(y = CumCount - 0.5 * n, label = paste(sprintf("%.1f%%", Prozent))), hjust = 0.5, size = 3.5, check_overlap = TRUE) +
      labs(title = 'Abstimmungsverhalten (gesamt)', x = NULL, y = "Anzahl der Stimmen") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
    # convert to plotly
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
  
  library(leaflet)
  
  output$leaflet_map <- renderLeaflet({
    dat_r <- dat_react()
    
    location_data <- dat_r
    location_data$name <- dat_r$label.x
    
    # Jittering 
    jitter_amount <- 0.001  # could be adjusted
    set.seed(123)  
    location_data$lat_jitter <- location_data$lat + runif(nrow(location_data), -jitter_amount, jitter_amount)
    location_data$long_jitter <- location_data$long + runif(nrow(location_data), -jitter_amount, jitter_amount)
    
    myColors <- c("yes" = "#a4d67a", "no" = "#c76f5e", "abstain" = "#c7b6a7", "no_show" = "#7c7b7d")
    pal <- colorFactor(myColors, levels = location_data$vote)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 10, lat = 51.5, zoom = 6) %>%
      addCircleMarkers(data = location_data, radius = 5, lng = location_data$long_jitter, lat = location_data$lat_jitter, color = ~pal(location_data$vote), opacity = 0.9, popup = paste(
        "<strong>", location_data$vote, "</strong>: ", 
        location_data$name, ", ", 
        location_data$party.label, 
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
    
    # dominant vote for each party
    dominant_votes <- dat_r %>%
      group_by(party.label, vote) %>%
      summarize(count = n()) %>%
      arrange(party.label, desc(count)) %>%
      group_by(party.label) %>%
      slice(1) %>%
      ungroup() %>%
      select(party.label, vote)
    
    colnames(dominant_votes) <- c("party.label", "party.vote")
    
    # adding dominant vote
    dat_r <- left_join(dat_r, dominant_votes, by = "party.label")
    
    dat_r <- dat_r %>%
      mutate(group = case_when(
        vote == "no_show" ~ "Nicht beteiligt",
        vote != party.vote ~ "Abweichler",
        TRUE ~ "Fraktionstreu"
      ))
    
    # Plotly Boxplot
    fig <- plot_ly(dat_r, y = ~electoral_data_constituency_result, color = ~group,
                   type = 'box', boxpoints = 'all', jitter = 0.3,
                   hoverinfo = "text",
                   text = ~paste("Name: ", first_name, " ", last_name, "<br>",
                                 "Partei: ", party.label, "<br>",
                                 "Abgestimmt mit: ", vote, "<br>Fraktion hat abgestimmt mit: ", party.vote)) %>%
      layout(
        title = "Fraktionsdisziplin vs. Abweichler vs. Nicht Beteiligt",
        xaxis = list(title = "Gruppe"),
        yaxis = list(title = "Wahlkreis Ergebnis in Prozent"),
        boxmode = 'group' 
      )
    
    fig
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
    current_year <- format(Sys.Date(), "%Y")
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
  



  
  output$dendrogramPlot <- renderPlotly({
    
    data <- prepared_data()
    # Hierarchisches Clustering
    hc <- hclust(dist(data), method = input$linkage)
    
    dhc <- as.dendrogram(hc)
    
    data <- dendro_data(dhc, type = "rectangle")
    p <- ggplot(segment(data)) + 
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
      coord_flip() + 
      scale_y_reverse(expand = c(0.2, 0))
    
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
                 text = ~paste('Name:', name, '<br>Party:', party_label, '<br>Cluster:', cluster)) %>% 
      layout(title = "MDS Scatterplot der Cluster",
             xaxis = list(title = "Dimension 1"),
             yaxis = list(title = "Dimension 2"),
             legend = list(title = list(text = 'Cluster'), font = list(size = 13)))
    
    return(p)
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
    current_year <- format(Sys.Date(), "%Y")
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


  
  # Beispiel, wie die Funktion in Shiny genutzt werden könnte
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
  
  
  
}

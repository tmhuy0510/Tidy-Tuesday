library(shiny)
library(tidyverse)
library(elo)
library(shinydashboard)

elo_df <- readRDS("../r_objects/elo_df.rds")
tidy_df <- readRDS("../r_objects/tidy_ufc.rds")

shinyServer(function(input, output) {

    output$out_top_ranking <- renderTable({
        elo.run(Winner ~ fighter + opponent,
                data = elo_df %>% 
                    filter(weight_class == input$in_weight_class),
                k = input$in_k_value) %>% 
            rank.teams() %>% 
            tibble(Name = names(.),
                   Ranking = .) %>% 
            arrange(Ranking) %>% 
            slice(1:10)
    }, striped = T, bordered = T, width = "100%", spacing = "m", align = "c")
    
    output$out_fighter_selection <- renderUI({
        fighter_selection <- 
            elo.run(Winner ~ fighter + opponent,
                data = elo_df %>% 
                    filter(weight_class == input$in_weight_class),
                k = input$in_k_value) %>% 
            rank.teams() %>% 
            tibble(Name = names(.),
                   Ranking = .) %>% 
            arrange(Ranking) %>% 
            slice(1:10) %>% 
            pull(Name)
        
        selectInput("in_fighter_selection",
                    label = "Select a fighter in the top 10 list",
                    choices = fighter_selection)
    })
    
    output$out_elo_histogram <- renderPlot({
        tidy_elo_history() %>% 
            group_by(fighter) %>%
            slice_max(date) %>% 
            ungroup() %>% 
            filter(weight_class == input$in_weight_class) %>% 
            ggplot(aes(fighter_elo)) +
            geom_histogram(color = "white", fill = "midnightblue", bins = 30) +
            labs(x = "ELO scores", y = NULL) +
            theme(text = element_text(size = 18),
                  axis.title.x = element_text(size = 16))
    })
    
    output$out_fighter_performance <- renderPlot({
        tidy_elo_history() %>% 
            filter(fighter == input$in_fighter_selection) %>%
            rename("Winning Probability" = fighter_prob,
                   "Elo Change After Match" = fighter_elo_change,
                   "Updated Elo After Match" = fighter_elo) %>% 
            pivot_longer(c(`Winning Probability`, 
                           `Elo Change After Match`, 
                           `Updated Elo After Match`),
                         names_to = "elo_para",
                         values_to = "value") %>% 
            ggplot(aes(date, value, color = elo_para)) +
            geom_line(show.legend = F, size = 1) +
            geom_point(show.legend = F, size = 2) +
            facet_wrap(vars(elo_para), nrow = 3, scales = "free_y") +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            labs(x = NULL, y = NULL, 
                 title = paste("ELO scores and Winning probability of", input$in_fighter_selection)) +
            theme(text = element_text(size = 18),
                  title = element_text(size = 14))
    }, height = 600)
    
    output$out_fighter_card <- renderUI({
        fighter_card_selection <- tidy_elo_history_2() %>%
            filter(weight_class == input$in_weight_class_2) %>% 
            select(fighter) %>% 
            distinct() %>% 
            arrange(fighter) %>% 
            pull(fighter)
        
        selectInput("fighter_card_selection",
                    "Select a fighter",
                    choices = fighter_card_selection)
    })
    
    output$out_opponent_card <- renderUI({
        opponent_card_selection <- tidy_elo_history_2() %>%
            filter(weight_class == input$in_weight_class_2) %>% 
            select(fighter) %>% 
            distinct() %>%
            filter(fighter != input$fighter_card_selection) %>% 
            arrange(fighter) %>% 
            pull(fighter)
        
        selectInput("opponent_card_selection",
                    "Select an opponent",
                    choices = opponent_card_selection)
    })
    
    output$fighter_attributes <- renderTable({
        tidy_df %>% 
            filter(fighter == input$fighter_card_selection) %>% 
            slice_max(date) %>% 
            select(Age = age, 
                   `Height (cm)` = Height_cms, 
                   `Reach (cm)` = Reach_cms, 
                   `Weight (cm)` = Weight_lbs) %>% 
            pivot_longer(1:4,
                         names_to = "Attribute",
                         values_to = "Value") %>% 
            select(Value)
    }, colnames = FALSE, width = "100%", align = "c", digits = 0)
    
    output$opponent_attributes <- renderTable({
        tidy_df %>% 
            filter(fighter == input$opponent_card_selection) %>% 
            slice_max(date) %>% 
            select(Age = age, 
                   `Height (cm)` = Height_cms, 
                   `Reach (cm)` = Reach_cms, 
                   `Weight (cm)` = Weight_lbs) %>% 
            pivot_longer(1:4,
                         names_to = "Attribute",
                         values_to = "Value") %>% 
            select(Value)
    }, colnames = FALSE, width = "100%", align = "c", digits = 0)
    
    output$attributes <- renderTable({
        tibble(Attribute = c("Age",
                             "Height (cm)",
                             "Reach (cm)",
                             "Weight (lbs)"))
    }, colnames = FALSE, width = "100%", align = "c")

    output$probability <- renderPlot({
        fighter_win_prob <- elo.run(Winner ~ fighter + opponent,
                                    k = input$in_k_value_2,
                                    data = elo_df) %>% 
            predict(newdata = tibble(fighter = input$fighter_card_selection,
                                     opponent = input$opponent_card_selection))
        
        tibble(name = factor(c("fighter", "opponent")),
               prob = c(fighter_win_prob, 1-fighter_win_prob)) %>% 
            ggplot(aes(x = "", y = prob, fill = fct_rev(name))) +
            geom_col(width = 1, position = "fill", show.legend = FALSE) +
            coord_flip() +
            scale_fill_manual(values = c("indianred", "steelblue")) +
            labs(x = NULL, y = "Winning Probability") +
            scale_x_discrete(breaks = NULL) +
            scale_y_continuous(labels = scales::percent, minor_breaks = NULL) +
            theme(axis.ticks.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  plot.background = element_rect(fill= "white",
                                                 color = "white"),
                  panel.background = element_rect(fill= "white"),
                  text = element_text(size = 18),
                  axis.title.x = element_text(size = 16))
    }, height = 100)

    output$fighter_win <- renderValueBox({
        fighter_win_prob <- elo.run(Winner ~ fighter + opponent,
                                    k = input$in_k_value_2,
                                    data = elo_df) %>% 
            predict(newdata = tibble(fighter = input$fighter_card_selection,
                                     opponent = input$opponent_card_selection))
        
        valueBox(value = paste0(round(fighter_win_prob*100, 1),"%"), 
                 subtitle = input$fighter_card_selection,
                 color = "light-blue",
                 icon = icon("hand-rock"))
    })
    
    output$opponent_win <- renderValueBox({
        fighter_win_prob <- elo.run(Winner ~ fighter + opponent,
                                    k = input$in_k_value_2,
                                    data = elo_df) %>% 
            predict(newdata = tibble(fighter = input$fighter_card_selection,
                                     opponent = input$opponent_card_selection))
        
        valueBox(value = paste0(round((1 - fighter_win_prob)*100, 1),"%"), 
                 subtitle = input$opponent_card_selection,
                 color = "red",
                 icon = icon("hand-rock"))
    })
            
 
       
    tidy_elo_history <- reactive({
        elo_history <- elo_df %>% 
            elo.run(Winner ~ fighter + opponent, data = ., k = input$in_k_value) %>% 
            as_tibble() %>% 
            rename("fighter" = 1, "opponent" = 2, 
                   "fighter_prob" = 3, "fighter_wins" = 4, 
                   "fighter_elo_change" = 5, "opponent_elo_change" = 6, 
                   "fighter_elo" = 7, "opponent_elo" = 8) %>% 
            mutate(match_id = elo_df$match_id,
                   weight_class = elo_df$weight_class,
                   date = elo_df$date) %>% 
            arrange(weight_class, match_id)
        
        fighter_elo_history <- elo_history %>% 
            select(fighter, match_id, date, weight_class, 
                   fighter_prob, fighter_wins, fighter_elo_change, fighter_elo)
        opponent_elo_history <- elo_history %>% 
            select(opponent, match_id, date, weight_class, 
                   fighter_prob, fighter_wins, opponent_elo_change, opponent_elo) %>% 
            mutate(fighter_prob = 1 - fighter_prob,
                   fighter_wins = 1 - fighter_wins) %>% 
            rename_all(.funs = function(x) str_replace(x, "opponent", "fighter"))
        
        bind_rows(fighter_elo_history, opponent_elo_history)
        
    })
    
    tidy_elo_history_2 <- reactive({
        elo_history <- elo_df %>% 
            elo.run(Winner ~ fighter + opponent, data = ., k = input$in_k_value_2) %>% 
            as_tibble() %>% 
            rename("fighter" = 1, "opponent" = 2, 
                   "fighter_prob" = 3, "fighter_wins" = 4, 
                   "fighter_elo_change" = 5, "opponent_elo_change" = 6, 
                   "fighter_elo" = 7, "opponent_elo" = 8) %>% 
            mutate(match_id = elo_df$match_id,
                   weight_class = elo_df$weight_class,
                   date = elo_df$date) %>% 
            arrange(weight_class, match_id)
        
        fighter_elo_history <- elo_history %>% 
            select(fighter, match_id, date, weight_class, 
                   fighter_prob, fighter_wins, fighter_elo_change, fighter_elo)
        opponent_elo_history <- elo_history %>% 
            select(opponent, match_id, date, weight_class, 
                   fighter_prob, fighter_wins, opponent_elo_change, opponent_elo) %>% 
            mutate(fighter_prob = 1 - fighter_prob,
                   fighter_wins = 1 - fighter_wins) %>% 
            rename_all(.funs = function(x) str_replace(x, "opponent", "fighter"))
        
        bind_rows(fighter_elo_history, opponent_elo_history)
        
    })

})

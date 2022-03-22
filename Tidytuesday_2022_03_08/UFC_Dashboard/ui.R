library(shiny)
library(tidyverse)
library(elo)
library(shinydashboard)

elo_df <- readRDS("../r_objects/elo_df.rds")
tidy_df <- readRDS("../r_objects/tidy_ufc.rds")

shinyUI(dashboardPage(
    dashboardHeader(title = "UFC Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(text = "Weight Class",
                     tabName = "weight_class_tab",
                     icon = icon("circle")),
            menuItem(text = "Head To Head",
                     tabName = "head_to_head_tab",
                     icon = icon("circle"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "weight_class_tab",
                fluidRow(
                    box(sliderInput(inputId = "in_k_value",
                                    label = "Select k factor which affects the ELO scores of fighters",
                                    min = 1, max = 101, step = 10, 
                                    value = 81, ticks = TRUE),
                        height = "100px"),
                    box(selectInput(inputId = "in_weight_class",
                                    label = "Select weight class",
                                    choices = elo_df %>% 
                                        select(weight_class) %>% 
                                        distinct() %>% 
                                        arrange(weight_class) %>% 
                                        pull(weight_class)),
                        height = "100px")
                ),
                fluidRow(
                    box(title = "Most recently updated ELO scores distribution in the selected class",
                        plotOutput("out_elo_histogram"),
                        height = "470px"),
                    box(title = "Top 10 UFC fighters with the selected weight class in the history",
                        tableOutput("out_top_ranking"),
                        height = "470px")
                ),
                fluidRow(column(width = 6, 
                                offset = 3,
                                box(uiOutput("out_fighter_selection"),
                                    width = 12))),
                fluidRow(box(title = "UFC performance history",
                             plotOutput("out_fighter_performance"),
                             width = 12, height = "670px"))
            ),
            tabItem(
                tabName = "head_to_head_tab",
                fluidRow(
                    box(sliderInput(inputId = "in_k_value_2",
                                    label = "Select k factor which affects the ELO scores of fighters",
                                    min = 1, max = 101, step = 10, 
                                    value = 81, ticks = TRUE),
                        height = "100px"),
                    box(selectInput(inputId = "in_weight_class_2",
                                    label = "Select weight class",
                                    choices = elo_df %>% 
                                        select(weight_class) %>% 
                                        distinct() %>% 
                                        arrange(weight_class) %>% 
                                        pull(weight_class)),
                        height = "100px")
                ),
                br(),
                fluidRow(
                    box(uiOutput("out_fighter_card"),
                        width = 6,
                        height = 100),
                    box(uiOutput("out_opponent_card"),
                         width = 6,
                         height = 100)
                ),
                br(),
                fluidRow(
                    box(valueBoxOutput("fighter_win", width = "100%"),
                        background = "light-blue", 
                        width = 4, height = 120),
                    box(plotOutput("probability", height = 100),
                        width = 4, height = 120),
                    box(valueBoxOutput("opponent_win", width = "100%"),
                        background = "red", 
                        width = 4, height = 120)
                ),
                fluidRow(
                    box(tableOutput("fighter_attributes"),
                        width = 4),
                    box(tableOutput("attributes"), 
                        width = 4),
                    box(tableOutput("opponent_attributes"),
                        width = 4)
                )
            )
        )
    )
))
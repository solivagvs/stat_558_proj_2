library(shiny)
library(tidyverse)
library(DT)

source("helper.R")

# Define UI for application ---------------------------------------------------
ui <- fluidPage(

    # Application title
    titlePanel("NFL (2010 - 2025) Data"),

    sidebarLayout(
        sidebarPanel(
            # Allows user to indicate multiple home team options
            selectizeInput(
                inputId = "home_team",
                label = "Choose home team(s):",
                choices = team_names,
                multiple = TRUE
            ),
            
            # Allows user to indicate multiple away team options
            selectizeInput(
                inputId = "away_team",
                label = "Choose away team(s):",
                choices = team_names,
                multiple = TRUE
            ),
            
            # Allows user to indicate a range of home team scores
            sliderInput(
                inputId = "home_score",
                label = "Provide a range of scores for the home team:",
                min = 0,
                max = 100,
                step = 1,
                value = c(0, 10)
            ),
            
            # Allows user to indicate a range of away team scores
            sliderInput(
                inputId = "away_score",
                label = "Provide a range of scores for the away team:",
                min = 0,
                max = 100,
                step = 1,
                value = c(0, 10)
            ),
            
            actionButton(
                inputId = "fetch",
                label = "Search"
            )
        ),

        mainPanel(
           DT::DTOutput("game_list")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    football_subset <- reactive({
        req(input$home_team | input$away_team)
        filter((HomeTeam %in% input$home_team) &
                   (AwayTeam %in% input$away_team))
    })
    
    output$game_list <- renderText({
        football |> filter(
            (HomeTeam %in% input$home_team) &
            (AwayTeam %in% input$away_team) &
            ((HomeScore < input$home_score[2] + 1) &
                 (HomeScore > input$home_score[1] - 1)) &
            ((AwayScore < input$away_score[2] + 1) &
                 (AwayScore > input$away_score[1] - 1))
            )

        })
}

# Run the application 
shinyApp(ui = ui, server = server)

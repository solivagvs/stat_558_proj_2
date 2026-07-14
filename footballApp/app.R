library(shiny)
library(tidyverse)
library(DT)
library(shinyjs)

source("helper.R")

# Define UI for application ---------------------------------------------------
ui <- fluidPage(
    # initializes shinyjs
    useShinyjs(),

    # Application title
    titlePanel("NFL (2010 - 2025) Data"),

    sidebarLayout(
        sidebarPanel(
            id = "sidebar_content",
            
            # Allows user to indicate multiple home team options
            selectizeInput(
                inputId = "home_team",
                label = "Choose home team(s):",
                choices = c("All", team_names),
                multiple = TRUE
            ),
            
            # Allows user to indicate multiple away team options
            selectizeInput(
                inputId = "away_team",
                label = "Choose away team(s):",
                choices = c("All", team_names),
                multiple = TRUE
            ),
            
            # Allows user to indicate a range of home team scores
            sliderInput(
                inputId = "home_score",
                label = "Provide a range of scores for the home team:",
                min = 0,
                max = 100,
                step = 1,
                value = c(0, 0)
            ),
            
            # Allows user to indicate a range of away team scores
            sliderInput(
                inputId = "away_score",
                label = "Provide a range of scores for the away team:",
                min = 0,
                max = 100,
                step = 1,
                value = c(0, 0)
                ),
            
            actionButton(
                inputId = "fetch",
                label = "Search"
                ),
            
            # adds a reset action button, later using shinyjs library
            actionButton(inputId = "reset",
                         label = "Reset Choices",
                         class = "btn-danger"
                         )
        ),

        mainPanel(
        tabsetPanel(
            tabPanel("About",
                     p("The purpose of this app is to..."),
                     p("The data"),
                     p("The side bar allows you to"),
                     tags$img(src = "nfl_logo.svg",
                              width = "25%",
                              height = "auto")
                     ),
            
            tabPanel("Data Download",
                     tableOutput(outputId = "subset_table")
                     ),
            
            tabPanel("Data Exploration", "contents",
                     dataTableOutput(outputId = "filtered_data")
                     )
        )
        )
    )
)

# Define server logic ---------------------------------------------------------
server <- function(input, output, session) {

    # resets choices
    observeEvent(input$reset, {
        shinyjs::reset("sidebar_content")
    })
    
    football_subset <- reactive({
        req(input$home_team | input$away_team)

    })
    
    output$filtered_data <- renderDataTable({
        req(input$home_team, input$away_team, input$home_score,
            input$away_score)
        
        if (input$home_team == "All" & input$away_team == "All"){
        football |> filter(
#            (HomeTeam %in% input$home_team) &
#                (AwayTeam %in% input$away_team) &
                ((HomeScore < input$home_score[2] + 1) &
                     (HomeScore > input$home_score[1] - 1)) &
                ((AwayScore < input$away_score[2] + 1) &
                     (AwayScore > input$away_score[1] - 1))
        )
        } else if (input$home_team == "All" & input$away_team != "All"){
            football |> filter(
#            (HomeTeam %in% input$home_team) &
                (AwayTeam %in% input$away_team) &
                ((HomeScore < input$home_score[2] + 1) &
                     (HomeScore > input$home_score[1] - 1)) &
                ((AwayScore < input$away_score[2] + 1) &
                     (AwayScore > input$away_score[1] - 1))
            )
        } else if (input$home_team != "All" & input$away_team == "All"){
            football |> filter(
            (HomeTeam %in% input$home_team) &
#                (AwayTeam %in% input$away_team) &
                ((HomeScore < input$home_score[2] + 1) &
                     (HomeScore > input$home_score[1] - 1)) &
                ((AwayScore < input$away_score[2] + 1) &
                     (AwayScore > input$away_score[1] - 1))
            )} else {
                football |> filter(
                    (HomeTeam %in% input$home_team) &
                    (AwayTeam %in% input$away_team) &
                    ((HomeScore < input$home_score[2] + 1) &
                         (HomeScore > input$home_score[1] - 1)) &
                    ((AwayScore < input$away_score[2] + 1) &
                        (AwayScore > input$away_score[1] - 1))
                )
        }
    }) |>
        bindEvent(input$fetch)
    
    output
}

# Run the application ---------------------------------------------------------
shinyApp(ui = ui, server = server)
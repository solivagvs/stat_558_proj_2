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
    titlePanel("NFL (2010 - 2025) Team Data"),

    sidebarLayout(
        sidebarPanel(
            id = "sidebar_content",
            
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
                max = 75,
                step = 1,
                value = c(0, 75)
            ),
            
            # Allows user to indicate a range of away team scores
            sliderInput(
                inputId = "away_score",
                label = "Provide a range of scores for the away team:",
                min = 0,
                max = 75,
                step = 1,
                value = c(0, 75)
                ),
            
            actionButton(
                inputId = "fetch",
                label = "Search",
                class = c("btn-block", "btn-success")
                ),
            
            br(),
            
            # adds a reset action button, later using shinyjs library
            actionButton(inputId = "reset",
                         label = "Reset Choices",
                         class = c("btn-danger", "btn-sm")
                         )
        ),

        mainPanel(
        tabsetPanel(
            tabPanel("About",
                     
                     p("The purpose of this app is to explore data concerning the United States National Football League (NFL) for the years 2010 - 2025."),
                     
                     p("The data was obtained from a repository located at https://www.kaggle.com/datasets/keonim/nfl-game-scores-dataset-2017-2023"),
                     
                     p("The side bar allows you to make selections for one or more home teams, away teams, and the ranges of scores for each. This will pool all the data satisfying ALL of the choices made. For example, selecting 'Atlanta' at Home and 'Cleveland' as Away will only subset those games for which both those conditions are true."),
                     
                     p("The Data Download tab will both display all games chosen and offer you the ability to download it as a comma-separated file or a tab-delimited file."),
                     
                     p("The Data Exploration tab will allow you to view statistical analyses of those games selected using the left menu."),
                     
                     tags$img(src = "nfl_logo.svg",
                              width = "25%",
                              height = "auto")
                     ),
            
            tabPanel("Data Download",
                     
                     p("After making selections on the side bar and clicking 'Search,' you may also download the data by selecting filetype, then clicking 'Download data'."),
                     
                     br(),
                     br(),
                     
                     radioButtons(inputId = "filetype",
                                  label = "Select filetype:",
                                  choices = c("csv", "tsv"),
                                  selected = "csv"),
                     
                     downloadButton(
                         outputId = "download_data",
                         label = "Download data"),
                     
                     br(),
                     br(),
                     
                     dataTableOutput(outputId = "filtered_data")
                    ),
            
            tabPanel("Data Exploration",
                     
                     p("This panel displays a scatterplot of home and away scores for each of the games chosen from the side panel."),
                     
                     plotOutput(
                         outputId = "nfl_geom"
                     ),
                     
                     hr(),
                     
                     p("This panel displays the statistics for each of the teams when they were a home team."),
                     
                     verbatimTextOutput(
                         outputId = "nfl_stats_home"
                     ),
                     
                     hr(),
                     
                     p("This panel displays the statistics for each of the teams when they were an away team."),
                     
                     verbatimTextOutput(
                         outputId = "nfl_stats_away"
                     )
                     )
        )
        )
    )
)

# Define server logic ---------------------------------------------------------
server <- function(input, output, session) {

# resets choices ----------------------
    observeEvent(input$reset, {
        shinyjs::reset("sidebar_content")
    })

# Subsets the data from side bar ------
    football_subset <- eventReactive(input$fetch, {
        req(input$home_team, input$away_team, input$home_score,
            input$away_score)
        
        football |> filter(
            (HomeTeam %in% input$home_team) &
                (AwayTeam %in% input$away_team) &
                ((HomeScore < input$home_score[2] + 1) &
                     (HomeScore > input$home_score[1] - 1)) &
                ((AwayScore < input$away_score[2] + 1) &
                     (AwayScore > input$away_score[1] - 1))
            )
    })

# Data Download Tab -------------------
# Download button ---------------------
    output$download_data <- downloadHandler(
        filename = function() {
            paste0("nflgames.", input$filetype)
        },
        content = function(file) {
            if(input$filetype == "csv"){
                write_csv(football_subset(), file)
            }
            if(input$filetype == "tsv"){
                write_tsv(football_subset(), file)
            }
        }
    )

# Display data ------------------------            
    output$filtered_data <- renderDataTable({
        football_subset()
    })
    
# Data Exploration Tab ----------------
# scatterplot of home vs away scores
    output$nfl_geom <- renderPlot({
        ggplot(data = football_subset(),
               mapping = aes(x = HomeScore,
                             y = AwayScore)) +
            geom_point(position = "jitter") +
            labs(x = "Home Score", y = "Away Score",
                 title = "Home vs. Away Scores"
                 ) +
            coord_cartesian(xlim = c(0, 75),
                            ylim = c(0, 75))
    })

# stats for home teams grouped by team    
    output$nfl_stats_home <- renderPrint({
        football_subset() |> group_by("Team" = HomeTeam) |>
            summarize(
                min = min(c(HomeScore, AwayScore)),
                quartile1 = quantile(c(HomeScore, AwayScore), 0.25),
                median = median(c(HomeScore, AwayScore)),
                quartile3 = quantile(c(HomeScore, AwayScore), 0.75),
                max = max(c(HomeScore, AwayScore)),
                IQR = quartile3 - quartile1,
                mean = mean(c(HomeScore, AwayScore)),
                sd = sd(c(HomeScore, AwayScore))
            ) |>
            print(n = Inf)
    })

# stats for away teams grouped by team
    output$nfl_stats_away <- renderPrint({
        football_subset() |> group_by("Team" = AwayTeam) |>
            summarize(
                min = min(c(HomeScore, AwayScore)),
                quartile1 = quantile(c(HomeScore, AwayScore), 0.25),
                median = median(c(HomeScore, AwayScore)),
                quartile3 = quantile(c(HomeScore, AwayScore), 0.75),
                max = max(c(HomeScore, AwayScore)),
                IQR = quartile3 - quartile1,
                mean = mean(c(HomeScore, AwayScore)),
                sd = sd(c(HomeScore, AwayScore))
            ) |>
            print(n = Inf)
    })
    
}

# Run the application ---------------------------------------------------------
shinyApp(ui = ui, server = server)
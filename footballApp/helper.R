# loads in data and prepares it -----------------------------------------------
football <- read_csv("data/2010-2025_scores.csv",
                     show_col_types = FALSE)
football <- drop_na(football)
football <- football |> select(-GameStatus)

# Changes JAC to JAX ----------------------------------------------------------
for (i in 1:length(football$HomeTeam)){
  if (football$HomeTeam[i] == "JAC"){
    football$HomeTeam[i] <- "JAX"
  }
  
  if (football$AwayTeam[i] == "JAC"){
    football$AwayTeam[i] <- "JAX"
  }
}

# adds a column of winning team and removes ties and NA data ------------------
football <- add_column(football, "Winner" = "")

for (i in 1:length(football$Winner)){
  if (football$AwayScore[i] > football$HomeScore[i]){
    football$Winner[i] = football$AwayTeam[i]
  } else if (football$AwayScore[i] == football$HomeScore[i]){
    football$Winner[i] = NA
  } else {
    football$Winner[i] = football$HomeTeam[i]
  }
}
football <- drop_na(football)

# converts month/ordinal day column to date -----------------------------------
football <- football |>
  mutate(GameDate = paste(GameDate, Season))

football$GameDate <- mdy(football$GameDate)

# sets factors ----------------------------------------------------------------
football$Season <- as.factor(football$Season)
football$Week <- as.factor(football$Week)
football$GameSlot <- as.factor(football$GameSlot)
football$AwayTeam <- as.factor(football$AwayTeam)
football$HomeTeam <- as.factor(football$HomeTeam)
football$Winner <- as.factor(football$Winner)

# creates data frame with specific teams' counts ------------------------------
winner_counts <- football |> group_by(Winner) |>
  summarize(count = n())

home_counts <- football |> group_by(HomeTeam) |>
  summarize(count = n())

home_winner_counts <- football |>
  group_by(HomeTeam)|>
  summarize(count = sum(HomeTeam == Winner))

away_counts <- football |> group_by(AwayTeam) |>
  summarize(count = n())

total_game_counts <- data.frame(Team = away_counts[[1]],
                                count = away_counts[[2]] +
                                  home_counts[[2]])

rm(away_counts)

team_win_home <- data.frame(Team = as.character(home_counts[[1]]),
                            Wins = winner_counts[[2]],
                            HomeGames = home_counts[[2]],
                            HomeWinGames = home_winner_counts[[2]],
                            TotalGames = total_game_counts[[2]],
                            row.names = NULL)

team_win_home <- as_tibble(team_win_home)


# defines pretty names for app ------------------------------------------------
team_names <- c("Arizona" = "ARI",
                "Atlanta" = "ATL",
                "Baltimore" = "BAL",
                "Buffalo" = "BUF",
                "Carolina" = "CAR",
                "Chicago" = "CHI",
                "Cincinnati" = "CIN",
                "Cleveland" = "CLE",
                "Dallas" = "DAL",
                "Denver" = "DEN",
                "Detroit" = "DET",
                "Green Bay" = "GB",
                "Houston" = "HOU",
                "Indianapolis" = "IND",
                "Jacksonville" = "JAX",
                "Kansas City" = "KC",
                "LA Chargers" = "LAC",
                "LA Raiders" = "LAR",
                "Las Vegas" = "LV",
                "Miami" = "MIA",
                "Minnesota" = "MIN",
                "New England" = "NE",
                "New Orleans" = "NO",
                "NY Giants" = "NYG",
                "NY Giants" = "NYJ",
                "Oakland" = "OAK",
                "Philadelphia" = "PHI",
                "Pittsburgh" = "PIT",
                "San Diego" = "SD",
                "Seattle" = "SEA",
                "San Francisco" = "SF",
                "St. Louis" = "STL",
                "Tampa Bay" = "TB",
                "Tennessee" = "TEN",
                "Washington" = "WAS")
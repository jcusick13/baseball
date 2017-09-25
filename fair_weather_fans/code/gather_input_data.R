# gather_input_data.R
library(tidyverse)
library(stringr)
library(rvest)

# 1. 2016 attendance counts -----------

# Download raw data from Retrosheet
f <- "http://www.retrosheet.org/gamelogs/gl2016.zip"
download.file(f, "data/gl2016.zip")
unzip(zipfile = "data/gl2016.zip", exdir = "data/")

game_log <- read_csv("data/GL2016.TXT", col_names = FALSE) %>%
    select(Date = X1, DayofWeek = X3, Visitor = X4, VisitorGameNum = X6, 
           Home = X7, HomeGameNum = X9, VisitorScore = X10, HomeScore = X11,
           DayNight = X13, Attendance = X18)


# 2. Map 2015/2016 All Star rosters to teams ---------

get_allstar_id <- function(year) {
    # Reads Baseball Reference all star game roster file 
    # for the given year (character) and returns a tibble 
    # of Player Name, Baseball Reference ID, First Letter
    # of ID
    
    in_data <- c(paste0("data/allstar_al_", year, ".txt"),
                 paste0("data/allstar_nl_", year, ".txt"))
    raw_table <- lapply(in_data, read_csv)
    
    allstar_id <- bind_rows(raw_table) %>%
        select(Batting) %>%
        separate(col = Batting, into = c("Name", "ID"), sep = "\\\\") %>%
        filter(!is.na(ID))
}

add_team_abbr <- function(df) {
    # Reads a data frame containing a column named ID,
    # Baseball Reference IDs, returns that same df
    # with a new col, Team, denoting the team abbreviation
    # of that player.
    
    # Add temp col used for url string, blank col for Team
    df <- mutate(df, l = substr(ID, 0, 1)) %>% mutate(Team = NA)
    
    for (i in 1:nrow(df)) {
        url <- paste0("https://www.baseball-reference.com/players/", df$l[i], "/",
                      df$ID[i], ".shtml")
        webpage <- read_html(url)
        # Scrape team weblink from player's BBRef bio
        team_desc <- html_nodes(webpage, xpath = '//*[@id="meta"]/div[2]/p[4]/a') %>%
            html_attr("href") %>%
            str_split("/", simplify = TRUE)
        df[i, "Team"] <- team_desc[3]
    }
    
    # Remove temp col from df
    select(df, -l)
}

# Count all stars by team
allstar_2015_ct <- get_allstar_id("2015") %>% 
    add_team_abbr() %>%
    group_by(Team) %>%
    summarize(Count = n())

allstar_2016_ct <- get_allstar_id("2016") %>% 
    add_team_abbr() %>%
    group_by(Team) %>%
    summarize(Count = n())


# 3. Combine all star counts with attendance -------

# Counts for home team
game_data <-
    left_join(x = game_log, y = allstar_2016_ct, by = c("Home" = "Team")) %>%
    mutate(AllStarCt_Home = ifelse(is.na(Count), 0, Count)) %>%
    select(-Count)

# Add away team
game_data <-
    left_join(x = game_data, y = allstar_2016_ct, by = c("Visitor" = "Team")) %>%
    mutate(AllStarCt_Away = ifelse(is.na(Count), 0, Count)) %>%
    select(-Count)


# 4. Tally running win percentage ------

# Create new table to store data
running_wins <- tibble(GameNum = rep(NA, 2 * nrow(game_data)), 
                       Team = rep(NA, 2 * nrow(game_data)),
                       Wins = rep(NA, 2 * nrow(game_data)),
                       WinPct = rep(NA, 2 * nrow(game_data)))
           

update_home_wins <- function(df_wins, df_game, row) {
    # Updates df_wins table with the knowledge that the home team 
    # won the game in df_game[row]. Will add data to two new rows in
    # df_wins and return the edited table.
    
        # Update output table for the winning home team
        df_wins$GameNum[row] <- df_game$HomeGameNum[row]
        df_wins$Team[row] <- df_game$Home[row]
        
        if (df_game$HomeGameNum[row] == 1) {
            df_wins$Wins[row] <- 1
        }
        else {
            prev_game_tibble <- filter(df_wins, Team == df_wins$Team[row] &
                                                GameNum == df_wins$GameNum[row] - 1)
            df_wins$Wins[row] <- prev_game_tibble$Wins
        }
        df_wins$WinPct[row] <- df_wins$Wins[row] / df_wins$GameNum[row]
        
        # Update output table for the losing visiting team
        df_wins$GameNum[row + 1] <- df_game$VisitorGameNum[row]
        df_wins$Team[row + 1] <- df_game$Visitor[row]
        
        if (df_game$VisitorGameNum[row] == 1) {
            df_wins$Wins[row + 1] <- 0
        }
        else {
            prev_game_tibble <- filter(df_wins, Team == df_wins$Team[row + 1] &
                                                GameNum == df_wins$GameNum[row + 1] - 1)
            df_wins$Wins[row + 1] <- prev_game_tibble$Wins
        }
        df_wins$WinPct[row + 1] <- df_wins$Wins[row + 1] / df_wins$Wins[row + 1]
        
        # Return final table
        df_wins
}

update_visitor_wins <- function(df_wins, df_game, row) {
    # Updates df_wins table with the knowledge that the visiting team 
    # won the game in df_game[row]. Will add data to two new rows in
    # df_wins and return the edited table.
    
    # Update output table for the winning visiting team
    df_wins$GameNum[row] <- df_game$VisitorGameNum[row]
    df_wins$Team[row] <- df_game$Visitor[row]
    
    if (df_game$VisitorGameNum[row] == 1) {
        df_wins$Wins[row] <- 1
    }
    else {
        prev_game_tibble <- filter(df_wins, Team == df_wins$Team[row] &
                                       GameNum == df_wins$GameNum[row] - 1)
        df_wins$Wins[row] <- prev_game_tibble$Wins
    }
    df_wins$WinPct[row] <- df_wins$Wins[row] / df_wins$GameNum[row]
    
    # Update output table for the losing visiting team
    df_wins$GameNum[row + 1] <- df_game$HomeGameNum[row]
    df_wins$Team[row + 1] <- df_game$Home[row]
    
    if (df_game$HomeGameNum[row] == 1) {
        df_wins$Wins[row + 1] <- 0
    }
    else {
        prev_game_tibble <- filter(df_wins, Team == df_wins$Team[row + 1] &
                                       GameNum == df_wins$GameNum[row + 1] - 1)
        df_wins$Wins[row + 1] <- prev_game_tibble$Wins
    }
    df_wins$WinPct[row + 1] <- df_wins$Wins[row + 1] / df_wins$Wins[row + 1]
    
    # Return final table
    df_wins
}


for (i in 1:10) {
    if (game_data$HomeScore[i] > game_data$VisitorScore[i]) {
        running_wins <- update_home_wins(running_wins, game_data, i)
    }
    else {
        running_wins <- update_visitor_wins(running_wins, game_data, i)
    }
}
    
running_wins <- update_home_wins(running_wins, game_data, 1)

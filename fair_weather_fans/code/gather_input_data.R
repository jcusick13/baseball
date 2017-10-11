# gather_input_data.R
library(tidyverse)
library(stringr)
library(rvest)

#
# 1. 2016 attendance counts ----------
#

# Download raw data from Retrosheet
f <- "http://www.retrosheet.org/gamelogs/gl2016.zip"
download.file(f, "data/gl2016.zip")
unzip(zipfile = "data/gl2016.zip", exdir = "data/")

game_log <- read_csv("data/GL2016.TXT", col_names = FALSE) %>%
    select(Date = X1, DayofWeek = X3, Visitor = X4, VisitorGameNum = X6, 
           Home = X7, HomeGameNum = X9, VisitorScore = X10, HomeScore = X11,
           DayNight = X13, Attendance = X18)


#
# 2. Map 2015/2016 All Star rosters to teams ----------
#

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


#
# 3. Tally running win percentage ----------
#

# Create new table to store data
running_wins <- tibble(GameNum = rep(NA, 2 * nrow(game_log)), 
                       Team = rep(NA, 2 * nrow(game_log)),
                       Wins = rep(NA, 2 * nrow(game_log)),
                       WinPct = rep(NA, 2 * nrow(game_log)))
           

update_home_wins <- function(wins_tbl, game_data, row) {
    # Updates wins_tbl table with the knowledge that the home team 
    # won the game in game_data[row]. Will add data to two new rows in
    # wins_tbl and return the edited table.
    
    # Update output table for the winning home team
    wins_tbl$GameNum[2 * row - 1] <- game_data$HomeGameNum[row]
    wins_tbl$Team[2 * row - 1] <- game_data$Home[row]
    
    if (game_data$HomeGameNum[row] == 1) {
        wins_tbl$Wins[2 * row - 1] <- 1
    }
    else {
        prev_game_tibble <- filter(wins_tbl, Team == wins_tbl$Team[2 * row - 1] &
                                       GameNum == wins_tbl$GameNum[2 * row - 1] - 1)
        wins_tbl$Wins[2 * row - 1] <- prev_game_tibble$Wins + 1
    }
    wins_tbl$WinPct[2 * row - 1] <- wins_tbl$Wins[2 * row - 1] / wins_tbl$GameNum[2 * row - 1]
    
    # Update output table for the losing visiting team
    wins_tbl$GameNum[2 * row] <- game_data$VisitorGameNum[row]
    wins_tbl$Team[2 * row] <- game_data$Visitor[row]
    
    if (game_data$VisitorGameNum[row] == 1) {
        wins_tbl$Wins[2 * row] <- 0
    }
    else {
        prev_game_tibble <- filter(wins_tbl, Team == wins_tbl$Team[2 * row] &
                                       GameNum == wins_tbl$GameNum[2 * row] - 1)
        wins_tbl$Wins[2 * row] <- prev_game_tibble$Wins
    }
    wins_tbl$WinPct[2 * row] <- wins_tbl$Wins[2 * row] / wins_tbl$GameNum[2 * row]
    
    # Return final table
    wins_tbl
}

update_visitor_wins <- function(wins_tbl, game_data, row) {
    # Updates wins_tbl table with the knowledge that the visiting team 
    # won the game in game_data[row]. Will add data to two new rows in
    # wins_tbl and return the edited table.
    
    # Update output table for the winning visiting team
    wins_tbl$GameNum[2 * row - 1] <- game_data$VisitorGameNum[row]
    wins_tbl$Team[2 * row - 1] <- game_data$Visitor[row]
    
    if (game_data$VisitorGameNum[row] == 1) {
        wins_tbl$Wins[2 * row - 1] <- 1
    }
    else {
        prev_game_tibble <- filter(wins_tbl, Team == wins_tbl$Team[2 * row - 1] &
                                       GameNum == wins_tbl$GameNum[2 * row - 1] - 1)
        wins_tbl$Wins[2 * row - 1] <- prev_game_tibble$Wins + 1
    }
    wins_tbl$WinPct[2 * row - 1] <- wins_tbl$Wins[2 * row - 1] / wins_tbl$GameNum[2 * row - 1]
    
    # Update output table for the losing visiting team
    wins_tbl$GameNum[2 * row] <- game_data$HomeGameNum[row]
    wins_tbl$Team[2 * row] <- game_data$Home[row]
    
    if (game_data$HomeGameNum[row] == 1) {
        wins_tbl$Wins[2 * row] <- 0
    }
    else {
        prev_game_tibble <- filter(wins_tbl, Team == wins_tbl$Team[2 * row] &
                                       GameNum == wins_tbl$GameNum[2 * row] - 1)
        wins_tbl$Wins[2 * row] <- prev_game_tibble$Wins
    }
    wins_tbl$WinPct[2 * row] <- wins_tbl$Wins[2 * row] / wins_tbl$GameNum[2 * row]
    
    # Return final table
    wins_tbl
}

# Build table of running win percentage across season
for (i in 1:nrow(game_log)) {
    if (game_log$HomeScore[i] > game_log$VisitorScore[i]) {
        running_wins <- update_home_wins(running_wins, game_log, i)
    }
    else {
        running_wins <- update_visitor_wins(running_wins, game_log, i)
    }
}

# Increase the game counts by 1 so they represent the win percentage
# going into that game number
running_wins$GameNum <- running_wins$GameNum + 1


#
# 4. Create final table for analysis ----------
# 

# Add all star counts for home team
game_data <-
    left_join(x = game_log, y = allstar_2016_ct, by = c("Home" = "Team")) %>%
    mutate(HomeAllStar = ifelse(is.na(Count), 0, Count)) %>%
    select(-Count)

# Add all star counts for away team
game_data <-
    left_join(x = game_data, y = allstar_2016_ct, by = c("Visitor" = "Team")) %>%
    mutate(VisitorAllStar = ifelse(is.na(Count), 0, Count)) %>%
    select(-Count)

# Add running win percentage for home team
game_data <-
    left_join(x = game_data, y = running_wins, by = c("Home" = "Team", 
                                                      "HomeGameNum" = "GameNum")) %>%
    mutate(HomeWinPct = WinPct) %>%
    select(-Wins, -WinPct)

# Add running win percentage for away team
game_data <-
    left_join(x = game_data, y = running_wins, by = c("Visitor" = "Team",
                                                      "VisitorGameNum" = "GameNum")) %>%
    mutate(VisitorWinPct = WinPct) %>%
    select(-Wins, -WinPct) %>%
    # Final column clean up
    select(-VisitorGameNum, -VisitorScore, -HomeScore) %>%
    replace_na(list(HomeWinPct = 0, VisitorWinPct = 0))

# Save dataset for later analysis
write_csv(game_data, "./data/game_data.csv")

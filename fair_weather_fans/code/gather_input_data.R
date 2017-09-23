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

allstar_2015 <- get_allstar_id("2015") %>% add_team_abbr()


    





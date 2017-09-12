# gather_input_data.R
library(tidyverse)

# 1. 2016 attendance counts -----------

# Download raw data from Retrosheet
f <- "http://www.retrosheet.org/gamelogs/gl2016.zip"
download.file(f, "data/gl2016.zip")
unzip(zipfile = "data/gl2016.zip", exdir = "data/")

game_log <- read_csv("data/GL2016.TXT", col_names = FALSE) %>%
    select(Date = X1, DayofWeek = X3, Visitor = X4, VisitorGameNum = X6, 
           Home = X7, HomeGameNum = X9, VisitorScore = X10, HomeScore = X11,
           DayNight = X13, Attendance = X18)


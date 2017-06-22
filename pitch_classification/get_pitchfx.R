# get_pitchfx.R
library(pitchRx)
library(readr)
library(stringr)
library(dplyr)
library(lubridate)

# 1. Find games Rick Porcello pitched in ---------------------------

# Read retrosheet play by play data, subset to Porcello
fields <- read_csv("fields.csv")
porcello <- 
    read_csv("all2016.csv", col_names = fields$Header, 
             cols(BAT_PLAY_TX = col_character(),
                  RUN1_PLAY_TX = col_character(),
                  RUN2_PLAY_TX = col_character(),
                  RUN3_PLAY_TX = col_character())) %>%
    filter(PIT_ID == "porcr001") %>%
    group_by(GAME_ID) %>%
    summarise(Inning = max(INN_CT), Outs = max(OUTS_CT + EVENT_OUTS_CT)) %>%
    mutate(Date = ymd(str_sub(GAME_ID, 4, 11)))


# loop through each row to identify top vs. bottom of inning
half <- vector(mode = "character", length = nrow(porcello))
for (i in 1:nrow(porcello)) {
    if (str_sub(porcello$GAME_ID[[i]], 0, 3) == "BOS") {
        half[[i]] <- "top"
    } else {
        half[[i]] <- "bottom"
    }
}
porcello <- mutate(porcello, Half = half)


# 2. Pull PitchFX data for the above games -----------------------
fxdata <- scrape(start = as.character(porcello$Date[1]), 
                 end = as.character(porcello$Date[1]))
porcello.fx <- 
    fxdata[["pitch"]] %>%
    filter(str_detect(gameday_link, "bos")) %>%
    filter(inning_side == porcello$Half[1] &
               inning <= porcello$Inning[1]) %>%
    select(x, y, start_speed, end_speed, sz_top, sz_bot,
           pfx_x, pfx_z, break_length, pitch_type, type_confidence)

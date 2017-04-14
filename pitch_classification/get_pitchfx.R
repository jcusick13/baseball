# get_pitchfx.R
library(pitchRx)
library(dplyr)

# Retrosheet play by play data
season16 <- read.csv("all2016.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(season16) <- fields[, "Header"]
# Pitcher of interest
porcello <- subset(season16, PIT_ID == "porcr001")

# Find dates and inning count of games pitched
por.summary <- group_by(porcello, GAME_ID) %>%
               summarise(Inning = max(INN_CT), 
                         Outs = max(OUTS_CT + EVENT_OUTS_CT))
# Convert dates to pitch fx format
por.summary$date <- substr(por.summary$GAME_ID, 4, length(por.summary$GAME_ID))
por.summary$date <- paste0(substr(por.summary$date, 0, 4), "-",
                           substr(por.summary$date, 5, 6), "-",
                           substr(por.summary$date, 7, 8))
# loop through each row for correct inning section
inning <- vector(mode = "character", length = nrow(por.summary))
for (i in 1:nrow(por.summary)) {
    if (substr(por.summary$GAME_ID[[i]], 0, 3) == "BOS") {
        inning[[i]] <- "top"
    } else {
        inning[[i]] <- "bottom"
    }
}
# Info necessary to query pitch fx data
query <- cbind(por.summary, inning)



# Get pitch fx for given day and team
dat <- scrape(start = query$date[[1]], end = query$date[[1]])
fx <- dat[["pitch"]]
bos.fx <- fx[grep("bos", fx$gameday_link),]

# Remove opposing pitcher's data
p <- subset(bos.fx, inning_side == query$inning[[1]] &
                         inning <= query$Inning[[1]])

keep.cols <- c("x", "y", "start_speed", "end_speed", "sz_top",
               "sz_bot", "pfx_x", "pfx_z", "break_length",
               "pitch_type", "type_confidence")
p <- p[, keep.cols]

# Plot by movement
plot(x = p$pfx_x, 
     y = p$pfx_z,
     main = "Rick Porcello Pitch Movement - July 29, 2016",
     xlab = "Horizontal Movement",
     ylab = "Vertical Movement",
     pch = (1:5)[as.factor(p$pitch_type)],
     col = (1:5)[as.factor(p$pitch_type)]
     )



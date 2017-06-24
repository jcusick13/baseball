# cluster_pitches.R
library(readr)
library(mclust)

# Read input data ----------------------------

abrv <- read_csv("./pitch_abbreviations.csv")
pitches <- 
    read_csv("./porcello_2016_pitches.csv") %>%
    mutate(Pitch = as.character(sapply(
        pitch_type, function(x) { abrv$Desc[abrv$Pitch == x]}))) %>%
    filter(Pitch != "Pitch Out") %>%
    filter(Pitch != "Sinker") %>%
    filter(!is.na(start_speed))

# Exploratory plotting -----------------------

# Density of pitch type by speed
 ggplot(pitches, aes(start_speed)) + 
    geom_density(aes(color = Pitch))
 
# Pitch movement by type
 ggplot(pitches, aes(x = pfx_x, y = pfx_z)) +
     geom_point(aes(color = Pitch))
 
 ggplot(pitches, aes(x = pfx_x, y = pfx_z)) + 
     geom_point(aes(color = start_speed)) +
     scale_color_gradient(low = "yellow", high = "red") + 
     facet_grid(. ~ Pitch)

 
# Cluster analysis --------------------------
 
clust1 <- Mclust(pitches[,c("start_speed", "break_y", "break_length",
                             "break_angle")])
#> Best model : ellipsoidal, equal shape (VEV) with 9 components 
 
# Record BIC from EM algorithm
clust1BIC <- mclustBIC(pitches[,c("start_speed", "break_y", "break_length",
                             "break_angle")])

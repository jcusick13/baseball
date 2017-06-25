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
speed <- ggplot(pitches, aes(start_speed)) + 
    geom_density(aes(color = Pitch)) + 
    labs(x = "Speed", y = "Density")
 
# Pitch movement by type
type <- ggplot(pitches, aes(x = pfx_x, y = pfx_z)) +
    geom_point(aes(color = Pitch)) +
    labs(x = "Horizontal Movement", y = "Vertical Movement")
 
# Speed and movement across pitch types
facet <- ggplot(pitches, aes(x = pfx_x, y = pfx_z)) + 
    geom_point(aes(color = start_speed)) +
    scale_color_gradient(low = "yellow", high = "red") + 
    facet_grid(. ~ Pitch) +
    labs(x = "Horizontal Movement", y = "Vertical Movement",
         color = "Speed")
ggsave("./images/facet.png")
 
# Cluster analysis --------------------------
p <- select(pitches, start_speed, break_y, break_length, break_angle,
            pfx_x, pfx_z, Pitch, type_confidence) 
clust <- Mclust(p[,c("start_speed", "break_y", "break_length", "break_angle")])
#> Best model : ellipsoidal, equal shape (VEV) with 9 components 
 
# Record BIC from EM algorithm
clustBIC <- mclustBIC(p[, c("start_speed", "break_y", "break_length", "break_angle")])

# Attach cluster membership/probability to each observation
p$cclass <- clust$class
cProb <- data.frame(clust$z)
p <- bind_cols(p, cProb)

# Compare cluster class to MLB class
table(p$cclass, p$Pitch)
ggplot(p, aes(x = pfx_x, y = pfx_z)) +
    geom_point(aes(color = as.character(cclass)))
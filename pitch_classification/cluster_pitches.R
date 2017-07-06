# cluster_pitches.R
library(readr)
library(tibble)
library(tidyr)
library(dplyr)
library(mclust)
library(ggplot2)

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

# Compare cluster class to MLB class, create normalized counts by cluster
classct <- 
    as.data.frame(table(p$cclass, p$Pitch)) %>%
    rename(cluster = Var1, mlb = Var2, count = Freq) %>%
    spread(mlb, count)

# Create normalized matrix of cluster counts by row
classct.norm <- data.frame(matrix(NA, nrow = nrow(classct), ncol = ncol(classct) - 1))
for (i in 1:nrow(classct)) {
    # Total pitches per cluster
    total <- sum(classct[i,-1])
    
    for (j in 2:ncol(classct)) {
        # Normalize values within each row
        classct.norm[i, j - 1] <- classct[i, j] / total
    }
}

# Combine with original class counts, reform for plotting
n <- bind_cols(classct, classct.norm) %>%
    gather(`Changeup`, `Curveball`, `Four-seam Fastball`, `Slider`, `Two-seam Fastball`,
           key = "mlb", value = "count") %>%
    gather(`X1`, `X2`, `X3`, `X4`, `X5`, key = normalized, value = "freq")

    
# Plot classification table   
ggplot(n, aes(x = mlb, y = cluster)) +
    geom_tile(aes(fill = count)) +
    scale_fill_gradient(low = "red", high = "green")

# Clusters by movement
ggplot(p, aes(x = pfx_x, y = pfx_z)) +
    geom_point(aes(color = as.character(cclass)))




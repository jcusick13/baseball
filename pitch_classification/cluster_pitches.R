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
ggsave("./images/test.png")
 
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
 
# Cluster pitches --------------------------
p <- select(pitches, start_speed, break_y, break_length, break_angle,
            pfx_x, pfx_z, Pitch, type_confidence) 
clust <- Mclust(p[,c("start_speed", "break_y", "break_length", "break_angle")])
summary(clust, parameters = TRUE)
#> Best model : ellipsoidal, equal shape (VEV) with 9 components 
 
# Record BIC from EM algorithm
clustBIC <- mclustBIC(p[, c("start_speed", "break_y", "break_length", "break_angle")])

# Cluster analysis -------------------------

# Attach cluster membership and probability to each observation
p$cclass <- clust$class
cProb <- data.frame(clust$z)
p <- bind_cols(p, cProb)

# Compare cluster class to MLB class
classes <- 
    as.data.frame(table(p$cclass, p$Pitch)) %>%
    rename(Cluster = Var1, mlb = Var2, count = Freq) %>%
    spread(mlb, count)

# There's high disagreement in cluster 7 - how many pitches are actually in that cluster?
ct7 <- filter(classes, Cluster == 7) %>%
    select(-Cluster) %>%
    rowSums()  # 42
ct_all <- select(classes, -Cluster) %>% sum()  # 3484

# Create matrix of normalized MLB classes by each cluster
classes.norm <- data.frame(matrix(NA, nrow = nrow(classes), ncol = ncol(classes) - 1))
for (i in 1:nrow(classes)) {
    # Total pitch count per cluster
    total <- sum(classes[i,-1])
    
    for (j in 2:ncol(classes)) {
        # Normalize values within each row (cluster)
        classes.norm[i, j - 1] <- classes[i, j] / total
    }
}

# Rename columns and add cluster ID
classes.norm <-
    rename(classes.norm, "Changeup" = X1, "Curveball" = X2, 
           "Four-seam Fastball" = X3, "Slider" = X4, "Two-seam Fastball" = X5) %>%
    mutate(Cluster = as.factor(1:9))

# Reshape and combine raw and normalized pitch counts
raw.count <- 
    classes %>%
    gather(`Changeup`, `Curveball`, `Four-seam Fastball`, `Slider`, 
           `Two-seam Fastball`, key = "MLB", value = "Count")
norm.count <-
    classes.norm %>%
    gather(`Changeup`, `Curveball`, `Four-seam Fastball`,
           `Slider`, `Two-seam Fastball`, key = "MLB", value = "Frequency")

combined <- inner_join(raw.count, norm.count, by = c("Cluster", "MLB"))

# Plot classification table   
ggplot(combined, aes(x = MLB, y = Cluster)) +
    geom_tile(aes(fill = Frequency)) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") + 
    labs(x = "", y = "Cluster")

# Clusters by movement
ggplot(p, aes(x = pfx_x, y = pfx_z)) +
    geom_point(aes(color = as.character(cclass)))




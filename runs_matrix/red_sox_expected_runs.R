# red_sox_expected_runs.R
setwd("Documents/local/baseball/runs_matrix")
source("expected_runs_matrix.R")

# Import 2016 Retrosheet season data
season16 <- read.csv("all2016.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(season16) <- fields[, "Header"]

# Compare Red Sox expectancy vs league
red.sox.expected <- create.runs.matrix(season16, "BOS")
mlb.expected <- create.runs.matrix(season16)
red.sox.diff <- round(100 * (red.sox.expected - mlb.expected) / mlb.expected, 2)

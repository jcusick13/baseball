# expected_runs_matrix.R


create.runs.matrix <- function(df, team="all"){
  # Creates runs expectancy matrix from a single season
  #  of Retrosheet data. Compiles matrix for individual team or
  #  entire league.
  
  # Restrict analysis to single team if necessary
  ifelse(team != "all", df <- subset.team(df, team), df)
  
  season <- calculate.ROI(df)
  season <- state.current(season)
  season <- state.new(season)
  
  # Restrict analysis to changes in state/runs scored
  season <- subset(season, (STATE != NEW.STATE) |
                     (RUNS.SCORED > 0))
  
  # Remove innings with less than 3 outs
  library(dplyr)
  data.outs <- summarise(group_by(season, HALF.INNING), Outs.Inning=sum(EVENT_OUTS_CT))
  season <- merge(season, data.outs)
  season.3o <- subset(season, Outs.Inning == 3)
  
  runs.exp <- create.matrix(season.3o)
}


subset.team <- function(df, team){
  # Subsets season data to return data frame of
  #  records where only 'team' is at bat
  #
  # df: data frame, full season of Retrosheet data
  # team: string, Retrosheet team abbreviation
  
  df.team <- subset(df, AWAY_TEAM_ID == team |
                        substr(GAME_ID, 1, 3) == team)
  df.team.ab <- subset(df.team,
                       (AWAY_TEAM_ID == team & BAT_HOME_ID == 0) |
                       (AWAY_TEAM_ID != team & BAT_HOME_ID == 1))
}


calculate.ROI <- function(df){
  # Creates runs scored in the remainder of a half inning
  #  for each plate appearance. Returns data frame with the 
  #  additional cols of RUNS, HALF.INNING, RUNS.SCORED, RUNS.ROI
  
  # Count current runs and create ID for each half inning
  df$RUNS <- with(df, AWAY_SCORE_CT + HOME_SCORE_CT)
  df$HALF.INNING <- with(df, paste(GAME_ID, INN_CT, BAT_HOME_ID))
  
  # Sum total runs scored on each at bat
  df$RUNS.SCORED <- with(df, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
                             (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
  
  # Total runs scored DURING each half inning
  RUNS.SCORED.INNING <- aggregate(df$RUNS.SCORED,
                                  list(HALF.INNING=df$HALF.INNING), sum)
  
  # Total runs scored AT START of each half inning
  RUNS.SCORED.START <- aggregate(df$RUNS,
                                 list(HALF.INNING=df$HALF.INNING), "[", 1)
  
  # Total runs scored AT END of each half inning
  MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
  MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
  
  # Add half inning run count to original df and rename
  df <- merge(df, MAX)
  N <- ncol(df)
  names(df)[N] <- "MAX.RUNS"
  
  df$RUNS.ROI <- with(df, MAX.RUNS - RUNS)
  df
}


state.rep <- function(runner1, runner2, runner3, outs){
  # Binary state representation of baserunners and outs
  #
  # runnerX: int, 0 for empty, 1 for present
  # outs: int, count of outs
  
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}


state.current <- function(df){
  # Records state of field for start of each at bat as defined
  #  by current baserunners and number of outs. Creates
  #  binary representation: 100 0 = runner on first, no outs.
  
  # Create status of each base
  RUNNER1 <- ifelse(as.character(df[, "BASE1_RUN_ID"]) == "", 0, 1)
  RUNNER2 <- ifelse(as.character(df[, "BASE2_RUN_ID"]) == "", 0, 1)
  RUNNER3 <- ifelse(as.character(df[, "BASE3_RUN_ID"]) == "", 0, 1)
  
  df$STATE <- state.rep(RUNNER1, RUNNER2, RUNNER3, df$OUTS_CT)
  df
}


state.new <- function(df){
  # Records state of field after each at bat as defined in state.current
  
  NRUNNER1 <- with(df, as.numeric(BAT_DEST_ID ==1))
  NRUNNER2 <- with(df, as.numeric(BAT_DEST_ID==2 | RUN1_DEST_ID==2))
  NRUNNER3 <- with(df, as.numeric(BAT_DEST_ID==3 | RUN1_DEST_ID==3 |
                                              RUN2_DEST_ID==3))
  NOUTS <- with(df, OUTS_CT + EVENT_OUTS_CT)
  
  df$NEW.STATE <- state.rep(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
  df
}


create.matrix <- function(df){
  # Creates runs expectancy matrix from formatted Retrosheet
  #  seasonal data. Runs new matrix of values by baserunner/outs
  #  combination.
  
  # Count average runs for each state of bases/outs
  RUNS <- with(df, aggregate(RUNS.ROI, list(STATE), mean))
  RUNS$Outs <- substr(RUNS$Group.1, 5, 5)
  RUNS <- RUNS[order(RUNS$Outs),]
  
  # Reformat data frame into matrix of base state/outs
  RUNS.out <- matrix(round(RUNS$x, 3), nrow=8, ncol=3)
  dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
  dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100",
                               "101", "110", "111")
  
  # Reorder matrix by more logical base states
  bases <- c("000", "100", "010", "001",
             "110", "101", "011", "111")
  RUNS.out <- RUNS.out[match(bases, dimnames(RUNS.out)[[1]]),]
}

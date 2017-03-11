parse.retrosheet.pbp <- function(season){
# Taken from Marchi and Albert (2013). More info at
# https://baseballwithr.wordpress.com/2014/02/10/downloading-retrosheet-data-and-runs-expectancy/
#
# Expects .../download/zipped/ and .../download/unzipped in working directory


  download.retrosheet <- function(season){
    download.file(
      url=paste("http://www.retrosheet.org/events/", season, "eve.zip", sep=""),
      destfile=paste("download", "/zipped/", season, "eve.zip", sep="")
      )
  }
  
  unzip.retrosheet <- function(season){
    unzip(paste("download", "/zipped/", season, "eve.zip", sep=""),
          exdir=paste("download", "/unzipped/", sep=""))
  }
  
  create.csv.file <- function(year){
    wd <- getwd()
    setwd("download/unzipped/")
    system(paste(paste("cwevent -y", year, "-f 0-96"),
                paste(year, "*.EV*", sep=""),
                paste("> all", year, ".csv", sep="")))
    setwd(wd)
  }
  
  create.csv.roster <- function(year){
    filenames <- list.files(path="download/unzipped/")
    filenames.roster <-
      subset(filenames, substr(filenames, 4, 11) == paste(year, ".ROS", sep=""))
    read.csv2 <- function(file)
      read.csv(paste("download/unzipped/", file, sep=""), header=FALSE)
    R <- do.call("rbind", lapply(filenames.roster, read.csv2))
    names(R)[1:6] <- c("Player.ID", "Last.Name", "First.Name",
                       "Bats", "Pitches", "Team")
    wd <- getwd()
    setwd("download/unzipped/")
    write.csv(R, file=paste("roster", year, ".csv", sep=""))
    setwd(wd)
  }
  
  cleanup <- function(){
    wd <- getwd()
    setwd("download/unzipped")
    system("rm *.EVN")
    system("rm *.EVA")
    system("rm *.ROS")
    system("rm TEAM*")
    setwd(wd)
  }
  
  download.retrosheet(season)
  unzip.retrosheet(season)
  create.csv.file(season)
  create.csv.roster(season)
  cleanup()
}
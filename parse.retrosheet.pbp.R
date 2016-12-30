parse.retrosheet.pbp <- function(season){
  download.retrosheet <- function(season){
    download.file(url=paste("http://www.retrosheet.org/events/",season,"eve.zip",sep=""),
                  ,destfile=paste("download.folder","/zipped/",season,"eve.zip",sep="")
                  )
  }

  unzip.retrosheet <- function(season){
    unzip(paste("download.folder","/zipped/",season,"eve.zip", sep=""),
          exdir=paste("download.folder","/unzipped", sep=""))
  }
  
  create.csv.file <- function(year){
    wd <- getwd()
    setwd("download.folder/unzipped")
    shell(paste(paste("cwevent -y", year, "-f 0-96"),
                paste(year, "*.EV*", sep=""),
                paste("> all", year, ".csv", sep="")))
    setwd(wd)
  }
  
  create.csv.roster <- function(year){
    filenames <- list.files(path = "download.folder/unzipped/")
    filenames.roster <-
      subset(filenames, substr(filenames, 4, 11)== paste(year,".ROS",sep=""))
    read.csv2 <- function(file)
      read.csv(paste("download.folder/unzipped/", file, sep=""), header=FALSE)
    R <- do.call("rbind", lapply(filenames.roster, read.csv2))
    names(R)[1 : 6] <-c("Player.ID", "Last.Name", "First.Name", "Bats", "Pitches", "Team")
    wd <- getwd()
    setwd("download.folder/unzipped")
    write.csv(R, file=paste("roster", year, ".csv", sep=""))
    setwd(wd)
  }
  
  cleanup <- function(){
    wd <- getwd()
    setwd("download.folder/unzipped")
    shell("del *.EVN")
    shell("del *.EVA")
    shell("del *.ROS")
    shell("del  TEAM*")
    setwd(wd)
  }
  download.retrosheet(season)
  unzip.retrosheet(season)
  create.csv.file(season)
  create.csv.roster(season)
  cleanup()
}

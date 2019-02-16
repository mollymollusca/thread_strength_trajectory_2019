# This code allowed me to put all files in 1 folder. Now the next step is to step through files in that folder.


# Read in all data files ===
setwd("~/threads/Force_trajectories")
folders <- list.dirs(full.names = FALSE)
folders_all <- folders[2:length(folders)]
directs_i <- paste("~/threads/Force_trajectories",folders_i, sep = "")
x <- 0
for(i in 1:length(directs_i)){
  directory <- directs_i[i]
  setwd(directory)
  files <- list.files(full.names = TRUE, include.dirs = TRUE)
  x <- x+1
  date <- folders_all[i]
for(j in 1:length(files)){
  setwd(directory)
  data <- read.csv(files[j], header = FALSE, stringsAsFactors = FALSE)
  # Here run the function which lets us generate the possible deltas
  # Save in a new folder with (1) the individual code that includes both the force trajectory and (2) a delta file for each 
  if(substr(files[j],21,21)=="."){
    test <- substr(files[j], 3,20)
  } else {
    test <- substr(files[j], 3,21)
  }
  filename_traj <- paste(date,test,".csv",sep="")
  setwd("~/threads/Trajectories_together")
  write.csv(data, file = filename_traj) ### NOT WORKING - NOT SURE WHY?
}
  
}

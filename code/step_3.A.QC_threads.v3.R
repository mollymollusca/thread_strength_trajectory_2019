# This is Part A of step 3 threads. First we do the QC and save new delta files. 
# Part B will be collating all of the delta files together, and will be very simple. 

# Check no break between 20 and 21, 22 and 23

library(gtools)

rm(list=ls())

#only run 1X per analysis
sysdate <- Sys.Date()
min_thread_strength_accepted <- 0.01

# Set up run metadata ####
# Data sources
wd_traj <- "~/thread_strength_trajectory_2019/Trajectories_together"
wd_peaks <- "~/thread_strength_trajectory_2019/analysis_20190417_good/PeakValley"

# Create the following working directories
directory <- "~/thread_strength_trajectory_2019/analysis_20190417_good"
newfolder <- paste(directory,'/step_3_',sysdate, sep="")
dir.create(newfolder)

wd_del_traj_plot <- paste(newfolder,"/del_traj_plot", sep="")
wd_del_hist_plot <- paste(newfolder,"/del_hist_plot", sep="")
wd_deltas <- paste(newfolder,"/deltas", sep="")
dir.create(wd_del_traj_plot)
dir.create(wd_del_hist_plot)
dir.create(wd_deltas)



comment(newfolder) <- "Info for this run. First time making an annotation file"

meta <- data.frame(
  comment = comment(newfolder),
  sysdate = sysdate,
  wd_traj = wd_traj, #trajectories
  wd_peaks = wd_peaks, #peaks and valleys - will be used for delta calcs
  wd_del_traj_plot = wd_del_traj_plot,
  wd_del_hist_plot = wd_del_hist_plot,
  wd_deltas=wd_deltas,
  wd_analysis_date = newfolder
)

# Open files #####
setwd(wd_peaks)
files <- list.files(full.names = TRUE, include.dirs = TRUE)
file_names <- list.files(full.names = FALSE, include.dirs = FALSE)

# Set starting and ending value
start<-58
for(j in start:length(file_names)){ ####
#for(j in 1:1){
    data_1 <- 0
    test2 <- 0
    setwd(wd_traj)
    data_1 <- read.csv(file = substr(file_names[j], 4, nchar(file_names[j])),header = TRUE, stringsAsFactors = FALSE, skip = 5)
    data_1 <- data_1[,2:4]
    names(data_1)<-c("time", "extension", "load")
    
    data_head <- read.csv(file = substr(file_names[j], 4, nchar(file_names[j])), header = FALSE, stringsAsFactors = FALSE)
    (data_head <- data_head[1:5,])
    date <- data_head[2,3]
    specimen_label <- data_head[3,3]
    max_load <- data_head[4,3]
    
    setwd(wd_peaks)
    test2<-NULL
    test2 <- read.csv(file = file_names[j], skip = 0, stringsAsFactors = FALSE, header = TRUE)
    header <- test2[1,] # Right now, for some reason the metadata did not save.... so this is just a placeholder.
    test2 <- test2[1:nrow(test2),] #Right now including all rows, but once the metadata is included, it will be great to include this.
    
    #test2 <- test2[test2$yDelta>=0.05,]
    print(j)
    print(files[j])
    #plot(data_1$load~data_1$time, pch=".", cex=1.5)
    #points(test2$Pload~test2$Ptime, pch=16, col="red")
    #points(test2$Vload~test2$Vtime, pch=16, col="blue")
    
    yDelta <- NULL
    yDelta <- test2$Pload-test2$Vload
    df1 <- cbind(test2,yDelta)
    df2a <- df1[df1$Vload>-.03,]
    df2a$Vload[df2a$Vload<0] <- 0
    df2 <- df2a[df2a$Pload>0.01,]
    df3 <- df2[df2$yDelta<=0.5,]
    df4 <- df3[df3$yDelta>=min_thread_strength_accepted,] # (see below)
    # Again, it's looking like the magic number of a minimum of 0.03 
    # catches oscillations that are not real breaks for the shittier code.
    # The code I was originally using doesn't really get those oscillations
    # so it's probably OK to include down to a delta of 0.01
    
    df_QC <- df4[!is.na(df4$Ptime),]
    
    setwd(wd_del_traj_plot)
    png(filename = paste(file_names[j],".png", sep = ""))
    plot(data_1$load~data_1$time, pch=".", cex=1.5)
    points(test2$Pload~test2$Ptime, pch=16, col="red")
    points(test2$Vload~test2$Vtime, pch=16, col="blue")
    points(df_QC$Pload~df_QC$Ptime, pch=16, col="pink")
    points(df_QC$Vload~df_QC$Vtime, pch=16, col="green")
    dev.off()
    
    test2<-df_QC
    
    # Check peaks individually ====
    str(test2)
    n <- nrow(test2)
    QC <- data.frame(
      del = rep(x = 0,times=n),
      num_th_broke = rep(x=0,times=n)
    )
    i <- 1
    #done <- "n"
    n <- nrow(QC)
    repeat{
      plot(data_1$load~data_1$time, pch=".", cex=1.5,
           xlim=c(test2$Ptime[i]-10, test2$Ptime[i]+10),
           ylim =c(test2$Vload[i]-.1,test2$Pload[i]+.1))
      points(test2$Pload[i]~test2$Ptime[i], pch=16, col="pink")
      points(test2$Vload[i]~test2$Vtime[i], pch=16, col="green")
      QC[i,1] <- ask(msg = "Is this a delta? (y=y,n=blank,undo=u,\n skip10=s,stop and break=b,)")
      if(QC[i,1] == "b"){
        break
      }
      else if(QC[i,1]=="u"){
        i <- i-3
      }
      else if(QC[i,1]=="s"){
        i <- i+10
      }
      else if(QC[i,1] == "y"){
        QC[i,2] <- ask(msg = "How many threads broke? (single = s, multiple = m, unknown = u, NA=NA)")
      } else QC[i,2] <- NA
      i = i+1
      if(i > n){
        break
      }
    }
    # # Stop here! ====
    # 
    # 
    # 
    # 
    setwd(wd_deltas)
    test2_QC <- cbind(df_QC, QC)
    write.csv(test2_QC, file = paste("deltas",file_names[j], sep = "_"), row.names = F)
}    


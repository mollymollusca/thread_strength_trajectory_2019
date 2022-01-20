#step_5_threads_catching the threads that fell through the cracks!
  
# Goal:
# Make sure no samples fall through the cracks
# Check that the sample has less than 5 good deltas identified within the normal range of deltas. 
# Run the analysis on the extra samples

#1. For each file check to see if there are 5 good deltas.

# This is Part A of step 3 threads. First we do the QC and save new delta files. 
# Part B will be collating all of the delta files together, and will be very simple. 

# Check no break between 20 and 21, 22 and 23


# Question:
# Do I need to do the by-hand QC?
# 1. Does the new delta file match the old one? If no, do 2
# 2. Does the old delta file have >7 'good' threads? If no, do 3
# 3. Run step 3A, which is the by hand analysis. 
# 4. For all, the old QC'ed and new QC'd delta files, include them in one file (step 3B)
# 5. Run the analysis. 

library(gtools)

rm(list=ls())

#only run 1X per analysis
sysdate <- Sys.Date()
min_thread_strength_accepted <- 0.002

# Set up run metadata ####
# Data sources
wd_traj <- "~/thread_strength_trajectory_2019/Trajectories_together"
wd_peaks_v1 <- "~/thread_strength_trajectory_2019/analysis_20190417_good/PeakValley"
wd_deltas_v1 <- "~/thread_strength_trajectory_2019/analysis_20190417_good/step_3_2019-04-29/deltas"
wd_peaks_v2 <- "~/thread_strength_trajectory_2019/analysis_20190501/PeakValley"


#
#... I still need to run the QC check on these, which is step 3B... 

# Create the following working directories
 directory <- "~/thread_strength_trajectory_2019/analysis_20190501"
 newfolder <- paste(directory,'/step_5', sep="")

 wd_deltas_new_output <- paste(newfolder,"/new_deltas", sep="")
 wd_deltas_old_output <- paste(newfolder,"/old_deltas", sep="")
 wd_deltas_all_output <- paste(newfolder,"/all_deltas", sep="")
 wd_step_5_error_header <- paste(newfolder,"/error_header", sep="")
 
 dir.create(newfolder)
 dir.create(wd_deltas_new_output)
 dir.create(wd_deltas_old_output)
 dir.create(wd_deltas_all_output)
 dir.create(wd_step_5_error_header)


# wd_del_traj_plot <- paste(newfolder,"/del_traj_plot", sep="")
# wd_del_hist_plot <- paste(newfolder,"/del_hist_plot", sep="")
# wd_deltas <- paste(newfolder,"/deltas", sep="")
# dir.create(wd_del_traj_plot)
# dir.create(wd_del_hist_plot)
# dir.create(wd_deltas)

comment(newfolder) <- "Info for this run. First time making an annotation file"

meta <- data.frame(
  comment = comment(newfolder),
  sysdate = sysdate,
  wd_traj = wd_traj, #trajectories
  wd_peaks.v1 = wd_peaks_v1, #peaks and valleys - will be used for delta calcs
  wd_peaks.v2 = wd_peaks_v2, #peaks and valleys - will be used for delta calcs
  #wd_del_traj_plot = wd_del_traj_plot,
  #wd_del_hist_plot = wd_del_hist_plot,
  wd_deltas_new_output, 
  wd_deltas_old_output, 
  wd_deltas_all_output,
  wd_analysis_date = newfolder
)

# Open files #####
setwd(wd_peaks_v1)
v1_peak_files <- list.files(full.names = TRUE, include.dirs = TRUE)
v1_peak_file_names <- list.files(full.names = FALSE, include.dirs = FALSE)

setwd(wd_peaks_v2)
v2_peak_files <- list.files(full.names = TRUE, include.dirs = TRUE)
v2_peak_file_names <- list.files(full.names = FALSE, include.dirs = FALSE)

setwd(wd_traj)
trajectory_files <- list.files(full.names = TRUE, include.dirs = TRUE)
trajectory_file_names <- list.files(full.names = FALSE, include.dirs = FALSE)

# Set starting and ending value
start<-230
for(j in start:length(trajectory_file_names)){ ####
#j <- 55
  #for(j in start:200){
  old_delta <- NA
  new_PV <- NA
  test2 <- 0
  print(j)
  print(trajectory_file_names[j])
  
  
  # Here, check if the old delta file exists
  old_delta_exists <- 0
  setwd(wd_deltas_v1)
  if (!file.exists(paste("deltas_PV_",trajectory_file_names[j], sep = ""))){
    print('old delta file does not exist')
    old_delta_exists <- "no"
    use_new_analysis <- "yes"
  } else {
    print('old peak valley file is present')
    old_delta_exists <- "yes"
    setwd(wd_peaks_v1)
    old_PV <- read.csv(file = paste("PV_",trajectory_file_names[j], sep = ""), skip = 0, stringsAsFactors = FALSE, header = TRUE)
    setwd(wd_deltas_v1)
    old_delta <- read.csv(file = paste("deltas_PV_",trajectory_file_names[j], sep = ""), skip = 0, stringsAsFactors = FALSE, header = TRUE)
    }
  
  setwd(wd_peaks_v2)
  if (!file.exists(paste("PV_",trajectory_file_names[j], sep = ""))){
    print('new peak valley file does not exist')
    new_delta_exists <- "no"
    use_new_analysis <- "no"
  } else {
    print('new peak valley file is present')
    new_delta_exists <- "yes"
    new_PV <- read.csv(file = paste("PV_",trajectory_file_names[j], sep = ""), skip = 0, stringsAsFactors = FALSE, header = TRUE)
  }
  
  
  #Now ask do the new and old PV files match? 
  PV_match <- NA
  if(new_delta_exists == "yes" & old_delta_exists == "yes"){
    if(nrow(new_PV)==nrow(old_PV) & (new_PV[1,1]=old_PV[1,1])){
      (PV_match <- "yes")
      use_new_analysis = "no"
      } else {
      (PV_match <- "no")
      }
  }
  

  
  
# Now ask, well, if they don't match are there enough 'good threads?' for the old file?
  count_threshold <- 5
  
  if (old_delta_exists == "yes" & PV_match=="no"){   
  test2 <- old_delta
  yDelta <- test2$Pload-test2$Vload
  df1 <- cbind(test2,yDelta)
  df2a <- df1[df1$Vload>-.03,]
  df2a$Vload[df2a$Vload<0] <- 0
  df2 <- df2a[df2a$Pload>0.01,]
  df3 <- df2[df2$yDelta<=0.5,]
  df4 <- df3[df3$yDelta>=min_thread_strength_accepted,]
  df_QC <- df4[!is.na(df4$Ptime),]
  
  #subset by the 'by eye QC'
  df_QC_subset <- df_QC[df_QC$del=='y'&df_QC$num_th_broke==1,]
  number_df_QC <- nrow(df_QC_subset)
  if(number_df_QC<=count_threshold){
    (use_new_analysis = "yes")
  }
  }

# === 
# If I have an old delta and I'm not doing the new analysis then use this:
    
if(use_new_analysis == "no" & old_delta_exists == "yes"){
  setwd(wd_deltas_old_output)
  write.csv(old_delta, file = paste("deltas_PV_",trajectory_file_names[j], sep = ""), row.names = F)
  
  setwd(wd_deltas_all_output)
  write.csv(old_delta, file = paste("deltas_PV_",trajectory_file_names[j], sep = ""), row.names = F)
  print("writing old file to deltas")
}
  
if(use_new_analysis == "no" & old_delta_exists == "no"){
  setwd(wd_traj)
  data_head <- NA
  tryCatch({
  data_head <- read.csv(file = trajectory_file_names[j], header = FALSE, stringsAsFactors = FALSE)
  data_head <- data_head[1:5,]
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
  setwd(wd_step_5_error_header)
  write.csv(data_head, file = paste("data_head",trajectory_file_names[j], sep = ""), row.names = F)
}  
  
if(use_new_analysis == "yes"){
  # read in new PV file
  data_1 <- 0
  setwd(wd_traj)
  data_1 <- read.csv(file = trajectory_file_names[j], header = TRUE, stringsAsFactors = FALSE, skip = 5)
  data_1 <- data_1[,2:4]
  names(data_1)<-c("time", "extension", "load")
  
  test2 <- new_PV
  
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
  
  #number_df_QC <- nrow(df_QC)
  
  #setwd(wd_del_traj_plot)
  #png(filename = paste(file_names[j],".png", sep = ""))
  plot(data_1$load~data_1$time, pch=".", cex=1.5)
  points(test2$Pload~test2$Ptime, pch=16, col="red")
  points(test2$Vload~test2$Vtime, pch=16, col="blue")
  points(df_QC$Pload~df_QC$Ptime, pch=16, col="pink")
  points(df_QC$Vload~df_QC$Vtime, pch=16, col="green")
  #dev.off()
  
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
  setwd(wd_deltas_new_output)
  test2_QC <- cbind(df_QC, QC)
  write.csv(test2_QC, file = paste("deltas_PV_",trajectory_file_names[j], sep = ""), row.names = F)
  
  setwd(wd_deltas_all_output)
  test2_QC <- cbind(df_QC, QC)
  write.csv(test2_QC, file = paste("deltas_PV_",trajectory_file_names[j], sep = ""), row.names = F)
 }  
} 
  
 

# This will be part B of step 3. 
# I will take the delta files which now includes QC info
# And use this to plot histograms, calculate medians, and create one summary sheet. 

# for 20190430, added a part to analyse only the last 5

rm(list=ls())

#only run 1X per analysis
sysdate <- Sys.Date()


# Set up run metadata ####
# Data sources - WD deltas may need to be changed
wd_traj <- "~/thread_strength_trajectory_2019/Trajectories_together"
wd_peaks <- "~/thread_strength_trajectory_2019/analysis_20190417_good/PeakValley"
wd_deltas <- "~/thread_strength_trajectory_2019/analysis_20190417_good/step_3_2019-04-29/deltas"

# Create the following working directories
directory <- "~/thread_strength_trajectory_2019/analysis_20190417_good/step_3_2019-04-29"
newfolder <- paste(directory,'/summary_',sysdate, sep="")
dir.create(newfolder)

wd_del_traj_plot <- paste(newfolder,"/del_traj_plot", sep="")
wd_del_hist_plot <- paste(newfolder,"/del_hist_plot", sep="")
dir.create(wd_del_traj_plot)
dir.create(wd_del_hist_plot)

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

# ouput file setup ####
input_names <- data.frame(
  date = numeric(),
  analysis_date = numeric(),
  filename = character(),
  specimen_label = character(),
  max_load = numeric(),
  dist_max = numeric(),
  median = numeric(),
  median_last5 = numeric(),
  variance = numeric(),
  num_threads_used = numeric(),
  single_breaks = numeric(),
  multiple_break_groups = numeric(),
  total_thread_breaks = numeric(),
  QC_flag = numeric(),
  b1=numeric(),
  b2=numeric(),
  b3=numeric(),
  b4=numeric(),
  b5=numeric(),
  b6=numeric(),
  b7=numeric(),
  b8=numeric(),
  stringsAsFactors = FALSE
)
output_append <- input_names[0,]
str(input_names)

setwd(wd_deltas)
files <- list.files(full.names = TRUE, include.dirs = TRUE)
file_names <- list.files(full.names = FALSE, include.dirs = FALSE)
j<-2
#file_names[3]
for(j in 1:length(file_names)){
  tryCatch({
    print(j)
    print(files[j])
    data_1 <- 0
    test2 <- 0
    setwd(wd_traj)
    data_1 <- read.csv(file = substr(file_names[j], 11, nchar(file_names[j])),header = TRUE, stringsAsFactors = FALSE, skip = 5)
    #data_1 <- read.csv(file = substr(file_names[j], 4, nchar(file_names[j])),header = TRUE, stringsAsFactors = FALSE, skip = 5)
    data_1 <- data_1[,2:4]
    names(data_1)<-c("time", "extension", "load")
    
    data_head <- read.csv(file = substr(file_names[j], 11, nchar(file_names[j])), header = FALSE, stringsAsFactors = FALSE)
    (data_head <- data_head[1:5,])
    date <- data_head[2,3]
    specimen_label <- data_head[3,3]
    max_load <- data_head[4,3]
    
    setwd(wd_deltas)
    test2<- NULL
    test2 <- read.csv(file = file_names[j], skip = 0, stringsAsFactors = FALSE, header = TRUE)
    
    # subset threads
    del_single <- test2[test2$del=='y' & (test2$num_th_broke == "s" | test2$num_th_broke == 1),]
    del_all <- test2
    del_d_single <- del_single$yDelta[!is.na(del_single$yDelta)]
    del_d_all <- del_all$yDelta[!is.na(del_all$yDelta)]
    
    # Delta calculations ####
    median_del <-NA # median
    median_last5 <-NA # median of the last 3 threads
    sd_del <- NA    # standard devition
    i.max <- NA     # which (i) is max
    M2 = NA         # maximum thread break
    QC_flag <- 1    # QC flag (1 = bad)
    d <- NA         # density function
    
    # Estimate the median of all single threads
    if(length(del_d_single) > 0){  
    median_del <-median(del_d_single)
    nrow.del <- length(del_d_single)
    if(nrow.del >= 5){
    start <- nrow.del - 4
    } else {
    start <- 1 
    } 
    median_last5 <- median(del_d_single[start:nrow.del])
    sd_del <- sd(del_d_single)
    }
    
    # When there is more than one delta (single threads only), 
    # estimate the density:
    if(length(del_d_single) > 1){  
      d <- density(del_d_single)
      i.max = which.max(d$y)
      M2 = d$x[i.max]
      M2
    }
  
    # When there is at least one delta (all threads here),
    # estimate the median
    if(length(del_d_all)>0 & !is.na(median_del)){
      h <- hist((del_d_all), breaks=seq(from=0,to=1,by = 0.05), plot = FALSE)
      med <- median(del_d_single, na.rm = TRUE)
      
      b1<-sum(h$counts[h$mids < 2*med]) #sum of breaks around the median
      b2<-sum(h$counts[h$mids >= 2*med & h$mids < 3*med]) #sum of breaks between 2-3 times the median
      b3<-sum(h$counts[h$mids >= 3*med & h$mids < 4*med])
      b4<-sum(h$counts[h$mids >= 4*med & h$mids < 5*med])
      b5<-sum(h$counts[h$mids >= 5*med & h$mids < 6*med])
      b6<-sum(h$counts[h$mids >= 6*med & h$mids < 7*med])
      b7<-sum(h$counts[h$mids >= 7*med & h$mids < 8*med])
      b8ormore<-sum(h$counts[h$mids >= 8*med])
      
      b1m <- b1
      b2m <- 2*b2
      b3m <- 3*b3
      b4m <- 4*b4
      b5m <- 5*b5
      b6m <- 6*b6
      b7m <- 7*b7
      b8m <- 8*b8ormore
      
      single_breaks <- b1
      multiple_break_groups <- sum(b2,b3,b4,b5,b6,b7,b8ormore)
      total_thread_breaks <- sum(b1m,b2m,b3m,b4m,b5m,b6m,b7m,b8m)
      
      # Make a plot
      setwd(wd_del_hist_plot)
      png(filename = paste(file_names[j],".png", sep = ""))
      par(mfrow = c(3,1))
      if(!is.na(d[1])){
        plot(d)
      } else {hist(del_d_all, breaks=seq(from=0,to=2,by = 0.05))}
      hist(del_d_all, breaks=seq(from=0,to=.5,by = 0.05))
      del_d_b1 <- NA
      del_d_b1<- del_d_all[del_d_all<2*med]
      hist(del_d_b1, breaks=seq(from=0,to=.5,by = 0.05))
      dev.off()
      
      setwd(wd_del_traj_plot)
      png(filename = paste(file_names[j],".png", sep = ""))
      par(mfrow = c(1,2), omi = c(0,0,0,0))
      plot(data_1$time,data_1$load, pch = '.',
           ylab = "Load",
           xlab = "Time")
      # Could convert deltas to 
      arrows(del_single$Ptime+4, del_single$Vload, del_single$Ptime+4, del_single$Pload, length=0.01, angle=90, code=2, col = "blue", lwd = 2)
      hist(del_d_all, breaks=seq(from=0,to=.5,by = 0.05), col=rgb(0,1,0,0.5), main="", xlab = "Delta (Change in load)")
      hist(del_d_single, breaks=seq(from=0,to=.5,by = 0.05), col=rgb(1,0,0,0.5), add = T)
      hist(del_d_b1, breaks=seq(from=0,to=.5,by = 0.05), col=rgb(0,0,1,0.5), add=T)
      dev.off()
      
      QC_flag_criteria1 <- " If 80% of the breaks were single thread breaks, we will consider it a typical sample. 
 # If not, let's flag it and look more in depth at this sample."
      QC_flag_criteria2 <- " If there are <5 breaks that are single thread breaks, we will consider it a typical sample. 
      # If not, let's flag it and look more in depth at this sample."
      
      if(single_breaks<.8*total_thread_breaks){
        QC_flag <- 1
      } else {
        QC_flag <- 0
      }
      if(single_breaks<5){
        QC_flag <- 1
      } else {
        QC_flag <- 0
      }
    }
    
    # Save deltas in output ####
    output_vec <- data.frame(
      date = NA,
      analysis_date = NA,
      filename = NA,
      specimen_label = NA,
      max_load = NA,
      dist_max = NA,
      median = NA,
      median_last5 = NA,
      variance = NA,
      num_threads_used = NA,
      single_breaks = NA,
      multiple_break_groups = NA,
      total_thread_breaks = NA,
      QC_flag = 1,
      b1=NA,
      b2=NA,
      b3=NA,
      b4=NA,
      b5=NA,
      b6=NA,
      b7=NA,
      b8=NA
    )
    
    if(nrow(del_single) > 0){
      output_vec <- data.frame(
        date = date,
        analysis_date = sysdate,
        filename = file_names[j],
        specimen_label = specimen_label,
        max_load = max_load,
        dist_max = M2,
        median = median_del,
        median_last5 = median_last5,
        variance = sd_del,
        num_threads_used = length(del_d_single),
        single_breaks = single_breaks,
        multiple_break_groups = multiple_break_groups,
        total_thread_breaks = total_thread_breaks,
        QC_flag = QC_flag,
        b1=b1,
        b2=b2,
        b3=b3,
        b4=b4,
        b5=b5,
        b6=b6,
        b7=b7,
        b8=b8ormore
      )
    } else {
      output_vec <- data.frame(
        date = date,
        analysis_date = sysdate,
        filename = file_names[j],
        specimen_label = specimen_label,
        max_load = max_load,
        dist_max = NA,
        median = NA,
        median_last5 = NA,
        variance = NA,
        num_threads_used = NA,
        single_breaks = NA,
        multiple_break_groups = NA,
        total_thread_breaks = NA,
        QC_flag = 1,
        b1=NA,
        b2=NA,
        b3=NA,
        b4=NA,
        b5=NA,
        b6=NA,
        b7=NA,
        b8=NA
      )
    }
    
    output_append <- rbind(output_append,output_vec)
    
    
    
    
    # plot(data_1$load~data_1$time, pch=".", cex=1.5)
    # plot(del$Pload~del$Ptime, pch=16, col="pink", cex = .5)
    # points(del$Vload~del$Vtime, pch=16, col="green")
    
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

setwd(newfolder)
write.csv(output_append, file = paste("summary",sysdate,".csv", sep = ""), row.names = FALSE)
write.csv(meta, file = paste("metadata",sysdate,".csv", sep = ""), row.names = FALSE)

    
# One idea is to use this to catch files that don't show up with the other analysis... 

# This code is modified from v.2.1 using Michael's newer version 
# of finding peaks. The code finds many peaks so there is an extra QC step that removes the oscillations. 
# The hope is that this will catch lower deltas, and so there might be some extra files that get considered as
# part of the dataset. 
# I'm still getting some bad deltas... and this code is slow, so switching this to lower priority. 
# Maybe there is some way to get the extra correct deltas, but for now I'm continuing without them -4/22/19
# The cutoff for this code is deltas that are >0.03. Below this I'm getting some spurious deltas. 


# The best code is step_2_threads_2.3. This is just for trying a 'new thing.'
# The best data files are 20190417

# This code steps through the files and generates a set of potential deltas for each. 
# There are two functinos, FindPeaks, and PeaksAndValleys.
# I have a subset function that limits potential deltas to a minimum of 0.03 load
# Note that this code is based off of step_2_threads_2.0 NOT 2.1
# For version 2.2, I modified the code to check that all files not used for analyses 
# went into their own folder. This is to check that they truly should be discarded, 
# and no deltas were missed. 

# This code is really slow and would need to run overnight. In about 1 hour, 30 out of 180 runs were analysed. 
rm(list=ls())

# Path and file directories, check paths work
wd_data_source <- "~/thread_strength_trajectory_2019/Trajectories_together"
wd_plot_dir <- "~/thread_strength_trajectory_2019/analysis_20190421/Trajectories_plots"
setwd(wd_plot_dir)
wd_output_error_header <- "~/thread_strength_trajectory_2019/analysis_20190421/Error_header"
setwd(wd_output_error_header)
wd_output_error_plots <- "~/thread_strength_trajectory_2019/analysis_20190421/Error_plots"
setwd(wd_output_error_plots)
wd_peak_valley_output <- "~/thread_strength_trajectory_2019/analysis_20190421/PeakValley"
setwd(wd_peak_valley_output)





PeaksAndValleys<-function(points){
  
  PV<-data.frame(time=c(0), load=c(0))
  y1<-NULL
  y2<-NULL
  y3<-NULL
  for(i in 2:(length(points$time)-1)){
    y1<-points$load[i-1]
    y2<-points$load[i]
    y3<-points$load[i+1]
    T1 <- 0.001
    if ((y2>(y1-T1)&y2>y3) | (y2<y1&y2<(y3+T1))){
      if(PV$load[1]==0){
        PV$time[1]<-points$time[i]
        PV$load[1]<-y2
      }else{
        PV<-rbind(PV, data.frame(time=points$time[i], load=y2))
      }
    }
  }
  return(PV)	
}

FindPeaks<-function(points, magslope=0.05){
  
  peaks<-data.frame(Ptime=c(0), Pload=c(0), Vtime=c(0), 
                    Vload=c(0), yDelta=c(0))
  y1<-NULL
  y2<-NULL
  x1<-NULL
  x2<-NULL
  
  for(i in 1:(length(points$time)-1)){
    y1<-points$load[i]
    y2<-points$load[i+1]
    x1<-points$time[i]
    x2<-points$time[i+1]
    if ((abs(y2-y1)/(x2-x1))>=magslope){
      if(peaks$Pload[1]==0){
        peaks$Ptime[1]<-x1
        peaks$Pload[1]<-y1
        peaks$Vtime[1]<-x2
        peaks$Vload[1]<-y2
        peaks$yDelta[1]<-y1-y2 # included peaks$ in front of yDelta
      }else{
        peaks<-rbind(peaks, data.frame(Ptime=x2, Pload=y1, 
                                       Vtime=x2, Vload=y2, yDelta=y1-y2))
      }
    }
  }
  return(peaks)	
}

# Open files #####
setwd(wd_data_source)
files <- list.files(full.names = TRUE, include.dirs = TRUE)
file_names <- list.files(full.names = FALSE, include.dirs = FALSE)
length(files)
length(file_names)
# Ok there are 252 files. 
# Now I'm wondering if I am missing some... I should check the notebook. 

# Loop code ####
testing <- "no"
#testing <- "yes"
if(testing=="yes"){
  loop_length <- 10
  } else {loop_length <- length(files)}
  
  
for(j in 1:loop_length){
  tryCatch({
  #Note, for testing j <- 7 is a good one
  #j <- 4
  #j <- 16
  #j<-7
  setwd(wd_data_source)
  data_1 <- 0
  data_1 <- read.csv(file = files[j], header = TRUE, stringsAsFactors = FALSE, skip = 5)
  data_1 <- data_1[,2:4]
  
  data_head <- read.csv(file = files[j], header = FALSE, stringsAsFactors = FALSE)
  (data_head <- data_head[1:5,])
  date <- data_head[2,3]
  specimen_label <- data_head[3,3]
  max_load <- data_head[4,3]
  
  print(j)
  print(files[j])
  # head(data_1, 10)
    if(all(names(data_1) == c("X.s.","X.mm.","X.N."))){
      names(data_1)<-c("time", "extension", "load")
      
      if(testing=="yes"){
      plot(data_1$load~data_1$time, pch=".", cex=1.5)
      }
  
      # Subset data ####
      #df_slopes <- Subset_data(Vthresh = -.04, points = data_1)
    
      
      # Run Peaks and Valleys #####
      # data_2 <- data.frame(
      #   time = df_slopes$x1,
      #   load = df_slopes$y1
      #   )
      data_2 <- data_1
  
      T1<-NA
      if(nrow(data_1)>=100){
        tryCatch({
          
          test1 <- PeaksAndValleys(data_1)
          test2 <- FindPeaks(test1, magslope=.2)
          test2 <- test2[test2$yDelta>=0.03,]
          #test2.down <- test2[,1:length(test2)-1]
          #test2.up <- test2[,2:length(test2)]
          str(test2)
          #T1 <- test2.down[(test2.down$Pload-test2.up$Pload)>=0.0001,]
          
          test3 <- test2[!is.na(test2$Pload),]
          T1 <- test3[1,]
          if(testing == "yes"){
          k <- 20
          k <- 21
          points(test3$Vtime[k],test3$Vload[k], cex = 2)
          points(test3$Ptime[k],test3$Vload[k], cex = 2)
          points(test3$Ptime[k+1],test3$Vload[k+1], cex = 2)
          points(test3$Ptime[k]+5, test3$Vload[k]+0.08, cex = .75)
          }
          for(k in 1:(nrow(test3)-1)){
            # here is the QC part of this to avoid the oscillations becoming breaks. I'm seeing if this code will pick up more thread breaks...?
            if((test3$Ptime[k+1]-test3$Ptime[k])>=0 & (test3$Ptime[k+1]-test3$Ptime[k])<=5  &
               ((test3$Vload[k+1]-test3$Vload[k])>=-.03) & (test3$Vload[k+1]-test3$Vload[k])<=.08 &
                (abs(test3$yDelta[k]-test3$yDelta[k+1])<=0.3))
              {
              T1[k,] <- NA
              } else {
              T1[k,] <- test3[k,]
              }
          }
  
          T1 <- T1[!is.na(T1$Ptime),]
          #str(T1)
          #T1 <- test2
          #T1<-PeaksAndValleys(data=data_2, pThresh=0, vThresh=0, maxint=500, maxPre=0.1, dyMin=0.001, dydxMax=(-1))
        }, error = function(e){cat("ERROR in PeaksAndValleys :", conditionMessage(e), "\n")})
      }
      if(!is.na(T1)){
       setwd(wd_plot_dir)
       png(filename = paste(file_names[j],".png", sep = ""))
       plot(data_1$load~data_1$time, pch=".", cex=1.5)
       points(T1$Pload~T1$Ptime, pch=16, cex=.75, col='green')
       points(T1$Vload~T1$Vtime, pch=16, cex=.75, col='red')
       dev.off()
       
       
       # Write deltas to file ####
       setwd(wd_peak_valley_output)
       metadata <- c(specimen_label, date, max_load,"")
       rbind(metadata,T1)
       write.csv(x = T1, file = paste("PV",file_names[j], sep = "_"), row.names = F)
       
       
       setwd(wd_data_source)
      }else{
       setwd(wd_output_error_header)
       #write.csv(file = files[j], data_head)
       write.csv(file = files[j], data_head)
       #png(filename = paste(file_names[j],".png", sep = ""))
       #plot(data_1$load~data_1$time, pch=".", cex=1.5)
       #dev.off()
       if(nrow(data_1)>=1){
       setwd(wd_output_error_plots)
       png(filename = paste(file_names[j],".png", sep = ""))
       plot(data_1$load~data_1$time, pch=".", cex=1.5)
       dev.off()
       }
       setwd(wd_data_source)
      }
     }
  }, error = function(e){cat("General ERROR:", conditionMessage(e), "\n")})
}


#read.csv(file = paste("PV",file_names[1], sep = "_"), skip = 0)
#Then just use the newer lines for all the calcs




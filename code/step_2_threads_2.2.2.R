# This is the same as 2.3, but I'm trying out the other peaks and valleys code from 2.2... 

# This code steps through the files and generates a set of potential deltas for each. 
# There are two functinos, FindValley, and PeaksAndValleys.
# Note that there are issues with a few deltas here and there not getting picked up by the code. 
# This is why we created 2.1, however with 2.1 there is the issue that there are too many false positives,
# i.e. false deltas picked up that are just the sinusoidal oscillations that place after an actual delta. 
# I have a subset function that only takes candidate points for the "points" 
# part. 
# Note that this code is based off of step_2_threads_2.0 NOT 2.1
# For version 2.2, I modified the code to check that all files not used for analyses 
# went into their own folder. This is to check that they truly should be discarded, 
# and no deltas were missed. 


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





# Functions ####
FindValley<-function(points, Vthresh){
  
  m<-1
  y1<-points$load[m]
  y2<-points$load[m+1]
  x1<-points$time[m]
  x2<-points$time[m+1]
  slope2<-(y2-y1)/(x2-x1)
  slopemin<-slope2
  
  m<-m+1
  
  repeat{
    y1<-points$load[m]
    y2<-points$load[m+1]
    x1<-points$time[m]
    x2<-points$time[m+1]
    
    slope2<-(y2-y1)/(x2-x1)
    
    if(slope2<slopemin){
      slopemin<-slope2    #Finds the minimum slope, step by step between each pair of points
    }
    
    if(slope2>=Vthresh){
      m<-m-1              #If the slope between the two points is positive, break
      break
    }else if (m==(length(points$load)-2)){
      m<-0                
      break
    }else{
      m<-m+1
    }
  }
  return(data.frame(numpoints=(m), minslope=slopemin))
}

#dyMin is the magnitude and should be positive or 0, dydxMax is the slope, should be 0 or less#
PeaksAndValleys<-function(data, pThresh=0, vThresh=0, maxint=500, maxPre=0.1, dyMin=0.01, dydxMax=(-1)){
  y0<-NULL
  yA1<-NULL
  slope1<-9999
  preslope<-NULL
  Vx<-NULL
  n<-NULL
  PV<-NULL
  temp<-NULL
  
  
  #first peak/valley pair#
  
  n<-5
  n<-23833
  repeat{
    n<-n+1
    
    y0<-data$load[n]
    yA1<-data$load[n+1]
    
    slope1<-yA1-y0
    
    if(is.na(slope1)){
      break
    }
    
    
    #check for peak#
    if (slope1<pThresh){
      
      #checks slope of preceeding five points#
      preslope<-(data$load[n]-data$load[n-5])/(data$time[n]-data$time[n-5])
      
      #finds following valley#
      temp<-FindValley(points=data[(n):(n+maxint),], Vthresh=vThresh)
      Vx<-temp$numpoints[1]
      
      #prints warning if does not find valley in the segment of points examined, stops loop#
      if (Vx==0){
        print("past maxint")
        break
        
        #compares dy tp dyMin and dydx to dydxMin#
      }else if ((data$load[n]-data$load[n+Vx])>=dyMin & temp$minslope[1]<=dydxMax & preslope<=maxPre){
        #records peak/valley pair, advances n#
        PV<-data.frame(Ptime=data$time[n], Pload=data$load[n], Vtime=data$time[n+Vx], Vload=data$load[n+Vx])
        n<-n+Vx
        break
      }else{
        n<-n+Vx
      }
      
    }
    
  }
  
  repeat{
    
    n<-n+1
    
    if((n+3)>length(data$time)){
      break
    }
    
    if((n+maxint)>length(data$time)){
      maxint<-length(data$time)-n
    }
    
    y0<-data$load[n]
    yA1<-data$load[n+1]
    slope1<-99999
    slope1<-yA1-y0
    
    #check for peak#
    if (slope1<pThresh){
      
      #checks slope of preceeding five points#
      preslope<-(data$load[n]-data$load[n-5])/(data$time[n]-data$time[n-5])
      
      #finds following valley#
      temp<-FindValley(points=data[(n):(n+maxint),], Vthresh=vThresh)
      Vx<-temp$numpoints[1]
      
      #prints warning if does not find valley in the segment of points examined, prints n to show if its because it reached the end, stops loop#
      if (Vx==0){
        print("past maxint")
        print(paste(n, "/", length(data$time)))
        break
        
        #compares dy tp dyMin and dydx to dydxMin#
      }else if ((data$load[n]-data$load[n+Vx])>=dyMin & (temp$minslope[1])<=dydxMax & preslope<=maxPre){
        #records peak/valley pair, advances n#
        PV<-rbind(PV, data.frame(Ptime=data$time[n], Pload=data$load[n], Vtime=data$time[n+Vx], Vload=data$load[n+Vx]))
        n<-n+Vx
      }else{
        n<-n+Vx
      }
      
    }
  }
  return(PV)
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
  
      T1<-NULL
      if(nrow(data_1)>=100){
        tryCatch({
      T1<-PeaksAndValleys(data=data_2, pThresh=0, vThresh=0, maxint=500, maxPre=0.1, dyMin=0.001, dydxMax=(-1))
        }, error = function(e){cat("ERROR in PeaksAndValleys :", conditionMessage(e), "\n")})
      }
      if(!is.null(T1)){
       setwd(wd_plot_dir)
       png(filename = paste(file_names[j],".png", sep = ""))
       plot(data_1$load~data_1$time, pch=".", cex=1.5)
       points(T1$Pload~T1$Ptime, pch=16, cex=1.25, col='green')
       points(T1$Vload~T1$Vtime, pch=16, cex=1.25, col='red')
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




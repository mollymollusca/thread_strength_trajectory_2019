# This code steps through the files and generates a set of deltas for each. I have a subset function that only takes candidate points for the "points" part. 
# This version, 2.1 includes a different function
#This is actually catching more false positives... :(

# Functions ####
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
setwd("~/threads/Trajectories_together")
files <- list.files(full.names = TRUE, include.dirs = TRUE)
file_names <- list.files(full.names = FALSE, include.dirs = FALSE)

# Loop code ####
for(j in 7:7
    #length(file_names)
    ){
  tryCatch({
  #j <- 3 # 7 is a good one
  setwd("~/threads/Trajectories_together")
  data_1 <- 0
  data_1 <- read.csv(file = files[j], header = TRUE, stringsAsFactors = FALSE, skip = 5)
  data_1 <- data_1[,2:4]
  print(files[j])
  # head(data_1, 10)
    if(all(names(data_1) == c("X.s.","X.mm.","X.N."))){
      names(data_1)<-c("time", "extension", "load")
      #plot(data_1$load~data_1$time, pch=".", cex=1.5)
  
      # Subset data ####
      #df_slopes <- Subset_data(Vthresh = -.04, points = data_1)
    
      
      # Run Peaks and Valleys #####
      # data_2 <- data.frame(
      #   time = df_slopes$x1,
      #   load = df_slopes$y1
      #   )
      test1<-PeaksAndValleys(data_1)
      plot(data_1$load~data_1$time, pch=".", cex=1.5)
      points(test1$load~test1$time, pch=16, col="red")
      
      test2 <- FindPeaks(test1, magslope=.2)
      test2 <- test2[test2$yDelta>=0.05,]
      plot(data_1$load~data_1$time, pch=".", cex=1.5)
      points(test2$Pload~test2$Ptime, pch=16, col="red")
      points(test2$Vload~test2$Vtime, pch=16, col="blue")
  
      T1<-NULL
      if(nrow(data_1)>=100){
      #T1<-PeaksAndValleys(data=data_2, pThresh=0, vThresh=0, maxint=500, maxPre=0.1, dyMin=0.01, dydxMax=(-1))
       setwd("~/threads/Trajectories_plots2.1")
       png(filename = paste(file_names[j],".png", sep = ""))
       plot(data_1$load~data_1$time, pch=".", cex=1.5, xlim=c(150, 625))
       points(T1$Pload~T1$Ptime, pch=16, cex=1.25, col='green')
       points(T1$Vload~T1$Vtime, pch=16, cex=1.25, col='red')
       dev.off()
       setwd(setwd("~/threads/Trajectories_together"))
      }else{
        setwd("~/threads/Trajectories_plots/Error")
        
      }


  
    # Write deltas to file ####
    setwd("~/threads/PeakValley2.1")
    write.csv(x = T1, file = paste("PV",file_names[j], sep = "_"))
    }
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

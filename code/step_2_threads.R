# This opens each file and creates a datafile of deltas
# Having issues with sorting through files without deltas

rm(list = ls())

setwd("~/threads/Trajectories_together")

# Functions from Finding peaks 3.4.2.R
FindValley<-function(points, Vthresh){
  
  # So checking there are two negative slopes in a row:
  # First slope
  y1_vec <- points$load[1:(nrow(points)-1)]
  y2_vec <- points$load[2:nrow(points)]
  x1_vec <- points$time[1:(nrow(points)-1)]
  x2_vec <- points$time[2:nrow(points)]
  slope1_vec <- (y2_vec-y1_vec)/(x2_vec-x1_vec)
  
  # Second slope
  slope2_vec <- slope1_vec[2:nrow(points)]
  
  df_slopes <- data.frame(x1 = x1_vec,
                          y1 = y1_vec,
                          slope1 = slope1_vec,
                          slope2 = slope2_vec
  )
  # Check that the second and first slopes are both negative 
  df_slopes <- df_slopes[slope1_vec <= Vthresh && slope2_vec <= Vthresh,]
  return(data.frame(numpoints=(nrows(df_slopes)), minslope=df_slopes$slope1))
}
  # m<-1
  # y1<-points$load[m]
  # y2<-points$load[m+1]
  # x1<-points$time[m]
  # x2<-points$time[m+1]
  # slope2<-(y2-y1)/(x2-x1)
  # slopemin<-slope2
  # 
  # m<-m+1
  # 
  # repeat{
  #   y1<-points$load[m]
  #   y2<-points$load[m+1]
  #   x1<-points$time[m]
  #   x2<-points$time[m+1]
  #   
  #   slope2<-(y2-y1)/(x2-x1)
  #   
  #   if(slope2<slopemin){
  #     slopemin<-slope2
  #   }
  #   
  #   if(slope2>=Vthresh){
  #     m<-m-1
  #     break
  #   }else if (m==(length(points$load)-2)){
  #     m<-0
  #     break
  #   }else{
  #     m<-m+1
  #   }
  # }
  # return(data.frame(numpoints=(m), minslope=slopemin))

#dyMin is the magnitude and should be positive or 0, dydxMax is the slope, should be 0 or less#
PeaksAndValleys<-function(data, pThresh=0, vThresh=0, maxint=500, maxPre=0.1, dyMin=0.01, dydxMax=(-1)){
  
  y0<-NULL
  yA1<-NULL
  slope1<-NULL
  preslope<-NULL
  Vx<-NULL
  n<-NULL
  PV<-NULL
  temp<-NULL
  
  #first peak/valley pair#
  n<-5
  repeat{
    n<-n+1
    
    y0<-data$load[n]
    yA1<-data$load[n+1]
    slope1<(-yA1-y0)*10^6
    
    
    
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
        
        #compares dy to dyMin and dydx to dydxMin#
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


# Run through data
setwd("~/threads/Trajectories_together")
files <- list.files(full.names = TRUE, include.dirs = TRUE)
file_names <- list.files(full.names = FALSE, include.dirs = FALSE)
for(j in 8:10){
  j <- 7 # 7 is a good one
  setwd("~/threads/Trajectories_together")
  data_1 <- 0
  data_1 <- read.csv(file = files[j], header = TRUE, stringsAsFactors = FALSE, skip = 5)
  data_1 <- data_1[,2:4]
  head(data_1, 10)
  str(data_1)
  names(data_1)<-c("time", "extension", "load")
  plot(data_1$load~data_1$time, pch=".", cex=1.5)
  #setwd("~/threads/Trajectories_plots")
  #png(filename = paste(file_names[j],".png", sep = ""))
  
  T1<-NULL
   if(nrow(data_1)>=100){
   T1<-PeaksAndValleys(data=data_1, pThresh=0, vThresh=0, maxint=500, maxPre=0.1, dyMin=0.01, dydxMax=(-1))
   }
  setwd("~/threads/PeakValley")
  write.csv(x = T1, file = paste("PV",file_names[j], sep = "_"))
  
  #png(filename = paste(files[j],".png",sep = ""))
  #plot(data_1$load~data_1$time, pch=".", cex=1.5)
  #points(T1$Pload~T1$Ptime, pch=16, cex=1.25, col='green')
  #points(T1$Vload~T1$Vtime, pch=16, cex=1.25, col='red')
  #dev.off()
  #test2 <- T1
  #}
}   
  

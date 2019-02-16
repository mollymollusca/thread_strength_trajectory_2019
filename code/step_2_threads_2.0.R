# This code steps through the files and generates a set of deltas for each. I have a subset function that only takes candidate points for the "points" part. 
#This one seems like the best code so far. Yep, it is... I'm going to go with it. NOT 2.1

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
      slopemin<-slope2
    }
    
    if(slope2>=Vthresh){
      m<-m-1
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
    slope1<-yA1-y0
      
      
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
setwd("~/threads/Trajectories_together")
files <- list.files(full.names = TRUE, include.dirs = TRUE)
file_names <- list.files(full.names = FALSE, include.dirs = FALSE)

# Loop code ####
for(j in 1:
    #1
    length(file_names)
    ){
  tryCatch({
  #j <- 7 # 7 is a good one
  
  setwd("~/threads/Trajectories_together")
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
      #plot(data_1$load~data_1$time, pch=".", cex=1.5)
  
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
      T1<-PeaksAndValleys(data=data_2, pThresh=0, vThresh=0, maxint=500, maxPre=0.1, dyMin=0.01, dydxMax=(-1))
       setwd("~/threads/Trajectories_plots")
       png(filename = paste(file_names[j],".png", sep = ""))
       plot(data_1$load~data_1$time, pch=".", cex=1.5)
       points(T1$Pload~T1$Ptime, pch=16, cex=1.25, col='green')
       points(T1$Vload~T1$Vtime, pch=16, cex=1.25, col='red')
       dev.off()
       setwd(setwd("~/threads/Trajectories_together"))
      }else{
        setwd("~/threads/Trajectories_plots/Error")
        
      }


  
    # Write deltas to file ####
    setwd("~/threads/PeakValley")
    metadata <- c(specimen_label, date, max_load,"")
    rbind(metadata,T1)
    write.csv(x = T1, file = paste("PV",file_names[j], sep = "_"), row.names = F)
    }
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}


read.csv(file = paste("PV",file_names[1], sep = "_"), skip = 0)
#Then just use the newer lines for all the calcs




setwd("~/Documents/School and jobs/UW Biology/Research/Summer 2016/TEMPXFOOD_Roberts_2016/data/thread testing/instron_extension_load/Force_trajectories/161114")
filename <- "Specimen_RawData_3.csv"

# read in data ====

data_1 <- read.csv(file = filename, header = TRUE, stringsAsFactors = FALSE, skip = 5)
data_head <- read.csv(file = filename, header = FALSE, stringsAsFactors = FALSE)
data_head <- data_head[1:5,]
date <- data_head[1,2]
specimen_label <- data_head[2,2]
max_load <- data_head[3,2]
names(data_1)<-c("time", "extension", "load")

plot(data_1$time,data_1$load)
specimen_label


#PeaksAndValleys: creates list of all points that are peaks or valleys. 
# The list will alternate between peak and valley.

PeaksAndValleys<-function(points){
  
  PV<-data.frame(time=c(0), load=c(0))
  y1<-NULL
  y2<-NULL
  y3<-NULL
  for(i in 2:(length(points$time)-1)){
    y1<-points$load[i-1]
    y2<-points$load[i]
    y3<-points$load[i+1]
    if ((y2>y1&y2>y3) | (y2<y1&y2<y3)){
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

test1<-PeaksAndValleys(data_1)
plot(data_1$load~data_1$time, pch=".", cex=1.5)
points(test1$load~test1$time, pch=16, col="red")

# Find and plot peaks _1 ====

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

test2 <- FindPeaks(test1, magslope=.2)
test2 <- test2[test2$yDelta>=0.05,]
plot(data_1$load~data_1$time, pch=".", cex=1.5)
points(test2$Pload~test2$Ptime, pch=16, col="red")
points(test2$Vload~test2$Vtime, pch=16, col="blue")
test2$Pload[1]-test2$Vload[1]

# # Find and plot peaks _2 ====
# data_1 <- read.csv(file = "Specimen_RawData_7.csv", header = TRUE,
#                    stringsAsFactors = FALSE, skip = 5)
# names(data_1)<-c("time", "extension", "load")
# 
# PeaksAndValleys<-function(points){
#   
#   yB1<-NULL
#   yB2<-NULL
#   y0<-NULL
#   yA1<-NULL
#   yA2<-NULL
#   
#   des<-"n"
#   
#   #first iteration#
#   yB2<-points$load[1]
#   yB1<-points$load[2]
#   y0<-points$load[3]
#   yA1<-points$load[4]
#   yA2<-points$load[5]
#   
#   #checks for peak#
#   if ((yB2<yB1 & yB1<y0) & (yA2<yA1 & yA1<y0)){
#     des<-"P"
#     
#     #checks for valley
#   }else if((yB2>yB1 & yB1>y0) & (yA2<yA1 & yA1>y0)){
#     des<-"V"
#     
#     #designates as N if neither
#   }else{
#     des<-"N"
#   }
#   
#   
#   #records
#   PV<-data.frame(time=points$time[3], load=y0, designation=des)
#   
#   for(i in 4:(length(points$time)-2)){
#     
#     designation<-"n"
#     
#     yB2<-points$load[i-2]
#     yB1<-points$load[i-1]
#     y0<-points$load[i]
#     yA1<-points$load[i+1]
#     yA2<-points$load[i+2]
#     
#     #checks for peak#
#     if ((yB2<yB1 & yB1<y0) & (yA2<yA1 & yA1<y0)){
#       des<-"P"
#       
#       #checks for valley
#     }else if((yB2>yB1 & yB1>y0) & (yA2<yA1 & yA1>y0)){
#       des<-"V"
#       
#       #designates as N if neither
#     }else{
#       des<-"N"
#     }
#     
#     #records
#     PV<-rbind(PV, data.frame(time=points$time[i], load=y0, designation=des))
#     
#   }
#   return(PV)
# }
# 
# 
# InParts<-function(points, partlength=5000){
#   
#   numparts.dec<-length(points$time)/partlength
#   numparts.integer<-floor(numparts.dec)
#   
#   temp1<-NULL
#   
#   subpoints<-data.frame(time=points$time[1:partlength],
#                         load=points$load[1:partlength])
#   temp2<-PeaksAndValleys(subpoints)
#   
#   print(length(temp2$time))
#   
#   for (i in 1:(numparts.integer-1)){
#     subpoints<-data.frame(time=points$time[(i*partlength+1):((i+1)*partlength)],
#                           load=points$load[(i*partlength+1):((i+1)*partlength)])
#     temp1<-PeaksAndValleys(subpoints)
#     temp2<-rbind(temp2, temp1)
#     
#     print(length(temp2$time))
#   }
#   
#   subpoints<-data.frame(time=points$time[(i*partlength+1):length(points$time)],
#                         load=points$load[(i*partlength+1):length(points$time)])
#   temp1<-PeaksAndValleys(subpoints)
#   temp2<-rbind(temp2, temp1)
#   
#   print(length(temp2$time))
#   
#   return(temp2)
# }
# 
# 
# test1<-InParts(data_1, partlength=5000)
# peaks<-test1[(test1$designation=="P"),]
# valleys<-test1[(test1$designation=="V"),]
# plot(data_1$load~data_1$time, pch=".", cex=1.5)
# points(peaks$load~peaks$time, pch=16, col="green")
# points(valleys$load~valleys$time, pch=16, col="red")
# 
# test2<-FindPeaks(test1, magslope=1)
# plot(data_1$load~data_1$time, pch=".", cex=1.5)
# points(test2$Pload~test2$Ptime, pch=16, col="red")
# points(test2$Vload~test2$Vtime, pch=16, col="blue")



#==== Step through ====
require("gtools")

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
# Stop the fcns here ====


setwd("~/threads")
write.csv(cbind(test2,QC), file = paste("deltas",specimen_label,".csv",sep=""))

test2_QC <- cbind(test2,QC)
del <- test2_QC[test2_QC$num_th_broke == 1,]
del_d <- del$yDelta[!is.na(del$yDelta)]
del_d <- del_d[del_d<=0.3]
del_d <- del_d[del_d<=0.3]
d <- density(del_d)
plot(d)
hist(del_d, breaks=c(-.025,0,.025,.05,.075,.1,.125,.15,.175,.2,.225,.25,.275))
i = which.max(d$y)
M2 = d$x[i]
M2
#input <- read.csv(summary.csv)
output <- data.frame(
  date = data_head[1,2],
  filename = filename,
  specimen_label = data_head[2,2],
  max_load = data_head[3,2],
  dist_max = M2,
  median = median(del_d),
  num_threads_used = length(del_d)
)
input <- read.csv(file = paste("1","summary.csv"), header=T)
summary <- rbind(input[,2:8], output)
write.csv(summary, file = paste("1","summary.csv"))

plot(data_1$load~data_1$time, pch=".", cex=1.5)
points(del$Pload~del$Ptime, pch=16, col="pink")
points(del$Vload~del$Vtime, pch=16, col="green")

# Step through questionable threads ====
str(test2)
n <- nrow(test2)
i <- 1
done <- "n"
question <- test2_QC[QC[,2]=="q",]
QC_q <- cbind(rep(x =0,times= nrow(question)),rep(x =0,times= nrow(question)))
n <- nrow(QC_q)
repeat{
  plot(data_1$load~data_1$time, pch=".", cex=1.5, 
       xlim=c(question$Ptime[i]-10, question$Ptime[i]+10), 
       ylim =c(question$Vload[i]-.1,question$Pload[i]+.1))
  points(question$Pload[i]~question$Ptime[i], pch=16, col="pink")
  points(question$Vload[i]~question$Vtime[i], pch=16, col="green")
  QC_q[i,1] <- ask(msg = "Is this a delta? (y=y,n=blank,break=b,undo=u,skip10=s)")
  if(QC_q[i,1] == "b"){
    break
  }
  if(QC_q[i,1]=="u"){
    i <- i-3
  } 
  if(QC_q[i,1]=="s"){
    i <- i+10
  } 
  if(QC_q[i,1] == "y"){
    QC_q[i,2] <- ask(msg = "How many threads broke? (single = s, multiple = m, unknown = u, NA=NA)")
  } else QC_q[i,2] <- NA
  i = i+1
  if(i > n){
    break
  }
}


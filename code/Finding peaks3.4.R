#setwd("~/threads")

data_1 <- read.csv(file = "Specimen_RawData_7.csv", header = TRUE, stringsAsFactors = FALSE, skip = 5)
names(data_1)<-c("time", "extension", "load")

FindValley<-function(points, Vthresh){

	m<-1
	p1<-NULL
	p2<-NULL
	slope2<-NULL
	
	repeat{
		p1<-points$load[m]
		p2<-points$load[m+1]
		slope2<-p2-p1
		
		if(slope2>=Vthresh){
			break
		}else if (m==(length(points$load)-1)){
			m<-0
			break
		}else{
			m<-m+1
		}
	}
	return(m)
}


#dyMin is the magnitude and should be positive or 0, dydxMax is the slope, should be 0 or#
PeaksAndValleys<-function(data, pThresh=0, vThresh=0, maxint=500, dyMin=0, dydxMin=(-0.5)){
	y0<-NULL
	yA1<-NULL
	slope1<-NULL
	Vx<-NULL
	n<-NULL
	PV<-NULL

	#first peak/valley pair#
	n<-0
	repeat{
		n<-n+1
		y0<-data$load[n]
		yA1<-data$load[n+1]
		slope1<-yA1-y0

		#check for peak#
		if (slope1<pThresh){
			
			#finds following valley#
			Vx<-FindValley(points=data[(n+1):(n+maxint),], Vthresh=vThresh)
			
			#prints warning if does not find valley in the segment of points examined, stops loop#
			if (Vx==0){
				print("past maxint")
				break

			#compares dy tp dyMin and dydx to dydxMin#
			}else if ((data$load[n]-data$load[n+Vx])>=dyMin & ((data$load[n+Vx]-data$load[n])/(data$time[n+Vx]-data$time[n]))<=dydxMin){
			
				#records peak/valley pair, advances n#
				PV<-data.frame(Ptime=data$time[n], Pload=data$load[n], Vtime=data$time[n+Vx], Vload=data$load[n+Vx])
				n<-n+Vx
				break

			}

		}

	}
	
		
	repeat{
		n<-n+1

		if((n+1)>length(data$time)){
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
			
			#finds following valley#
			Vx<-FindValley(points=data[(n+1):(n+maxint),], Vthresh=vThresh)
			
			#prints warning if does not find valley in the segment of points examined, prints n to show if its because it reached the end, stops loop#
			if (Vx==0){
				print("past maxint")
				print(paste(n, "/", length(data$time)))
				break

			#compares dy tp dyMin and dydx to dydxMin#
			}else if ((data$load[n]-data$load[n+Vx])>=dyMin & ((data$load[n+Vx]-data$load[n])/(data$time[n+Vx]-data$time[n]))<=dydxMin){
			
				#records peak/valley pair, advances n#
				PV<-rbind(PV, data.frame(Ptime=data$time[n], Pload=data$load[n], Vtime=data$time[n+Vx], Vload=data$load[n+Vx]))
				n<-n+Vx
			}
		}
	}
	
	return(PV)
}


T1<-PeaksAndValleys(data=data_1, pThresh=0, vThresh=0, maxint=500, dyMin=0.05, dydxMin=(-1))
plot(data_1$load~data_1$time, pch=".", cex=1.5, xlim=c(150, 625))
points(T1$Pload~T1$Ptime, pch=16, cex=1.25, col='green')
points(T1$Vload~T1$Vtime, pch=16, cex=1.25, col='red')
T1 <- cbind(T1, del = T1$Pload-T1$Vload)
T2 <- T1[(T1$Pload-T1$Vload)>=-0.05,]
plot(data_1$load~data_1$time, pch=".", cex=1.5, xlim=c(150, 625))
points(T2$Pload~T2$Ptime, pch=16, cex=1.25, col='green')
points(T2$Vload~T2$Vtime, pch=16, cex=1.25, col='red')

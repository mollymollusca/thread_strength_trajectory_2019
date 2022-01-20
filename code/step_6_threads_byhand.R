#step_6_threads 
# final last plots for anything I need to do by eye 
# using web plot digitizer


# I did this set first
trajectory_file_names = c(
  "161226_1Specimen_RawData_3.csv",
  "170121_1Specimen_RawData_5.csv",
  "170121_1Specimen_RawData_8.csv",
  "170212_1Specimen_RawData_9.csv",
  "170212_3Specimen_RawData_4.csv",
  "170225_2Specimen_RawData_9.csv",
  "170226_1Specimen_RawData_2.csv",
  "170226_1Specimen_RawData_3.csv",
  "161218_2Specimen_RawData_3.csv",
  "161218_2Specimen_RawData_4.csv",
  "161218Specimen_RawData_8.csv",
  "161221_1Specimen_RawData_4.csv",
  "161221_1Specimen_RawData_9.csv",
  "170212_1Specimen_RawData_1.csv"
)

#read.csv(file = )
#deltas_PV_161104Specimen_RawData_5.csv

for(j in 1:length(trajectory_file_names)){
wd_traj <- "~/thread_strength_trajectory_2019/Trajectories_together"
setwd(wd_traj)
data_head <- read.csv(file = trajectory_file_names[j], header = FALSE, stringsAsFactors = FALSE)
data_head <- data_head[1:5,]

data_1 <- read.csv(file = trajectory_file_names[j], header = TRUE, stringsAsFactors = FALSE, skip = 5)
data_1 <- data_1[,2:4]
names(data_1)<-c("time", "extension", "load")

setwd("~/thread_strength_trajectory_2019/by_hand/plots")
pdf(file = paste(trajectory_file_names[j],".pdf",sep = ""))
plot(data_1$load~data_1$time, pch=".", cex=1.5)
dev.off()
}

# Stop here =====

# Now make the web plot digitizer files into delta files


#step_6_threads 
# final last plots for anything I need to do by eye 
# using web plot digitizer


# I did this set first
trajectory_file_names = c(
  "161226_1Specimen_RawData_3.csv",
  "170121_1Specimen_RawData_5.csv",
  "170121_1Specimen_RawData_8.csv",
  "170212_1Specimen_RawData_9.csv",
  "170212_3Specimen_RawData_4.csv",
  "170225_2Specimen_RawData_9.csv",
  "170226_1Specimen_RawData_2.csv",
  "170226_1Specimen_RawData_3.csv",
  "161218_2Specimen_RawData_3.csv",
  "161218_2Specimen_RawData_4.csv",
  "161218Specimen_RawData_8.csv",
  "161221_1Specimen_RawData_4.csv",
  "161221_1Specimen_RawData_9.csv",
  "170212_1Specimen_RawData_1.csv"
)


for(j in 1:length(trajectory_file_names)){

setwd("~/thread_strength_trajectory_2019/by_hand/WPD_output")
WPD_output <- read.csv(file = trajectory_file_names[j], header = FALSE, stringsAsFactors = FALSE, skip = 0)

Ptime <- WPD_output$V1[seq(1, length(WPD_output$V1), by = 2)]
Vtime <- WPD_output$V1[seq(2, length(WPD_output$V1), by = 2)]
Pload <- WPD_output$V2[seq(1, length(WPD_output$V2), by = 2)]
Vload <- WPD_output$V2[seq(2, length(WPD_output$V2), by = 2)]
yDelta <- Pload-Vload
del <- rep(x = "y", length.out = length(Ptime))
num_th_broke <- rep(x = 1, length.out = length(Ptime))

test2 <- data.frame(
  Ptime=Ptime,	
  Pload=Pload,	
  Vtime=Vtime,	
  Vload=Vload,	
  yDelta=yDelta,	
  del=del,	
  num_th_broke=num_th_broke
)

setwd("~/thread_strength_trajectory_2019/by_hand/deltas")
write.csv(test2, file = paste("deltas_PV_",trajectory_file_names[j], sep = ""), row.names = F)

}
  

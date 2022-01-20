# step_3_threads
# OK - want to organize this differently - create a directory for that analysis with individual folders for each thing that is going on
# this can start with step 2. This will make it really easy to know exactly what I did and also to know which trajectories I need to do by hand. 
# Open the file and calculate deltas
# To do: still need to do QC, want to have stuff finalized before I go through that process by hand
# Also want to select below 0.05N... like 0.01 or 0.02
# 5/4/18 I think that I changed the criteria but then I wasn't convinved that everything was real... 
# I think I need to just run this again with the new criteria... a
# Also I was making a graph 
# maybe I should rename this... yikes.  

library(gtools)

rm(list=ls())

#only run 1X per analysis
sysdate <- Sys.Date()
min_thread_strength_accepted <- 0.01

# Set up run metadata ####
# Data sources
wd_traj <- "~/thread_strength_trajectory_2019/Trajectories_together"
wd_peaks <- "~/thread_strength_trajectory_2019/analysis_20190417_good/PeakValley"

# Create the following working directories
directory <- "~/thread_strength_trajectory_2019/analysis_20190417_good"
newfolder <- paste(directory,'/step_3_',sysdate, sep="")
dir.create(newfolder)

wd_del_traj_plot <- paste(newfolder,"/del_traj_plot", sep="")
wd_del_hist_plot <- paste(newfolder,"/del_hist_plot", sep="")
wd_deltas <- paste(newfolder,"/deltas", sep="")
dir.create(wd_del_traj_plot)
dir.create(wd_del_hist_plot)
dir.create(wd_deltas)



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
#setwd("~/threads")
#input_names <- read.csv(file = "Summary_template.csv", header=T, stringsAsFactors = FALSE)
input_names <- data.frame(
  date = numeric(),
  analysis_date = numeric(),
  filename = character(),
  specimen_label = character(),
  max_load = numeric(),
  dist_max = numeric(),
  median = numeric(),
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


# Open files #####
setwd(wd_peaks)
files <- list.files(full.names = TRUE, include.dirs = TRUE)
file_names <- list.files(full.names = FALSE, include.dirs = FALSE)
j<-2
#for(j in 1:length(file_names)){ ####
for(j in 3:4){
  tryCatch({
    data_1 <- 0
    test2 <- 0
    setwd(wd_traj)
    data_1 <- read.csv(file = substr(file_names[j], 4, nchar(file_names[j])),header = TRUE, stringsAsFactors = FALSE, skip = 5)
    data_1 <- data_1[,2:4]
    names(data_1)<-c("time", "extension", "load")
    
    data_head <- read.csv(file = substr(file_names[j], 4, nchar(file_names[j])), header = FALSE, stringsAsFactors = FALSE)
    (data_head <- data_head[1:5,])
    date <- data_head[2,3]
    specimen_label <- data_head[3,3]
    max_load <- data_head[4,3]
    
    setwd(wd_peaks)
    test2<-NULL
    test2 <- read.csv(file = file_names[j], skip = 0, stringsAsFactors = FALSE, header = TRUE)
    header <- test2[1,] # Right now, for some reason the metadata did not save.... so this is just a placeholder.
    test2 <- test2[1:nrow(test2),] #Right now including all rows, but once the metadata is included, it will be great to include this.

#test2 <- test2[test2$yDelta>=0.05,]
print(j)
print(files[j])
 #plot(data_1$load~data_1$time, pch=".", cex=1.5)
 #points(test2$Pload~test2$Ptime, pch=16, col="red")
 #points(test2$Vload~test2$Vtime, pch=16, col="blue")

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

setwd(wd_del_traj_plot)
png(filename = paste(file_names[j],".png", sep = ""))
plot(data_1$load~data_1$time, pch=".", cex=1.5)
points(test2$Pload~test2$Ptime, pch=16, col="red")
points(test2$Vload~test2$Vtime, pch=16, col="blue")
points(df_QC$Pload~df_QC$Ptime, pch=16, col="pink")
points(df_QC$Vload~df_QC$Vtime, pch=16, col="green")
dev.off()

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

# Write file of deltas, compute average ####
 setwd(wd_deltas)
 test2_QC <- cbind(df_QC, QC)
 write.csv(test2_QC, file = paste("deltas",file_names[j], sep = "_"), row.names = F)

 

# ## QC for individual deltas ####
  # Now only including deltas for single threads for finding the median, etc. 
  
  del_single <- test2_QC[test2_QC$del=='y' & (test2_QC$num_th_broke == "s" | test2_QC$num_th_broke == 1),]
  del_all <- test2_QC
  # Note that I could estimate the thread strength of double breaks as 1/2 the thread strength, but this seems complicated. 
  # del_est <- test2_QC$yDelta/test2_QC$num_th_broke
  del_d_single <- del_single$yDelta[!is.na(del_single$yDelta)]
  #del_d_est <- del_all$yDelta[!is.na(del_all$yDelta)]
  del_d_all <- del_all$yDelta[!is.na(del_all$yDelta)]
  
# 
 
# Delta calculations ####
#del_d <- df_QC$yDelta # see above
median_del <-NA
sd_del <- NA
i.max <- NA
M2 = NA
QC_flag <- 1
d <- NA
if(length(del_d_single)>=2){
d <- density(del_d_single)
i.max = which.max(d$y)
M2 = d$x[i.max]
M2
}
median_del <-median(del_d_single)
sd_del <- sd(del_d_single)

# Here I have an issue that I'm defining b1 through b8 by the median. 

if(length(del_d_all)>0){
 h <- hist((del_d_all), breaks=seq(from=0,to=1,by = 0.05))
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

 setwd(wd_del_hist_plot)
 # plot histogram ####
 png(filename = paste(file_names[j],".png", sep = ""))
  par(mfrow = c(3,1))
  if(!is.na(d[1])){
   plot(d)
  } else {hist(del_d_all, breaks=seq(from=0,to=2,by = 0.05))}
  hist(del_d_all, breaks=seq(from=0,to=2,by = 0.05))
  del_d_b1 <- NA
  del_d_b1<- del_d[del_d<2*med]
  hist(del_d_b1, breaks=seq(from=0,to=2,by = 0.05))
 dev.off()
 
 mean(del_d_b1)

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

if(b1>=1){
output_vec <- data.frame(
  date = date,
  analysis_date = sysdate,
  filename = file_names[j],
  specimen_label = specimen_label,
  max_load = max_load,
  dist_max = M2,
  median = median_del,
  variance = sd_del,
  num_threads_used = length(del_d),
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

# step_3_threads
# Open the file and calculate deltas
# To do: still need to do QC, want to have stuff finalized before I go through that process by hand
# Also want to select below 0.05N... like 0.02
# 5/4/18 I think that I changed the criteria but then I wasn't convinved that everything was real... 
# I think I need to just run this again with the new criteria... a
# Also I was making a graph 
# maybe I should rename this... yikes.  

rm(list=ls())

# Open files #####
setwd("~/threads/PeakValley")
files <- list.files(full.names = TRUE, include.dirs = TRUE)
file_names <- list.files(full.names = FALSE, include.dirs = FALSE)
j<-11
for(j in 1:length(file_names)){
  tryCatch({
    data_1 <- 0
    test2 <- 0
    setwd("~/threads/Trajectories_together")
    data_1 <- read.csv(file = substr(file_names[j], 4, nchar(file_names[j])),header = TRUE, stringsAsFactors = FALSE, skip = 5)
    data_1 <- data_1[,2:4]
    names(data_1)<-c("time", "extension", "load")
    
    data_head <- read.csv(file = substr(file_names[j], 4, nchar(file_names[j])), header = FALSE, stringsAsFactors = FALSE)
    (data_head <- data_head[1:5,])
    date <- data_head[2,3]
    specimen_label <- data_head[3,3]
    max_load <- data_head[4,3]
    
    setwd("~/threads/PeakValley")
    test2<-NULL
    test2 <- read.csv(file = file_names[j], skip = 0, stringsAsFactors = FALSE, header = TRUE)
    header <- test2[1,] # Right now, for some reason the metadata did not save.... so this is just a placeholder.
    test2 <- test2[1:nrow(test2),] #Right now including all rows, but once the metadata is included, it will be great to include this.

#test2 <- test2[test2$yDelta>=0.05,]
print(j)
print(files[j])
# plot(data_1$load~data_1$time, pch=".", cex=1.5)
# points(test2$Pload~test2$Ptime, pch=16, col="red")
# points(test2$Vload~test2$Vtime, pch=16, col="blue")

yDelta <- NULL
yDelta <- test2$Pload-test2$Vload
df1 <- cbind(test2,yDelta)
df2a <- df1[df1$Vload>-.02,]
df2a$Vload[df2a$Vload<0] <- 0
df2 <- df1[df2a$Pload>0.04,]
df3 <- df2[df2$yDelta<=0.5,]
df4 <- df3[df3$yDelta>=0.04,]
df_QC <- df4[!is.na(df4$Ptime),]

setwd("~/threads/Trajectories_plots_step3")
png(filename = paste(file_names[j],".png", sep = ""))
plot(data_1$load~data_1$time, pch=".", cex=1.5)
points(test2$Pload~test2$Ptime, pch=16, col="red")
points(test2$Vload~test2$Vtime, pch=16, col="blue")
points(df_QC$Pload~df_QC$Ptime, pch=16, col="pink")
points(df_QC$Vload~df_QC$Vtime, pch=16, col="green")
dev.off()

test2<-df_QC

# # Check peaks individually ====
# str(test2)
# n <- nrow(test2)
# QC <- data.frame(
#   del = rep(x = 0,times=n),
#   num_th_broke = rep(x=0,times=n)
# )
# i <- 1
# #done <- "n"
# n <- nrow(QC)
# repeat{
#   plot(data_1$load~data_1$time, pch=".", cex=1.5,
#        xlim=c(test2$Ptime[i]-10, test2$Ptime[i]+10),
#        ylim =c(test2$Vload[i]-.1,test2$Pload[i]+.1))
#   points(test2$Pload[i]~test2$Ptime[i], pch=16, col="pink")
#   points(test2$Vload[i]~test2$Vtime[i], pch=16, col="green")
#   QC[i,1] <- ask(msg = "Is this a delta? (y=y,n=blank,undo=u,\n skip10=s,stop and break=b,)")
#   if(QC[i,1] == "b"){
#     break
#   }
#   else if(QC[i,1]=="u"){
#     i <- i-3
#   }
#   else if(QC[i,1]=="s"){
#     i <- i+10
#   }
#   else if(QC[i,1] == "y"){
#     QC[i,2] <- ask(msg = "How many threads broke? (single = s, multiple = m, unknown = u, NA=NA)")
#   } else QC[i,2] <- NA
#   i = i+1
#   if(i > n){
#     break
#   }
# }
# # Stop here! ====
# 
# 
# 
# 
# # Write file of deltas, compute average====
 setwd("~/threads/PeakValleyQC")
 write.csv(test2, file = paste("PV",file_names[j], sep = "_"), row.names = F)

 

# ## QC for individual deltas ####
#  test2_QC <- cbind(test2,yDelta, QC)
#  del <- test2_QC[test2_QC$num_th_broke == "s" | test2_QC$num_th_broke == 1,]
#  del_d <- del$yDelta[!is.na(del$yDelta)]
# 
# Delta calculations ####

del_d <- df_QC$yDelta
median_del <-9999
sd_del <- 9999
if(length(del_d)>1){
d <- density(del_d)
plot(d)
hist(del_d, breaks=c(0,.05,.1,.15,.2,.25,.3,.35,.4))
median_del <-median(del_d)
sd_del <- sd(del_d)
}
M2 = 9999
if(length(del_d)>0){
i = which.max(d$y)
M2 = d$x[i]
M2}

sysdate <- Sys.Date()
#input <- read.csv(summary.csv)
#format(betterDates, "19%y-%m-%d")
output <- NULL
output <- data.frame(
  date = date,
  analysis_date = format(sysdate, "%m/%d/%y"),
  filename = file_names[j],
  specimen_label = specimen_label,
  max_load = max_load,
  dist_max = M2,
  median = median_del,
  variance = sd_del,
  num_threads_used = length(del_d)
)
setwd("~/threads")
input <- read.csv(file = paste("2","summary.csv"), header=T, stringsAsFactors = FALSE)
head(input)
summary <- rbind(input[,2:10], output)
write.csv(summary, file = paste("2","summary.csv"))

#plot(data_1$load~data_1$time, pch=".", cex=1.5)
# plot(del$Pload~del$Ptime, pch=16, col="pink", cex = .5)
# points(del$Vload~del$Vtime, pch=16, col="green")

  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
}


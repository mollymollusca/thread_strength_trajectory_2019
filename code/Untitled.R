###Untitled...

setwd("~/thread_strength_trajectory_2019/analysis_20190501/step_5/all_deltas_incl.byhand")
deltas <- read.csv("deltas_PV_161226_1Specimen_RawData_3.csv")
deltas <- read.csv("deltas_PV_170212_1Specimen_RawData_9.csv")
deltas <- read.csv("deltas_PV_170226_2Specimen_RawData_8.csv")

setwd("~/thread_strength_trajectory_2019/Trajectories_together")
df_force <- read.csv("170226_2Specimen_RawData_8.csv", stringsAsFactors = FALSE, skip = 5)
head(df_force)
new_df <- df_force[df_force$X.mm.>15,]
new_df$X.mm. <- new_df$X.mm.-15
plot(new_df$X.mm.,new_df$X.N., pch = ".")

good_del <- deltas[deltas$del=="y",]

# Plot with time
plot(new_df$X.s.,new_df$X.N., pch = ".")
points(good_del$Ptime, good_del$Pload, col = "red")
points(good_del$Vtime, good_del$Vload, col = 'blue')
d <- density(good_del$yDelta, from = 0)

Pmm <- new_df[new_df$X.s.==good_del$Ptime,]

lm(new_df$X.mm.~new_df$X.s.)

good_del$Pmm <- .08333*good_del$Ptime-14.99979
good_del$Vmm <- .08333*good_del$Vtime-14.99979


plot(new_df$X.mm.,new_df$X.N., pch = ".",
     ylab = "Force (N)",
     xlab = "Extension (mm)")
points(good_del$Pmm, good_del$Pload, col = "green")
points(good_del$Vmm, good_del$Vload, col = 'purple')

nrow(good_del)

good_del_short <- good_del[1:7,]
nrow(good_del_short)

#good_del <- good_del_short

d <- density(good_del$yDelta, from = 0)


plot(d,
     xlab = "Breaking strength (delta load)",
     main = "")
median_del <- median(good_del$yDelta)
#median_del <- mean(good_del$yDelta)
 yline <- seq(.1,6.9,by=.1)
 xline <- rep(x = median_del,length(yline))
 lines(x = xline, y = yline)
# points(x = median_del, 6.72)
max_y <- max(d$y)
max_x <- d$x[d$y==max(d$y)]
#points(max_x, max_y)
yline <- seq(.1,6.9,by=.1)
xline <- rep(x = max_x,length(yline))
#lines(x = xline, y = yline, lty = 2)




hist(good_del$yDelta)

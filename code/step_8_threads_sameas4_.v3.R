#step_8: organization for stats and plotting ####

# oops Stats v1 is the more recently updated one (6/16/19) with REML = TRUE, but v2 is the one that is more organized. :(
# Note that I used the random intercept model, but maybe including Food as a slope is more appropriate since there is a lower AIC

# This is the same as step 4, but I'm also incorporating the inputed data
# Now use the labels from the run_list - match using the file name. 
# It looks like I'm missing some files from one of the days.... FIND THESE IF I CAN... I don't know where... maybe the computer?
# and stats and plotting ####
# This versin (v2) is more organized. 

# It's important that there aren't extra NAs in the excel sheet. This was screwing everything up. 



require(lme4)
require(MASS)
require(car)
require(ggplot2)
library(tidyr) # For command spread convert long form into wide form
require(stats)
require(effects)
require(plyr)


rm(list=ls())

#only run 1X per analysis
sysdate <- Sys.Date()

# Set file locations and names
# Inputs
wd_thread_labels <- "~/thread_strength_trajectory_2019"
file_thread_labels <- "thread_labels.csv"

wd_run_list <- "~/thread_strength_trajectory_2019"
file_run_list <- "Tensile_test_observation_data.csv"

# Change the summary file on both lines (end of the first line)
wd_summary <- "~/thread_strength_trajectory_2019/analysis_20190501/step_7/summary_2019-05-06"
file_summary <- "summary2019-05-06_check20190611.csv"
# There are about 230 samples. There were about 250 runs (I thought there were about 300 previously). 

wd_treatment_levels <- "~/thread_strength_trajectory_2019"
file_treatment_levels <- "treatment_levels.csv"

# Outputs
newfolder <- paste(wd_summary,'/step_4_',sysdate, sep="")
dir.create(newfolder)

annotated_summary <- paste('annotated_summary','_',sysdate,'.csv', sep="")

# Write metadata to file ####
comment(newfolder) <- "Including about 230 samples here."

meta <- data.frame(
  comment = comment(newfolder),
  sysdate = sysdate,
  wd_thread_labels = wd_thread_labels, 
  wd_summary = wd_summary, 
  file_summary = file_summary,
  wd_treatment_levels = wd_treatment_levels,
  wd_analysis = newfolder #output goes here
)
setwd(newfolder)
write.csv(meta, file = paste("metadata",sysdate,".csv", sep = ""), row.names = FALSE)


# Import datasheets ####
# Import thread label information #
setwd(wd_run_list)
run_list <- read.csv(file = file_run_list, stringsAsFactors = FALSE)
#run_list$Filename[run_list$Filename=="",] <- NA
names(run_list) <- c("Run_number", "date", "filename", "sample_name", "Mesocosm", "level_plexiglass",
                     "species", "specimen_ID", "run", "duplicate", "ob2", "ob3","ob4","ob5","ob6", "ob7",
                     "other", "other_count", "single_threads_observed", "total_threads_observed","one_good_single_break",
                     "max_load_recorded", "single_threads_broke_on_run", "groups_2_or_more_threads", "single_threads_available_QC",
                     "max_load_QC", "notes","byssus_damage", "labelling.notes")
run_list<- run_list[!is.na(run_list$Mesocosm),]
NAs <- run_list[is.na(run_list$Mesocosm),] #These are just all extra rows with NAs in them


# Import delta summary worksheet #
setwd(wd_summary)
data <- read.csv(file = file_summary, stringsAsFactors = FALSE)
head(data)

# Import treatment levels of mesocosms #
setwd(wd_treatment_levels)
treatment_levels <- read.csv(file = file_treatment_levels, stringsAsFactors = FALSE)
head(treatment_levels)
# Merge everything ####

# Merge the datasheet, run_list, with the output of the thread strength analysis:
new.df <- join(run_list, data, by = "filename", type = "full", match = "first") #Get same result with match = first and all, and with type = full or left. Yay, this means I entered everything correctly. 
nrow(new.df)
new.df <- join(run_list, data, by = "filename", type = "left", match = "first") #Get same result with match = first and all, and with type = full or left. Yay, this means I entered everything correctly. 
nrow(new.df)
head(new.df)

# Remove extra rows that are duplicate runs of the same sample:
all.data <- new.df[new.df$duplicate!= 1,]
all.data <- all.data[new.df$sample_name != "test",] #removes the very first sample, which was a test
all.data<- all.data[!is.na(all.data$Mesocosm),]
NAs <- all.data[is.na(all.data$Mesocosm),] #These are just all extra rows with NAs in them


# Merge with the mesocosm seawater conditions datasheet
total_2 <- join(x = all.data, y = treatment_levels, by = "Mesocosm", type = "left", match = "first" )


# Check data completion 
total_2$species <- as.factor(total_2$species)
table(total_2$species, total_2$Mesocosm)

total_2$Food <- as.factor(total_2$Food)
total_2$deg.C <- as.factor(total_2$deg.C)
total_2$Mesocosm <- as.factor(total_2$Mesocosm)

total_2$species <- as.factor(total_2$species)
total_2$total_threads_observed <- as.numeric(total_2$total_threads_observed)
total_2$max_load <- as.numeric(total_2$max_load)
total_2$median <- as.numeric(total_2$median)
total_2$median_last5 <- as.numeric(total_2$median_last5)

# Final QC ####

# For the max load analysis, remove bad max loads (damaged byssus, some threads cut prior):
# 1: If some threads are damaged, then this is flagged with a 1. 
# 2: If many or most threads are damaged, then this is flagged with a 2.
# 3: If the root slipped, then this is flagged with a 3. Usually this is marked as a duplicate as well, and removed earlier on. 
# 4: If there is some other reason to exclude this run, it is marked with a 4. 
total_2_max <- total_2[total_2$byssus_damage==0|total_2$byssus_damage==1,]
# For the single thread strength analysis, remove samples with no single threads available:
# 0: If there are no single threads, this is flagged with a 0. 
# 1: If there are single threads available, this is flagged with a 1. 
# 2: If there isn't very much info marked, and it's hard to tell from the datasheet 
# whether this run is good or not, this is marked with a 2. These are included in the analysis. 
total_2_single_th <- total_2[total_2$single_threads_available_QC==1|total_2$single_threads_available_QC==2,]

#This just gets included
# [total_2$one_good_single_break!=NA,]
nrow(total_2)
nrow(total_2_max)
nrow(total_2_single_th)

head(total_2_max)





# For the max load analysis, 
# replace the max load for a few select samples that the software missed.
total_2_max$max_load[!is.na(total_2_max$max_load_check)] <- total_2_max$max_load_check[!is.na(total_2_max$max_load_check)]
total_2_max$max_load[total_2_max$max_load_check_NA==1] <- NA

total_2_max$max_load[!is.na(total_2_max$max_load_recorded)] <- total_2_max$max_load_recorded[!is.na(total_2_max$max_load_recorded)]
#Also need to get rid of "NA"s here. 


plot(total_2_max$Mesocosm, total_2_max$max_load)
# If I need to include my values of breaks this is how I could do this from the datasheet:
#total_2_single_th$median[!is.na(total_2_single_th$one_good_single_break),] <- total_2_single_th$one_good_single_break[!is.na(total_2_single_th$one_good_single_break)]



plot(total_2_max$Mesocosm, total_2_max$max_load)
gallo_m <- total_2_max[total_2_max$species == "G",]
tross_m <- total_2_max[total_2_max$species == "T",]


plot(as.factor(paste(gallo_m$Food,gallo_m$deg.C, sep = "")), gallo_m$max_load)
plot(as.factor(paste(tross_m$Food,tross_m$deg.C, sep = "")), tross_m$max_load)

gallo_single_th <- total_2_single_th[total_2_max$species == "G",]
tross_single_th <- total_2_single_th[total_2_max$species == "T",]

plot(as.factor(paste(gallo_single_th$Food,gallo_single_th$deg.C, sep = "")), gallo_single_th$max_load)
plot(as.factor(paste(tross_single_th$Food,tross_single_th$deg.C, sep = "")), tross_single_th$max_load)

table(gallo_m)


setwd(newfolder)
write.csv(file = annotated_summary, x = total_2)

png(file = "mesocosm_plot_all_spp.png")
plot(as.numeric(total_2$Run_number),as.numeric(total_2$Mesocosm), cex = 1, pch = '.')
dev.off()

# One way to get rid of variation below a certain size of break... 
# total_2$median[total_2$median<=0.03,] <- 0

# Stats assumptions  ####
library(car)
require(graphics)
# Normality + Equal variance (bartlet)

# Max load
qqnorm(total_2_max$max_load) # not normal
qqnorm(log(total_2_max$max_load+1,10)) # not normal
# Use this log, since it is more normalized. 
qqnorm(log(total_2_max$max_load,10))

bartlett.test(total_2_max$max_load ~ paste(total_2_max$deg.C,total_2_max$Food))
bartlett.test(log(total_2_max$max_load+1,10) ~ paste(total_2_max$deg.C,total_2_max$Food))
# Use this log, since variances are then equal: 
bartlett.test(log(total_2_max$max_load,10) ~ paste(total_2_max$deg.C,total_2_max$Food))


tt2_spp <- total_2_max[total_2_max$species=="T",]
boxplot(tt2_spp$max_load~ as.factor(paste(tt2_spp$deg.C,tt2_spp$Food, sep = "")))
tt2_spp <- total_2_max[total_2_max$species=="G",]
boxplot(tt2_spp$max_load~ as.factor(paste(tt2_spp$deg.C,tt2_spp$Food, sep = "")))


# Check number of replicates
total_2_max <- total_2_max[!is.na(total_2_max$max_load),]
table_max_load <- table(total_2_max$deg.C,total_2_max$Food, total_2_max$species)
write.csv(file = "replicate_num_max.csv", table_max_load)
#     H  L
# 12 29 32
# 15 27 31
# 18 20 25
# 21 17 19


# Number of attached threads
qqnorm(total_2_max$total_thread_breaks) # not normal
qqnorm(sqrt(total_2_max$total_thread_breaks)) # normal
bartlett.test(total_2_max$total_thread_breaks ~ paste(total_2_max$deg.C,total_2_max$Food))
bartlett.test(sqrt(total_2_max$total_thread_breaks) ~ paste(total_2_max$deg.C,total_2_max$Food))
plot(total_2_max$total_thread_breaks[total_2_max$species=="G"])
points(total_2_max$total_thread_breaks[total_2_max$species=="T"], col = "blue")
par(mfrow = c(1,2))
tt2_spp <- total_2_max[total_2_max$species=="T",]
boxplot(tt2_spp$total_thread_breaks~ as.factor(paste(tt2_spp$deg.C,tt2_spp$Food, sep = "")))
tt2_spp <- total_2_max[total_2_max$species=="G",]
boxplot(tt2_spp$total_thread_breaks~ as.factor(paste(tt2_spp$deg.C,tt2_spp$Food, sep = "")))


# Thread strength
qqnorm(total_2_single_th$median_last5)
qqnorm(log(total_2_single_th$median_last5+.1,10))

bartlett.test(total_2_single_th$median_last5 ~ paste(total_2_single_th$deg.C,total_2_single_th$Food))

qqnorm(total_2_single_th$median)
bartlett.test(total_2_single_th$median ~ paste(total_2_single_th$deg.C,total_2_single_th$Food))
qqnorm(log(total_2_single_th$median+1,10))
bartlett.test(total_2_single_th$median ~ paste(total_2_single_th$deg.C,total_2_single_th$Food))
bartlett.test(log(total_2_single_th$median+1,10) ~ paste(total_2_single_th$deg.C,total_2_single_th$Food))
# Thread strength was not normal, but ANOVA is robust to this. 


tt2_spp <- total_2_single_th[total_2_single_th$species=="T",]
boxplot(tt2_spp$median~ as.factor(paste(tt2_spp$deg.C,tt2_spp$Food, sep = "")))
tt2_spp <- total_2_single_th[total_2_single_th$species=="G",]
boxplot(tt2_spp$median~ as.factor(paste(tt2_spp$deg.C,tt2_spp$Food, sep = "")))

par(mfrow = c(1,1))
total_2_single_th <- total_2_single_th[!is.na(total_2_single_th$median),]
table_single_thread <- table(total_2_single_th$deg.C,total_2_single_th$Food, total_2_single_th$species)
write.csv(file = "replicate_number_single_thread.csv",table_single_thread)

#     H  L
# 12 29 29
# 15 26 30
# 18 25 23
# 21 19 19
total_2_single_th[total_2_single_th$deg.C==12&total_2_single_th$Food=="L"&total_2_single_th$median>=.1,c("filename","median","single_breaks")]
total_2_single_th[total_2_single_th$deg.C==21&total_2_single_th$Food=="L"&total_2_single_th$median>=.1,c("filename","median","single_breaks")]
total_2_single_th[total_2_single_th$deg.C==12&total_2_single_th$Food=="L"&total_2_single_th$median>=.1,c("filename","median","single_breaks")]
total_2_single_th[total_2_single_th$deg.C==21&total_2_single_th$Food=="L"&total_2_single_th$median>=.1,c("filename","median","single_breaks")]

true_median <- total_2_single_th$median
# total_2_single_th$median <- true_median
# total_2_single_th$median[total_2_single_th$median<=.03] <- .03
# #total_2_single_th$median <- total_2_single_th$median_cutoff
# 
# qqnorm(total_2_single_th$median)
# bartlett.test(total_2_single_th$median ~ paste(total_2_single_th$deg.C,total_2_single_th$Food))
# qqnorm(log(total_2_single_th$median+1,10))
# bartlett.test(total_2_single_th$median ~ paste(total_2_single_th$deg.C,total_2_single_th$Food))
# bartlett.test(log(total_2_single_th$median+1,10) ~ paste(total_2_single_th$deg.C,total_2_single_th$Food))





# Log transform the data
# tt is short for "treatment vector transformed"
tt_max <- data.frame(
  trans_thread_count = sqrt(total_2_max$total_thread_breaks),
  trans_max = log(total_2_max$max_load, 10),
  Temp = factor(total_2_max$deg.C),
  Food = factor(total_2_max$Food, levels = c("L","H")),
  Mesocosm = factor(total_2_max$Mesocosm),
  species = factor(total_2_max$species)
)
tt_max <- tt_max[!is.na(tt_max$trans_max),]
tt_max_nas <- tt_max[is.na(tt_max$trans_max),]

tt_single_th <- data.frame(
  transvec = log10(total_2_single_th$median+1),
  transvec_last5 = log10(total_2_single_th$median_last5+1), # Include the last 5 thread breaks
  Temp = factor(total_2_single_th$deg.C),
  Food = factor(total_2_single_th$Food, levels = c("L","H")),
  Mesocosm = factor(total_2_single_th$Mesocosm),
  species = factor(total_2_single_th$species)
)
tt_single_th <- tt_single_th[!is.na(tt_single_th$transvec),]
tt_single_th_nas <- tt_single_th[is.na(tt_single_th$transvec),]

tt_all <- data.frame(
  trans_thread_count = sqrt(total_2$total_thread_breaks),
  trans_max = log10(total_2$max_load),
  transvec = log10(total_2$median+1),
  transvec_last5 = log10(total_2$median_last5+1),
  Temp = factor(total_2$deg.C),
  Food = factor(total_2$Food, levels = c("L","H")),
  Mesocosm = factor(total_2$Mesocosm),
  species = factor(total_2$species)
)
tt_all <- tt_all[!is.na(tt_all$transvec),]
tt_all_nas <- tt_all[is.na(tt_all$transvec),]

# ===
# Means!!! ####
mean(total_2_single_th$median[total_2_single_th$species == "T"], na.rm = TRUE)
sd(total_2_single_th$median[total_2_single_th$species == "T"], na.rm = TRUE)/sqrt(length(total_2_single_th$median[total_2_single_th$species == "T"])-1)


mean(total_2_single_th$median[total_2_single_th$species == "G"], na.rm = TRUE)
sd(total_2_single_th$median[total_2_single_th$species == "G"], na.rm = TRUE)/sqrt(length(total_2_single_th$median[total_2_single_th$species == "G"])-1)

# Stats!!! v1 ####

#non-hierarchical check
fun_Mesocosm <- lm(transvec ~ species * Temp * Food, data=tt_single_th)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)
# species X food interaction

#nested model
fun_Mesocosm <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm), data=tt_single_th, REML = TRUE)
fun_Mesocosm <- lme(transvec ~ species * Temp * Food, random = ~ 1 | Mesocosm, data=tt_single_th)

summary(fun_Mesocosm)
Anova(fun_Mesocosm)
# Nested and un-nested give the same result
# species X food interaction

setwd(newfolder)
write.csv(file = "new.nested_thread_strength_output_both_spp.csv", Anova(fun_Mesocosm))


# both species, thread strength, comparison
m0 <- glm(transvec ~ species * Temp * Food, data=tt_single_th)
m1 <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm), data=tt_single_th, REML = FALSE)
m2 <- lmer(transvec ~ species * Temp * Food + (species | Mesocosm), data=tt_single_th, REML = FALSE)
# m3 <- lmer(transvec ~ species * Temp * Food + (Temp | Mesocosm), data=tt_single_th, REML = FALSE)
m4 <- lmer(transvec ~ species * Temp * Food + (Food | Mesocosm), data=tt_single_th, REML = FALSE)
m5 <- lmer(transvec ~ species * Temp * Food + (Food | Mesocosm), data=tt_single_th, REML = FALSE)
m6 <- lmer(transvec ~ species * Temp * Food + (Food | Mesocosm) + (species | Mesocosm), data=tt_single_th, REML = FALSE)

anova(m1,m2,m4,m5,m6)
# This demonstrates the best model is not the intercept model, but the Food|Mesocosm model is "better"
summary(m3)
Anova(m3)

fun_Mesocosm <- lmer(transvec ~ species * Temp * Food + (Food | Mesocosm), data=tt_single_th, REML = TRUE)
summary(fun_Mesocosm) 
Anova(fun_Mesocosm) # This gives the same answer as above but is less stable because sometimes this model doesn't converge. 


fun_Mesocosm <- lme(scale(transvec) ~ species * Temp * Food, random = ~ Food | Mesocosm, data=tt_single_th)
fun_Mesocosm <- lme(scale(transvec) ~ species * Temp * Food, random = ~ 1 | Mesocosm, data=tt_single_th)

intervals(fun_Mesocosm)
# Issue with 
# nlme
# R^2 nlme - pseudo r-square - how good model is
# Observation - 
# REML - decide random structure
# Fixed - get rid of unnecessary
# Orthogonal comparisons
# Plot with predict

# Numeric temperature ####
# R book - McCrawley
tt_single_th$Temp.n <- as.numeric(as.character(tt_single_th$Temp))
m1 <- lme(transvec ~ species * Temp.n * Food, random =~ 1 | Mesocosm, data=tt_single_th)

Anova(m1) # This numeric temperature version gives the same species by food interaction. 


m0 <- lmer(transvec ~ 1 + (1 | Mesocosm), data=tt_single_th, REML = FALSE)
m1 <- lmer(transvec ~ species + (1 | Mesocosm), data=tt_single_th, REML = FALSE)
m2 <- lmer(transvec ~ species * Temp + (1 | Mesocosm), data=tt_single_th, REML = FALSE)
m3 <- lmer(transvec ~ species + Food + Temp + species:Food + species:Temp + Food:Temp + (1 | Mesocosm), data=tt_single_th, REML = FALSE)
m4 <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm), data=tt_single_th, REML = FALSE)

anova(m0,m1,m2,m3,m4)
m2 <- lmer(transvec ~ species * Temp + (1 | Mesocosm), data=tt_single_th, REML = TRUE)
m3 <- lmer(transvec ~ species + Food + Temp + species:Food + species:Temp + Food:Temp + (1 | Mesocosm), data=tt_single_th, REML = TRUE)
Anova(m3) # We get the same answer no matter which test we use. 


tross_all <- tt_all[tt_all$species == "T", ]
tross_max <- tt_max[tt_max$species == "T", ]
tross_single_th <- tt_single_th[tt_single_th$species == "T", ]

m0 <- lmer(transvec ~ 1 + (1 | Mesocosm), data=tross_single_th, REML = FALSE)
m1 <- lmer(transvec ~ Temp + (1 | Mesocosm), data=tross_single_th, REML = FALSE)
m2 <- lmer(transvec ~ Food + (1 | Mesocosm), data=tross_single_th, REML = FALSE)
m3 <- lmer(transvec ~ Temp * Food + (1 | Mesocosm), data=tross_single_th, REML = FALSE)

anova(m0,m1,m2,m3)

# Data: tross_single_th
# Models:
#   m0: transvec ~ 1 + (1 | Mesocosm)
# m2: transvec ~ Food + (1 | Mesocosm)
# m1: transvec ~ Temp + (1 | Mesocosm)
# m3: transvec ~ Temp * Food + (1 | Mesocosm)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# m0  3 -349.26 -342.43 177.63  -355.26                         
# m2  4 -347.62 -338.51 177.81  -355.62 0.3559      1     0.5508
# m1  6 -344.33 -330.67 178.16  -356.33 0.7065      2     0.7024
# m3 10 -338.31 -315.54 179.16  -358.31 1.9853      4     0.7385

# Gallo ####
gallo_all <- tt_all[tt_all$species == "G", ]
gallo_max <- tt_max[tt_max$species == "G", ]
gallo_single_th <- tt_single_th[tt_single_th$species == "G", ]

m0 <- gls(transvec ~ Temp + Food + Temp:Food, data=gallo_single_th, method = "REML")
m1 <- lme(transvec ~ Temp + Food + Temp:Food, random = ~ 1 | Mesocosm, data=gallo_single_th)
#m2 <- lme(transvec ~ Temp + Food + Temp:Food, random = ~ Temp | Mesocosm, data=gallo_single_th)
m3 <- lme(transvec ~ Temp + Food + Temp:Food, random = ~ Food | Mesocosm, data=gallo_single_th) # This has a lower AIC value
anova(m1,m3) #m3 nesting food within mesocosm gives a significant effect, but it is still right on the edge (p = .0494)
# Model df       AIC       BIC   logLik   Test   L.Ratio p-value
# m1     1 10 -467.9004 -441.0791 243.9502                         
# m3     2 12 -464.8950 -432.7094 244.4475 1 vs 2 0.9946146  0.6082
Anova(m3)

m0 <- lmer(transvec ~ 1 + (1 | Mesocosm), data=gallo_single_th, REML = FALSE)
m1 <- lmer(transvec ~ Temp + (1 | Mesocosm), data=gallo_single_th, REML = FALSE)
m2 <- lmer(transvec ~ Food + (1 | Mesocosm), data=gallo_single_th, REML = FALSE)
m3 <- lmer(transvec ~ Temp + Food + (1 | Mesocosm), data=gallo_single_th, REML = FALSE)
m4 <- lmer(transvec ~ Temp + Food + Temp:Food + (1 | Mesocosm), data=gallo_single_th, REML = FALSE)
anova(m0,m1,m2,m3,m4)

m1 <- lmer(scale(transvec) ~ Temp + Food + Temp:Food + (Food | Mesocosm), data=gallo_single_th, REML = TRUE)
# To troubleshoot issues: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
m1 <- lme(scale(transvec) ~ Temp + Food + Temp:Food, random = ~Food | Mesocosm, data=gallo_single_th) ### Hmmm... maybe this is the correct way to do this. 
m2 <- gls(transvec ~ Temp + Food + Temp:Food, data=gallo_single_th) # The non-nested version and REML = False give significant results
anova(m2,m1)
Anova(m1)


m1 <- lmer(transvec ~ Temp.n + Food + Temp.n:Food + Temp.n^2 + (1 | Mesocosm), data=gallo_single_th, REML = TRUE)
summary(m1)
Anova(m1)


m1 <- lme(transvec ~ Temp.n + Food + Temp.n^2, random = ~ 1 | Mesocosm, data=gallo_single_th)
summary(m1)
Anova(m1)

fun_Mesocosm <- lmer(transvec ~ Temp + Food + Temp:Food, random = ~ 1 | Mesocosm, data=gallo_single_th)
Anova(fun_Mesocosm)

setwd(newfolder)
write.csv(file = "new.ANOVA_both_sp_thread_strength_nested.csv",Anova(fun_Mesocosm))


# The random intecept model (m1) wins!

# Data: gallo_single_th
# Models:
#   m0: transvec ~ 1 + (1 | Mesocosm)
# m2: transvec ~ Food + (1 | Mesocosm)
# m1: transvec ~ Temp + (1 | Mesocosm)
# m3: transvec ~ Temp + Food + (1 | Mesocosm)
# m4: transvec ~ Temp + Food + Temp:Food + (1 | Mesocosm)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# m0  3 -532.67 -524.41 269.33  -538.67                           
# m2  4 -534.35 -523.34 271.18  -542.35 3.6833      1    0.05496 .
# m1  6 -527.52 -510.99 269.76  -539.52 0.0000      2    1.00000  
# m3  7 -529.35 -510.08 271.68  -543.35 3.8378      1    0.05011 .
# m4 10 -532.06 -504.53 276.03  -552.06 8.7085      3    0.03343 *


# Both species ####
#Check max byssal structure strength #
fun_Mesocosm_max <- lme(trans_max ~ species * Temp * Food, random = ~ 1 | Mesocosm, data=tt_max)
# fun_Mesocosm_max <- lme(trans_max ~ species + Temp + Food , random = ~ 1 | Mesocosm, data=tt_max)

summary(fun_Mesocosm_max)
Anova(fun_Mesocosm_max)
setwd(newfolder)
write.csv(file = "new.ANOVA_both_sp_max_strength_nested.csv",Anova(fun_Mesocosm_max))

#Check number of threads per mussel #
# Check different levels of pooling (it doesn't really change then answer)
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species * Temp * Food + (1 | Mesocosm), data=tt_max, REML = FALSE)
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species + Temp + Food + Temp:Food + species:Temp + species:Food + (1 | Mesocosm), data=tt_max, REML = FALSE)
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species + Temp + Food + Temp:Food + species:Temp + (1 | Mesocosm), data=tt_max, REML = FALSE)
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species + Temp + Food + species:Temp + (1 | Mesocosm), data=tt_max, REML = FALSE)


fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species * Temp * Food + (1 | Mesocosm), data=tt_max, REML = TRUE)
summary(fun_Mesocosm_thread_count)
Anova(fun_Mesocosm_thread_count)
setwd(newfolder)
write.csv(file = "ANOVA_both_attached_threads_nested.csv",Anova(fun_Mesocosm_thread_count))
# Species has an effect.

# Tross ####
tross_all <- tt_all[tt_all$species == "T", ]
tross_max <- tt_max[tt_max$species == "T", ]
tross_single_th <- tt_single_th[tt_single_th$species == "T", ]

setwd(newfolder)
pdf(file = "mesocosm_plot_tross.pdf")
par(mfrow = c(2,2))
plot(tross_all$Mesocosm, 10^tross_all$transvec - 1, ylab = "log thread strength")
plot(tross_all$Food, tross_all$transvec, ylab = "log thread strength")
plot(tross_all$Temp, tross_all$transvec, ylab = "log thread strength")
dev.off()

pdf(file = "mesocosm_plot_tross_max.pdf")
par(mfrow = c(2,2))
plot(tross_all$Mesocosm, 10^tross_all$trans_max, ylab = "log max byssus strength")
plot(tross_all$Food, 10^tross_all$trans_max, ylab = "log max byssus strength")
plot(tross_all$Temp, 10^tross_all$trans_max, ylab = "log max byssus strength")
dev.off()

str(tross_max)
fun_Mesocosm_max_tross <-lme(trans_max ~ Temp * Food, random = ~1| Mesocosm, data=tross_max)
#m1 <-lme(trans_max ~ Temp * Food, random = ~ Food | Mesocosm, data=tross_max) #Does not converge
Anova(m1)
summary(fun_Mesocosm_max_tross)
Anova(fun_Mesocosm_max_tross)
setwd(newfolder)
write.csv(file = "new.ANOVA_tross_max_load_nested.csv",Anova(fun_Mesocosm_max_tross))


fun_Mesocosm_thread_count_tross <- lmer(trans_thread_count ~ Temp * Food + (1 | Mesocosm), data=tross_max, REML = TRUE)
summary(fun_Mesocosm_thread_count_tross)
Anova(fun_Mesocosm_thread_count_tross)
# Temp decreases attached thread count in general - huh, temp didn't decrease the total number of threads
setwd(newfolder)
write.csv(file = "new.ANOVA_tross_attached_threads_nested.csv",Anova(fun_Mesocosm_thread_count_tross))

# Non-nested version for thread strength (gives the same answer)
fun_Mesocosm <- lm(transvec ~ Temp * Food, data=tross_single_th)
# plot(fun_Mesocosm)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

fun_Mesocosm <- lmer(transvec ~ Temp * Food + (1| Mesocosm), data=tross_single_th, REML = TRUE)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)
setwd(newfolder)
write.csv(file = "new.ANOVA_tross_th_strength_nested.csv",Anova(fun_Mesocosm))
tross_model <- fun_Mesocosm #Right now I'm using the not nested form because the nested version isn't plotting. 

# Gallo ####
gallo_all <- tt_all[tt_all$species == "G", ]
gallo_max <- tt_max[tt_max$species == "G", ]
gallo_single_th <- tt_single_th[tt_single_th$species == "G", ]


setwd(newfolder)
pdf(file = "mesocosm_plot_gallo.pdf")
par(mfrow = c(2,2))
plot(gallo_all$Mesocosm, 10^gallo_all$transvec - 1, ylab = "log thread strength")
plot(gallo_all$Food, 10^gallo_all$transvec-1, ylab = "log thread strength")
plot(gallo_all$Temp, 10^gallo_all$transvec-1, ylab = "log thread strength")
dev.off()

pdf(file = "mesocosm_plot_gallo_max.pdf")
par(mfrow = c(2,2))
plot(gallo_all$Mesocosm, 10^gallo_all$trans_max, ylab = "log max byssus strength")
plot(gallo_all$Food, 10^gallo_all$trans_max, ylab = "log max byssus strength")
plot(gallo_all$Temp, 10^gallo_all$trans_max, ylab = "log max byssus strength")
dev.off()


pdf(file = "Gallo_all_mesocosms.pdf")
plot(gallo_all$Mesocosm, 10^gallo_all$transvec-1)
dev.off()
str(gallo_all)
head(gallo_all)

fun_Mesocosm_max_gallo <- lme(trans_max ~ Temp * Food, random = ~ 1| Mesocosm, data=gallo_max)
summary(fun_Mesocosm_max_gallo)
Anova(fun_Mesocosm_max_gallo)
setwd(newfolder)
write.csv(file = "new.ANOVA_gallo_max_load_nested.csv",Anova(fun_Mesocosm_max_gallo))



# Nesting matters when evaluating thread count!!! 
# Not nested version - food significant effect
fun_Mesocosm_thread_count_gallo <- lm(trans_thread_count ~ Temp * Food, data=gallo_max)
summary(fun_Mesocosm_thread_count_gallo)
Anova(fun_Mesocosm_thread_count_gallo)

# Nested version - food not significant effect
fun_Mesocosm_thread_count_gallo <- lme(trans_thread_count ~ Temp * Food, random = ~ 1 | Mesocosm, data=gallo_max)
summary(fun_Mesocosm_thread_count_gallo)
Anova(fun_Mesocosm_thread_count_gallo)
setwd(newfolder)
write.csv(file = "new.ANOVA_gallo_attached_threads_nested.csv",Anova(fun_Mesocosm_thread_count_gallo))
# write.csv(file = "ANOVA_gallo_attached_threads_nested.csv",anova(fun_Mesocosm_thread_count_gallo))


# Not nested version - not significant, except interaction
fun_Mesocosm <- lm(transvec ~ Temp * Food, data=gallo_single_th)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

# Nested version - same answer
fun_Mesocosm <- lme(transvec ~ Temp * Food, random = ~ 1 | Mesocosm, data=gallo_single_th)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)
setwd(newfolder)
write.csv(file = "new.ANOVA_gallo_th_strength_nested.csv",Anova(fun_Mesocosm))

gallo_model <- fun_Mesocosm #This is nested in Mescocosm 

# Stats!!! v2 ####
require(nlme)

m0 <- gls(transvec ~ species * Temp * Food, data=tt_single_th, method = "REML")
m1 <- lme(transvec ~ species * Temp * Food, random=~1|Mesocosm, data=tt_single_th, method = "REML")
m2 <- lme(transvec ~ species * Temp * Food, random=~Food|Mesocosm, data=tt_single_th, method = "REML")
m3 <- lme(transvec ~ species * Temp * Food, random=~Temp|Mesocosm, data=tt_single_th, method = "REML")
anova(m0,m1,m2,m3)



#non-hierarchical check
fun_Mesocosm <- lme(transvec ~ species * Temp * Food, data=tt_single_th)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

#nested model
fun_Mesocosm <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm), data=tt_single_th, REML = TRUE)
Anova(fun_Mesocosm)
# Nested and un-nested give the same result

fun_Mesocosm <- lmer(transvec ~ species * Temp * Food + (Temp | Mesocosm), data=tt_single_th, REML = TRUE) # Same answer
Anova(fun_Mesocosm)

fun_Mesocosm <- lmer(transvec ~ species * Temp * Food + (Food | Mesocosm), data=tt_single_th, REML = TRUE) # Same answer
Anova(fun_Mesocosm)

m1 <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm), data=tt_single_th, REML = TRUE)
m2 <- lmer(transvec ~ species * Temp * Food + (species | Mesocosm), data=tt_single_th, REML = TRUE)
# m3 <- lmer(transvec ~ species * Temp * Food + (Temp | Mesocosm), data=tt_single_th, REML = FALSE)
m4 <- lmer(transvec ~ species * Temp * Food + (Food | Mesocosm), data=tt_single_th, REML = TRUE)
m5 <- lmer(transvec ~ species * Temp * Food + (Food | Mesocosm), data=tt_single_th, REML = TRUE)
m6 <- lmer(transvec ~ species * Temp * Food + (Food | Mesocosm) + (species | Mesocosm), data=tt_single_th, REML = FALSE)

anova(m1,m2,m4,m5,m6)
# This demonstrates the best model is not the intercept model, but the Food|Mesocosm model
summary(m3)
Anova(m3)

par(mfrow = c(1,1))
plot(scale(tt_single_th$transvec))


m0 <- glm(transvec ~ species * Temp * Food, data=tt_single_th)
m1 <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm), data=tt_single_th, REML = TRUE)
m2 <- lmer(transvec ~ species * Temp * Food + (Food | Mesocosm) , data=tt_single_th, REML = TRUE)
# m3 is the best...
m3 <- lmer(transvec ~ species * Temp * Food + (Temp | Mesocosm), data=tt_single_th, REML = TRUE)
summary(m3)
m4 <- lmer(transvec ~ species * Temp * Food + (species | Mesocosm), data=tt_single_th, REML = TRUE)
m5 <- lmer(transvec ~ species * Temp * Food + (Food | Mesocosm) , data=tt_single_th, REML = TRUE)
m6 <- lmer(transvec ~ species * Temp * Food + (1+Food | Mesocosm), data=tt_single_th, REML = TRUE)
m7 <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm)+ (1|Mesocosm:Temp), data=tt_single_th, REML = TRUE)

# lmer(Y ~ 1 + (1 | A) + (1 | A:B), data=d)
anova(m0,m1,m2,m3,m4,m5,m6,m7)
summary(m1)
Anova(m1)
intervals(m1)
# Issue with
# nlme
# R^2 nlme - pseudo r-square - how good model is
# Observation -
# REML - decide random structure
# Fixed - get rid of unnecessary
# Orthogonal comparisons
# Plot with predict


# R book - McCrawley

m1 <- lmer(transvec ~ species * as.numeric(Temp) * Food + (1 | Mesocosm) , data=tt_single_th, REML = FALSE)
m2 <- lmer(transvec ~ species * as.factor(Temp) * Food + (1 | Mesocosm) , data=tt_single_th, REML = FALSE)
anova(m1, m2)

m1 <- lmer(transvec ~ species * Temp * Food
           + (1 | Mesocosm) , data=tt_single_th, REML = TRUE)
m2 <- lmer(transvec ~ species * Temp * Food
           + (1 | Mesocosm) , data=tt_single_th, REML = FALSE)
anova(m1,m2)
summary(m1)
summary(m2)


m1 <- lmer(transvec ~ species * Temp * Food , data=tt_single_th, REML = FALSE)
m2 <- lmer(transvec ~ species * Temp * Food
           + (1 | Mesocosm) , data=tt_single_th, REML = FALSE)
m3 <- lmer(transvec ~ species * Temp * Food
           + (species | Mesocosm) , data=tt_single_th, REML = FALSE)

anova(m1,m2,m3)
#these won't run...
m3 <- lmer(transvec ~ species * Temp * Food
           + (Temp | Mesocosm) , data=tt_single_th, REML = TRUE)
m4 <- lmer(transvec ~ species * Temp * Food
           + (Food | Mesocosm) , data=tt_single_th, REML = TRUE)
m5 <- lmer(transvec ~ species * Temp * Food
           + (Temp | Mesocosm) + (Food | Mesocosm) , data=tt_single_th, REML = TRUE)
m6 <- lmer(transvec ~ species * Temp * Food
           + (species | Mesocosm) + (Temp | Mesocosm) + (Food | Mesocosm) , data=tt_single_th, REML = TRUE)
anova(m1,m2,m3,m4,m5)


m1 <- lmer(transvec ~ species * Temp * Food
           + (1 | Mesocosm) , data=tt_single_th, REML = FALSE)

m2 <- lmer(transvec ~ species * Temp * Food
           + (1 | Mesocosm) , data=tt_single_th, REML = TRUE)

summary(m1)
Anova(m1)
summary(m2)
Anova(m2)



# Analysis of Deviance Table (Type II Wald chisquare tests)
#
# Response: transvec
# Chisq Df Pr(>Chisq)
# species           3.0898  1    0.07878 .
# Temp              1.9444  3    0.58403
# Food              3.8278  1    0.05041 .
# species:Temp      0.5005  3    0.91877
# species:Food      4.6088  1    0.03181 *
#   Temp:Food         8.2673  3    0.04080 *
#   species:Temp:Food 5.7621  3    0.12377
# ---
summary(m1)
anova(m1)

# Analysis of Deviance Table (Type II Wald chisquare tests)
#
# Response: transvec
# Chisq Df Pr(>Chisq)
# species           2.7121  1    0.09959 .
# Temp              2.6052  3    0.45658
# Food              3.6011  1    0.05774 .
# species:Temp      0.3460  3    0.95116
# species:Food      4.7148  1    0.02990 *
#   Temp:Food         8.4793  3    0.03708 *
#   species:Temp:Food 5.6530  3    0.12977

m0 <- lmer(transvec ~ 1 + (1 | Mesocosm), data=tt_single_th, REML = FALSE)
m1 <- lmer(transvec ~ species + (1 | Mesocosm), data=tt_single_th, REML = FALSE)
m2 <- lmer(transvec ~ species * Temp + (1 | Mesocosm), data=tt_single_th, REML = FALSE)
m3 <- lmer(transvec ~ species * Food + (1 | Mesocosm), data=tt_single_th, REML = FALSE)
m4 <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm), data=tt_single_th, REML = FALSE)

anova(m0,m1,m2,m3,m4)

# Data: tt_single_th
# Models:
#   m0: transvec ~ 1 + (1 | Mesocosm)
# m1: transvec ~ species + (1 | Mesocosm)
# m3: transvec ~ species * Food + (1 | Mesocosm)
# m2: transvec ~ species * Temp + (1 | Mesocosm)
# m4: transvec ~ species * Temp * Food + (1 | Mesocosm)
# Df     AIC     BIC logLik deviance   Chisq Chi Df Pr(>Chisq)
# m0  3 -877.37 -867.66 441.69  -883.37
# m1  4 -877.47 -864.53 442.74  -885.47  2.0996      1   0.147340
# m3  6 -880.40 -860.98 446.20  -892.40  6.9242      2   0.031363 *
#   m2 10 -867.97 -835.60 443.98  -887.97  0.0000      4   1.000000
# m4 18 -872.49 -814.24 454.25  -908.49 20.5285      8   0.008511 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tross_all <- tt_all[tt_all$species == "T", ]
tross_max <- tt_max[tt_max$species == "T", ]
tross_single_th <- tt_single_th[tt_single_th$species == "T", ]

m0 <- lmer(transvec ~ 1 + (1 | Mesocosm), data=tross_single_th, REML = FALSE)
m1 <- lmer(transvec ~ Temp + (1 | Mesocosm), data=tross_single_th, REML = FALSE)
m2 <- lmer(transvec ~ Food + (1 | Mesocosm), data=tross_single_th, REML = FALSE)
m3 <- lmer(transvec ~ Temp * Food + (1 | Mesocosm), data=tross_single_th, REML = FALSE)

anova(m0,m1,m2,m3)

# Data: tross_single_th
# Models:
#   m0: transvec ~ 1 + (1 | Mesocosm)
# m2: transvec ~ Food + (1 | Mesocosm)
# m1: transvec ~ Temp + (1 | Mesocosm)
# m3: transvec ~ Temp * Food + (1 | Mesocosm)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# m0  3 -349.26 -342.43 177.63  -355.26
# m2  4 -347.62 -338.51 177.81  -355.62 0.3559      1     0.5508
# m1  6 -344.33 -330.67 178.16  -356.33 0.7065      2     0.7024
# m3 10 -338.31 -315.54 179.16  -358.31 1.9853      4     0.7385

gallo_all <- tt_all[tt_all$species == "G", ]
gallo_max <- tt_max[tt_max$species == "G", ]
gallo_single_th <- tt_single_th[tt_single_th$species == "G", ]

m0 <- lmer(transvec ~ 1 + (1 | Mesocosm), data=gallo_single_th, REML = FALSE)
m1 <- lmer(transvec ~ Temp + (1 | Mesocosm), data=gallo_single_th, REML = FALSE)
m2 <- lmer(transvec ~ Food + (1 | Mesocosm), data=gallo_single_th, REML = FALSE)
m3 <- lmer(transvec ~ Temp + Food + (1 | Mesocosm), data=gallo_single_th, REML = FALSE)
m4 <- lmer(transvec ~ Temp + Food + Temp:Food + (1 | Mesocosm), data=gallo_single_th, REML = FALSE)
anova(m0,m1,m2,m3,m4)


# Data: gallo_single_th
# Models:
#   m0: transvec ~ 1 + (1 | Mesocosm)
# m2: transvec ~ Food + (1 | Mesocosm)
# m1: transvec ~ Temp + (1 | Mesocosm)
# m3: transvec ~ Temp + Food + (1 | Mesocosm)
# m4: transvec ~ Temp + Food + Temp:Food + (1 | Mesocosm)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# m0  3 -532.67 -524.41 269.33  -538.67
# m2  4 -534.35 -523.34 271.18  -542.35 3.6833      1    0.05496 .
# m1  6 -527.52 -510.99 269.76  -539.52 0.0000      2    1.00000
# m3  7 -529.35 -510.08 271.68  -543.35 3.8378      1    0.05011 .
# m4 10 -532.06 -504.53 276.03  -552.06 8.7085      3    0.03343 *

setwd(newfolder)
write.csv(file = "ANOVA_both_sp_thread_strength_nested.csv",Anova(fun_Mesocosm))

# Both species ####
#Check max byssal structure strength #

# Including the transformed thread count as a covariate
m1 <- lmer(trans_max ~ species * Temp * Food + trans_thread_count + (1 | Mesocosm), data=tt_max, REML = TRUE)
Anova(m1)


m1 <- lmer(trans_max ~ species * Temp * Food  + (1 | Mesocosm), data=tt_max, REML = TRUE)
m2 <- lmer(trans_max ~ species + Temp + Food + species:Temp + species:Food + Temp:Food + (1 | Mesocosm), data=tt_max, REML = TRUE)
m3 <- lmer(trans_max ~ species + Temp + Food + (1 | Mesocosm), data=tt_max, REML = TRUE)
m4 <- lmer(trans_max ~ species + Temp + Food + Temp:species + (1 | Mesocosm), data=tt_max, REML = TRUE)
m5 <- lmer(trans_max ~ species + Temp + Food + species:Food+ (1 | Mesocosm), data=tt_max, REML = TRUE)
m6 <- lmer(trans_max ~ species + Temp + Food + Temp:Food + (1 | Mesocosm), data=tt_max, REML = TRUE)
anova(m1,m2,m3,m4,m5,m6)
# The best model selected has no interactions
fun_Mesocosm_max <- m1
summary(fun_Mesocosm_max)
Anova(fun_Mesocosm_max)
setwd(newfolder)
write.csv(file = "ANOVA_both_sp_max_strength_nested.csv",Anova(fun_Mesocosm_max))

#Check number of threads per mussel #
# Check different levels of pooling (it doesn't really change then answer)
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species * Temp * Food + (1 | Mesocosm), data=tt_max, REML = FALSE)
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species + Temp + Food + Temp:Food + species:Temp + species:Food + (1 | Mesocosm), data=tt_max, REML = FALSE)
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species + Temp + Food + Temp:Food + species:Temp + (1 | Mesocosm), data=tt_max, REML = FALSE)
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species + Temp + Food + species:Temp + (1 | Mesocosm), data=tt_max, REML = FALSE)


tt_max$Temp.n <- as.numeric(as.character(tt_max$Temp))
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species * Temp * Food + (1 | Mesocosm), data=tt_max, REML = TRUE)
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species * Temp.n * Food + (1 | Mesocosm), data=tt_max, REML = TRUE)

summary(scale(fun_Mesocosm_thread_count))
Anova(fun_Mesocosm_thread_count)
setwd(newfolder)
write.csv(file = "ANOVA_both_attached_threads_nested.csv",Anova(fun_Mesocosm_thread_count))
# Species has an effect.

# Tross ####
tross_all <- tt_all[tt_all$species == "T", ]
tross_max <- tt_max[tt_max$species == "T", ]
tross_single_th <- tt_single_th[tt_single_th$species == "T", ]

setwd(newfolder)
pdf(file = "mesocosm_plot_tross.pdf")
par(mfrow = c(2,2))
plot(tross_all$Mesocosm, 10^tross_all$transvec - 1, ylab = "log thread strength")
plot(tross_all$Food, tross_all$transvec, ylab = "log thread strength")
plot(tross_all$Temp, tross_all$transvec, ylab = "log thread strength")
dev.off()

pdf(file = "mesocosm_plot_tross_max.pdf")
par(mfrow = c(2,2))
plot(tross_all$Mesocosm, 10^tross_all$trans_max, ylab = "log max byssus strength")
plot(tross_all$Food, 10^tross_all$trans_max, ylab = "log max byssus strength")
plot(tross_all$Temp, 10^tross_all$trans_max, ylab = "log max byssus strength")
dev.off()

str(tross)
fun_Mesocosm_max_tross <- lmer(trans_max ~ Temp * Food + (1| Mesocosm), data=tross_max, REML = TRUE)
summary(fun_Mesocosm_max_tross)
Anova(fun_Mesocosm_max_tross)
setwd(newfolder)
write.csv(file = "ANOVA_tross_max_load_nested.csv",Anova(fun_Mesocosm_max_tross))

# Including the transformed thread count as a covariate
m1 <- lmer(trans_max ~ Temp * Food * trans_thread_count + (1 | Mesocosm), data=tross_max, REML = TRUE)
Anova(m1)


m1 <- lmer(trans_max ~ Temp * Food * trans_thread_count + (1 | Mesocosm), data=gallo_max, REML = TRUE)
Anova(m1)

fun_Mesocosm_thread_count_tross <- lmer(trans_thread_count ~ Temp * Food + (1 | Mesocosm), data=tross_max, REML = TRUE)
summary(fun_Mesocosm_thread_count_tross)
Anova(fun_Mesocosm_thread_count_tross)
# Temp decreases attached thread count in general - huh, temp didn't decrease the total number of threads
setwd(newfolder)
write.csv(file = "ANOVA_tross_attached_threads_nested.csv",Anova(fun_Mesocosm_thread_count_tross))


m1 <- lmer(trans_thread_count ~ Temp * Food + (1 | Mesocosm), data=tross_max, REML = FALSE)
m2 <- lmer(trans_thread_count ~ Temp + (1 | Mesocosm), data=tross_max, REML = FALSE)
anova(m1,m2)

m1 <- lmer(trans_thread_count ~ Temp * Food + (1 | Mesocosm), data=tross_max, REML = FALSE)
m2 <- lmer(trans_thread_count ~ Food + (1 | Mesocosm), data=tross_max, REML = FALSE)
anova(m1,m2)


# Non-nested version for thread strength (gives the same answer)
fun_Mesocosm <- lm(transvec ~ Temp * Food, data=tross_single_th)
# plot(fun_Mesocosm)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

fun_Mesocosm <- lmer(transvec ~ Temp * Food + (1| Mesocosm), data=tross_single_th, REML = TRUE)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)
setwd(newfolder)
write.csv(file = "ANOVA_tross_th_strength_nested.csv",Anova(fun_Mesocosm))
tross_model <- fun_Mesocosm #Right now I'm using the not nested form because the nested version isn't plotting.

# Gallo ####
gallo_all <- tt_all[tt_all$species == "G", ]
gallo_max <- tt_max[tt_max$species == "G", ]
gallo_single_th <- tt_single_th[tt_single_th$species == "G", ]


setwd(newfolder)
pdf(file = "mesocosm_plot_gallo.pdf")
par(mfrow = c(2,2))
plot(gallo_all$Mesocosm, 10^gallo_all$transvec - 1, ylab = "log thread strength")
plot(gallo_all$Food, 10^gallo_all$transvec-1, ylab = "log thread strength")
plot(gallo_all$Temp, 10^gallo_all$transvec-1, ylab = "log thread strength")
dev.off()

pdf(file = "mesocosm_plot_gallo_max.pdf")
par(mfrow = c(2,2))
plot(gallo_all$Mesocosm, 10^gallo_all$trans_max, ylab = "log max byssus strength")
plot(gallo_all$Food, 10^gallo_all$trans_max, ylab = "log max byssus strength")
plot(gallo_all$Temp, 10^gallo_all$trans_max, ylab = "log max byssus strength")
dev.off()


pdf(file = "Gallo_all_mesocosms.pdf")
plot(gallo_all$Mesocosm, 10^gallo_all$transvec-1)
dev.off()
str(gallo_all)
head(gallo_all)

fun_Mesocosm_max_gallo <- lmer(trans_max ~ Temp * Food + (1| Mesocosm), data=gallo_max, REML = TRUE)
# fun_Mesocosm_max_gallo <- lmer(trans_max ~ Temp + Food + (1| Mesocosm), data=gallo_max, REML = TRUE)

summary(fun_Mesocosm_max_gallo)
Anova(fun_Mesocosm_max_gallo)
setwd(newfolder)
write.csv(file = "ANOVA_gallo_max_load_nested.csv",Anova(fun_Mesocosm_max_gallo))



# Nesting matters when evaluating thread count!!!
# Not nested version - food significant effect
fun_Mesocosm_thread_count_gallo <- lm(trans_thread_count ~ Temp * Food, data=gallo_max)
summary(fun_Mesocosm_thread_count_gallo)
Anova(fun_Mesocosm_thread_count_gallo)

# Nested version - food not significant effect
fun_Mesocosm_thread_count_gallo <- lmer(trans_thread_count ~ Temp * Food + (1 | Mesocosm), data=gallo_max, REML = TRUE)
summary(fun_Mesocosm_thread_count_gallo)
Anova(fun_Mesocosm_thread_count_gallo)
setwd(newfolder)
write.csv(file = "ANOVA_gallo_attached_threads_nested.csv",Anova(fun_Mesocosm_thread_count_gallo))
# write.csv(file = "ANOVA_gallo_attached_threads_nested.csv",anova(fun_Mesocosm_thread_count_gallo))


# Not nested version - not significant, except interaction
fun_Mesocosm <- lm(transvec ~ Temp * Food, data=gallo_single_th)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

# Nested version - same answer
fun_Mesocosm <- lmer(transvec ~ Temp * Food + (1 | Mesocosm), data=gallo_single_th, REML = TRUE)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)
setwd(newfolder)
write.csv(file = "ANOVA_gallo_th_strength_nested.csv",Anova(fun_Mesocosm))

gallo_model <- fun_Mesocosm #This is nested in Mescocosm




# ============================================== #
# Nice plot of thread strength with both species ####
# ============================================== #
fn <- tross_model
summary(fn)
Anova(fn)
ef <- effect("Temp:Food", fn) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x <- as.data.frame(ef)
upper <- x$fit+x$se
lower <- x$fit-x$se
fn_t <- fn

m <- fn_t

coef(summary(m))
ef <- effect("Temp:Food", m) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x.1 <- as.data.frame(ef)
str(x.1)
species <- rep("M. trossulus", times = nrow(x.1))
x.1 <- cbind(species, x.1)


fn_t <- fn

fn_g <- gallo_model
m <- fn_g

coef(summary(m))
ef <- effect("Temp:Food", m) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x.2 <- as.data.frame(ef)
str(x.2)
species <- rep("M. galloprovincialis", times = nrow(x.2))
x.2 <- cbind(species, x.2)

x <- rbind(x.1,x.2)

upper <- 10^(x$fit+x$se)-1
lower <- 10^(x$fit-x$se)-1

head(x)
x.backtrans <- data.frame(
  x$species,
  x$Temp,
  x$Food,
  fit = 10^x$fit-1,
  se = 10^x$se-1,
  upper = 10^(x$fit+x$se)-1,
  lower = 10^(x$fit-x$se)-1
)
x$fit
x.backtrans$fit


shape.spe <- rep(x = 21, times = length(x$Food))
shape.spe[x$Food=="H"] <- 16

color.spe <- c(rep("blue",times = 8),rep("red",times = 8))
spe.plot <- ggplot(data = x.backtrans, aes(x$Temp, y = fit))+
  coord_cartesian(ylim = c(0, .2))+
  geom_point(stat = 'identity', 
             #position = pd, 
             color = color.spe, 
             shape = shape.spe,
             size = 3) +
  geom_errorbar(stat = "identity", 
                aes(ymax=upper, ymin=lower), 
                color = color.spe,
                #position = pd,
                data=x.backtrans, 
                width = .1) +
  #geom_line(position = pd, color = "blue")+
  #scale_fill_manual(values=c("#006600", "#66CCFF")) +
  labs(x ="Temperature (deg C)", 
       y = "Thread strength (N)") +
  #scale_y_continuous(breaks=seq(0,5, by=1))+
  #geom_hline(yintercept=0, size=1, color="black") +
  theme_classic(base_size = 12)+
  facet_wrap(~x$species, scales = "free") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        #axis.text.x=element_blank() #This is the last graph so needs the numbers
        axis.title.x=element_blank()
  )
# annotate("text", x = 3.9, y = 5, label = "Temp: p = 0.03")
# annotate("text", x = 3.9, y = 4.5, label = "Food: p = 0.02")+
# annotate("text", x = 3.9, y = 4, label = "Temp X Food: NS")
spe.plot
setwd(newfolder)
pdf(file = "Thread_strength.pdf", width = 7, height = 3)
spe.plot
dev.off()
pdf(file = "Thread_strength.png", width=500,height=200)
spe.plot
dev.off()
write.csv(file = "ANOVA_gallo_thread_strength_check.csv", x = Anova(fn_g))
write.csv(file = "ANOVA_tross_thread_strength_check.csv", x = Anova(fn_t))


# ======================================== #
# Nice plot of max load with both species ####
# ======================================== #
fn <- fun_Mesocosm_max_tross
summary(fn)
Anova(fn)
ef <- effect("Temp:Food", fn) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x <- as.data.frame(ef)
upper <- x$fit+x$se
lower <- x$fit-x$se
fn_t <- fn

m <- fun_Mesocosm_max_tross

coef(summary(m))
ef <- effect("Temp:Food", m) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x.1 <- as.data.frame(ef)
str(x.1)
species <- rep("M. trossulus", times = nrow(x.1))
x.1 <- cbind(species, x.1)

#pd <- position_dodge(2)


fn_t <- fn

fn_g <- fun_Mesocosm_max_gallo
m <- fn_g

coef(summary(m))
ef <- effect("Temp:Food", m) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x.2 <- as.data.frame(ef)
str(x.2)
species <- rep("M. galloprovincialis", times = nrow(x.2))
x.2 <- cbind(species, x.2)

x <- rbind(x.1,x.2)

upper <- 10^(x$fit+x$se)
lower <- 10^(x$fit-x$se)

head(x)
x.backtrans <- data.frame(
  x$species,
  x$Temp,
  x$Food,
  fit = 10^x$fit,
  se = 10^x$se,
  upper = 10^(x$fit+x$se),
  lower = 10^(x$fit-x$se)
)

shape.spe <- rep(x = 21, times = length(x$Food))
shape.spe[x$Food=="H"] <- 16
color.spe <- c(rep("blue",times = 8),rep("red",times = 8))
spe.plot <- ggplot(data = x.backtrans, aes(x$Temp, y =fit))+
  coord_cartesian(ylim = c(0, 1))+
  geom_point(stat = 'identity', 
             #position = pd, 
             color = color.spe, 
             shape = shape.spe,
             size = 3) +
  geom_errorbar(stat = "identity", 
                aes(ymax=upper, ymin=lower), 
                color = color.spe,
                #position = pd,
                data=x.backtrans, 
                width = .1) +
  #geom_line(position = pd, color = "blue")+
  #scale_fill_manual(values=c("#006600", "#66CCFF")) +
  labs(x ="Temperature (deg C)", 
       y = "Byssus strength (N)") +
  #scale_y_continuous(breaks=seq(0,5, by=1))+
  #geom_hline(yintercept=0, size=1, color="black") +
  theme_classic(base_size = 12)+
  facet_wrap(~x$species, scales = "free") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        #axis.text.x=element_blank() #This is the last graph so needs the numbers
        axis.title.x=element_blank()
  )
# annotate("text", x = 3.9, y = 5, label = "Temp: p = 0.03")
# annotate("text", x = 3.9, y = 4.5, label = "Food: p = 0.02")+
# annotate("text", x = 3.9, y = 4, label = "Temp X Food: NS")
spe.plot
setwd(newfolder)
pdf(file = "Max_byssus_strength.pdf", width = 7, height = 3)
spe.plot
dev.off()

# ======================================== #
# Max byssus_strength zoomed in for tross ####
# ======================================== #
fn <- fun_Mesocosm_max_tross
summary(fn)
Anova(fn)
ef <- effect("Temp:Food", fn) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x <- as.data.frame(ef)
upper <- x$fit+x$se
lower <- x$fit-x$se
fn_t <- fn

m <- fun_Mesocosm_max_tross

coef(summary(m))
ef <- effect("Temp:Food", m) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x.1 <- as.data.frame(ef)
str(x.1)
species <- rep("M. trossulus", times = nrow(x.1))
x.1 <- cbind(species, x.1)

#pd <- position_dodge(2)

# fn <- gallo_model
# summary(fn)
# Anova(fn)
# ef <- effect("Temp:Food", fn) # This only makes sense to do as a 2-way ANOVA. 
# summary(ef)
# x <- as.data.frame(ef)
# upper <- x$fit+x$se
# lower <- x$fit-x$se
fn_t <- fn

fn_g <- fun_Mesocosm_max_gallo
m <- fn_g

coef(summary(m))
ef <- effect("Temp:Food", m) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x.2 <- as.data.frame(ef)
str(x.2)
species <- rep("M. galloprovincialis", times = nrow(x.2))
x.2 <- cbind(species, x.2)

x <- rbind(x.1,x.2)

upper <- 10^(x$fit+x$se)
lower <- 10^(x$fit-x$se)

head(x)
x.backtrans <- data.frame(
  x$species,
  x$Temp,
  x$Food,
  fit = 10^x$fit,
  se = 10^x$se,
  upper = 10^(x$fit+x$se),
  lower = 10^(x$fit-x$se)
)

shape.spe <- rep(x = 21, times = length(x$Food))
shape.spe[x$Food=="H"] <- 16
color.spe <- c(rep("blue",times = 8),rep("red",times = 8))
spe.plot <- ggplot(data = x.backtrans, aes(x$Temp, y =fit))+
  coord_cartesian(ylim = c(0, .3))+
  geom_point(stat = 'identity', 
             #position = pd, 
             color = color.spe, 
             shape = shape.spe,
             size = 3) +
  geom_errorbar(stat = "identity", 
                aes(ymax=upper, ymin=lower), 
                color = color.spe,
                #position = pd,
                data=x.backtrans, 
                width = .1) +
  #geom_line(position = pd, color = "blue")+
  #scale_fill_manual(values=c("#006600", "#66CCFF")) +
  labs(x ="Temperature (deg C)", 
       y = "Byssus strength (N)") +
  #scale_y_continuous(breaks=seq(0,5, by=1))+
  #geom_hline(yintercept=0, size=1, color="black") +
  theme_classic(base_size = 12)+
  facet_wrap(~x$species, scales = "free") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        #axis.text.x=element_blank() #This is the last graph so needs the numbers
        axis.title.x=element_blank()
  )
# annotate("text", x = 3.9, y = 5, label = "Temp: p = 0.03")
# annotate("text", x = 3.9, y = 4.5, label = "Food: p = 0.02")+
# annotate("text", x = 3.9, y = 4, label = "Temp X Food: NS")
spe.plot
setwd(newfolder)
pdf(file = "Max_byssus_strength_tross_zoom.pdf", width = 7, height = 3)
spe.plot
dev.off()



# ========================================================== #
# Nice plot of number of attached threads with both species ####
# ========================================================== #



fn <- fun_Mesocosm_thread_count_tross
summary(fn)
Anova(fn)
ef <- effect("Temp:Food", fn) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x <- as.data.frame(ef)
upper <- x$fit+x$se
lower <- x$fit-x$se
fn_t <- fn

m <- fn_t

coef(summary(m))
ef <- effect("Temp:Food", m) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x.1 <- as.data.frame(ef)
str(x.1)
species <- rep("M. trossulus", times = nrow(x.1))
x.1 <- cbind(species, x.1)

#pd <- position_dodge(2)

# fn <- gallo_model
# summary(fn)
# Anova(fn)
# ef <- effect("Temp:Food", fn) # This only makes sense to do as a 2-way ANOVA. 
# summary(ef)
# x <- as.data.frame(ef)
# upper <- x$fit+x$se
# lower <- x$fit-x$se
fn_t <- fn

fn_g <- fun_Mesocosm_thread_count_gallo
m <- fn_g

coef(summary(m))
ef <- effect("Temp:Food", m) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x.2 <- as.data.frame(ef)
str(x.2)
species <- rep("M. galloprovincialis", times = nrow(x.2))
x.2 <- cbind(species, x.2)

x <- rbind(x.1,x.2)


head(x)
x.backtrans <- data.frame(
  x$species,
  x$Temp,
  x$Food,
  fit = x$fit^2,
  se = x$se^2,
  upper = (x$fit+x$se)^2,
  lower = (x$fit-x$se)^2
)

shape.spe <- rep(x = 21, times = length(x$Food))
shape.spe[x$Food=="H"] <- 16
color.spe <- c(rep("blue",times = 8),rep("red",times = 8))
spe.plot <- ggplot(data = x.backtrans, aes(x$Temp, y =fit))+
  coord_cartesian(ylim = c(0, 40))+
  geom_point(stat = 'identity', 
             #position = pd, 
             color = color.spe, 
             shape = shape.spe,
             size = 3) +
  geom_errorbar(stat = "identity", 
                aes(ymax=upper, ymin=lower), 
                color = color.spe,
                #position = pd,
                data=x.backtrans, 
                width = .1) +
  #geom_line(position = pd, color = "blue")+
  #scale_fill_manual(values=c("#006600", "#66CCFF")) +
  labs(x ="Temperature (deg C)", 
       y = "Number of attached threads (N)") +
  #scale_y_continuous(breaks=seq(0,5, by=1))+
  #geom_hline(yintercept=0, size=1, color="black") +
  theme_classic(base_size = 12)+
  facet_wrap(~x$species, scales = "free") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        #axis.text.x=element_blank() #This is the last graph so needs the numbers
        axis.title.x=element_blank()
  )
# annotate("text", x = 3.9, y = 5, label = "Temp: p = 0.03")
# annotate("text", x = 3.9, y = 4.5, label = "Food: p = 0.02")+
# annotate("text", x = 3.9, y = 4, label = "Temp X Food: NS")
spe.plot
setwd(newfolder)
pdf(file = "Number_of_attached_threads.pdf", width = 7, height = 3)
spe.plot
dev.off()







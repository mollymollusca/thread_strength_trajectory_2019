#step_4: organization for stats and plotting ####
# and stats and plotting ####
# This versin (v2) is more organized. 

require(lme4)
require(MASS)
require(car)
require(ggplot2)
library(tidyr) # For command spread convert long form into wide form
require(stats)
require(effects)


rm(list=ls())

#only run 1X per analysis
sysdate <- Sys.Date()

# Set file locations and names
# Inputs
wd_thread_labels <- "~/thread_strength_trajectory_2019"
file_thread_labels <- "thread_labels.csv"

# Change the summary file on both lines (end of the first line)
wd_summary <- "~/thread_strength_trajectory_2019/analysis_20190417_good/step_3_2019-04-29/summary_2019-04-30"
file_summary <- "summary2019-04-30.csv"
# There are about 180 samples. I thought there were more like 300, but I guess not. 

wd_treatment_levels <- "~/thread_strength_trajectory_2019"
file_treatment_levels <- "treatment_levels.csv"

# Outputs
newfolder <- paste(wd_summary,'/step_4_',sysdate, sep="")
dir.create(newfolder)

annotated_summary <- paste('annotated_summary','_',sysdate,'.csv', sep="")


# Write metadata to file ####
comment(newfolder) <- "Info for this run."

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


# Import thread label information ####
setwd(wd_thread_labels)
specimen <- read.csv(file = file_thread_labels, stringsAsFactors = FALSE)
str(specimen)
specimen_list <- unlist(strsplit(specimen$new_label, "_"))#Now look at every other
odd <- rep(x = c(FALSE,TRUE), times = length(specimen_list))
even <- rep(x = c(TRUE,FALSE), times = length(specimen_list))
treatment <- subset(specimen_list,even)
IDnumber <- subset(specimen_list,odd)
specimen_df <- data.frame(
  treatment=treatment,
  IDnumber=IDnumber
)
str(specimen_df)
specimen_df <- specimen_df[!is.na(treatment),]
str(specimen_df)

mesocosm <- as.numeric(substr(specimen_df$treatment, start = 1, stop = 2))
level_plexiglass <- substr(specimen_df$treatment, start = 3, stop = 3)
species <- substr(specimen_df$treatment, start = 4, stop = 4)
(factor(level_plexiglass)) # Note that some levels are "U" for unknown

df.specimen <- data.frame(
  #specimen$num,
  old_label = as.character(specimen$old_label),
  new_label = as.character(specimen$new_label),
  repeated = specimen$sample_duplicate_run_exclude,
  mesocosm,
  level_plexiglass,
  species,
  specimen_ID = as.numeric(specimen_df$IDnumber)
)
str(df.specimen)
nrow(df.specimen)

# Taking a look at the datasheet ####
# Some of them are labeled species "C" not gallo or tross.
# Look at records. What is this tile? 
# Look at records. Are there any high food threads? 
table(df.specimen$species, df.specimen$mesocosm)
# With a cutoff of ~0.01
# 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
# G 5 3 3 5 5 3 4 4 9  5  6  2  4  6  3  5  8  3  2  6  7  5  3  4
# T 2 4 7 2 2 5 2 0 4  0  2  4  4  4  4  4  4  5  1  6  2  2  0  2

# I think this file was written twice 
# and just the same thing added twice, so to just have unique rows:
# nrow(df.specimen)
# df.specimen <- unique(df.specimen)
# nrow(df.specimen)

# Import delta sample summary ####
nrow(df.specimen[species =="T",])
setwd(wd_summary)
data <- read.csv(file = file_summary, stringsAsFactors = FALSE)
#data <- read.csv(file = "2 summary.csv", stringsAsFactors = FALSE)

# It appears data has one fewer row than df.specimen
str(df.specimen)
nrow(df.specimen)
str(data)
nrow(data)

# Data only has 180 rows. Why do I then gain 9 rows when merging with df.specimen?
# Maybe I'm actually missing 9 data points... or maybe there are about 9 unmatched rows.

# Issue with this step - I'm gaining extra lines
total_1 <- merge(x=df.specimen,y=data,by.x ="old_label",by.y ="specimen_label", all.x = FALSE, all.y = TRUE)
head(total_1)
nrow(total_1)

setwd(newfolder)
write.csv(file = "temp_summary.csv", x = total_1)

# Import treatment levels ####
setwd(wd_treatment_levels)
treatment_levels <- read.csv(file = file_treatment_levels, stringsAsFactors = FALSE)
str(treatment_levels)

# Merge ####
setwd(wd_summary)
total_2 <- merge(x = treatment_levels, y = total_1, by.x = c("Mesocosm"), by.y = c("mesocosm"), all.x = FALSE, all.y = TRUE)
head(total_2)
str(total_2)

total_2$Food <- as.factor(total_2$Food)
total_2$deg.C <- as.factor(total_2$deg.C)
total_2$Mesocosm <- as.factor(total_2$Mesocosm)

setwd(newfolder)
write.csv(file = annotated_summary, x = total_2)

png(file = "mesocosm_plot_all_spp.png")
plot(as.numeric(total_2$Mesocosm), cex = 1, pch = '.')
dev.off()
str(total_1)

# When there were no threads or too few threads for the calculation, 
# the median was flagged with a number of 9999. 
# Here these are converted to NA. 

nrow(total_2)
head(total_2)

# Final QC ####
# There is one run, 161104Specimen_RawData_5.csv, that doesn't have a specimen label to match up. I think this one may have been a test. 
total_2 <- total_2[!is.na(total_2$new_label),] 
# Although this delta looks like one single drop, this is an outlier. Unfortunately this run only had one thread, so the median is based on this one value. 
total_2$median[total_2$filename!="deltas_PV_170212_1Specimen_RawData_10.csv"] <- NA
total_2$median_last5[total_2$filename!="deltas_PV_170212_1Specimen_RawData_10.csv"] <- NA
total_2[total_2$median>=.2,]

total_2_subset <- total_2_subset[total_2$repeated==0,]

nrow(total_2_subset)

total_2 <- total_2_subset

# Here I changed this analysis to include the last 5 thread breaks only:
treatvec_trans <- data.frame(
  trans_thread_count = sqrt(total_2$total_thread_breaks),
  trans_max = log10(total_2$max_load+1),
  transvec = log10(total_2$median_last5+1),
  Temp = factor(total_2$deg.C),
  Food = factor(total_2$Food),
  Mesocosm = factor(total_2$Mesocosm),
  species = factor(total_2$species)
)

head(treatvec_trans)
treatvec_trans <- treatvec_trans[!is.na(treatvec_trans$transvec),]


# Stats!!! ####
#non-hierarchical check
fun_Mesocosm <- lm(transvec ~ species * Temp * Food, data=treatvec_trans)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

#nested model
fun_Mesocosm <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm), data=treatvec_trans, REML = FALSE)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)
# Nested and un-nested give the same result

setwd(newfolder)
write.csv(file = "ANOVA_both_species_thread_strength_nested.csv",Anova(fun_Mesocosm))

#Check max values ####
fun_Mesocosm_max <- lmer(trans_max ~ species * Temp * Food + (1 | Mesocosm), data=treatvec_trans, REML = FALSE)
summary(fun_Mesocosm_max)
Anova(fun_Mesocosm_max)

#Check number of threads per mussel ####
fun_Mesocosm_thread_count <- lmer(trans_thread_count ~ species * Temp * Food + (1 | Mesocosm), data=treatvec_trans, REML = FALSE)
summary(fun_Mesocosm_max)
Anova(fun_Mesocosm_max)

setwd(newfolder)
write.csv(file = "ANOVA_both_species_max_byssus_strength_nested.csv",Anova(fun_Mesocosm_max))


# Tross ####
tross <- treatvec_trans[treatvec_trans$species == "T", ]

setwd(newfolder)
pdf(file = "mesocosm_plot_tross.pdf")
par(mfrow = c(2,2))
plot(tross$Mesocosm, 10^tross$transvec - 1, ylab = "log thread strength")
plot(tross$Food, tross$transvec, ylab = "log thread strength")
plot(tross$Temp, tross$transvec, ylab = "log thread strength")
dev.off()

pdf(file = "mesocosm_plot_tross_max.pdf")
par(mfrow = c(2,2))
plot(tross$Mesocosm, tross$trans_max, ylab = "log max byssus strength")
plot(tross$Food, tross$trans_max, ylab = "log max byssus strength")
plot(tross$Temp, tross$trans_max, ylab = "log max byssus strength")
dev.off()

str(tross)
fun_Mesocosm_max_tross <- lmer(trans_max ~ Temp * Food + (1| Mesocosm), data=tross, REML = FALSE)
summary(fun_Mesocosm_max_tross)
Anova(fun_Mesocosm_max_tross)


fun_Mesocosm_thread_count_tross <- lmer(trans_thread_count ~ Temp * Food + (1 | Mesocosm), data=tross, REML = FALSE)
summary(fun_Mesocosm_thread_count_tross)
Anova(fun_Mesocosm_thread_count_tross)

# Non-nested version (gives the same answer)
fun_Mesocosm <- lm(transvec ~ Temp * Food, data=tross)
# plot(fun_Mesocosm)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)


str(tross)
fun_Mesocosm <- lmer(transvec ~ Temp * Food + (1| Mesocosm), data=tross, REML = FALSE)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)
tross_model <- fun_Mesocosm #Right now I'm using the not nested form because the nested version isn't plotting. 

# Gallo ####
gallo <- treatvec_trans[treatvec_trans$species == "G", ]

pdf(file = "Gallo_all_mesocosms.pdf")
plot(gallo$Mesocosm, gallo$transvec)
dev.off()
str(gallo)
head(gallo)

fun_Mesocosm_max_gallo <- lmer(trans_max ~ Temp * Food + (1| Mesocosm), data=gallo, REML = FALSE)
summary(fun_Mesocosm_max_gallo)
Anova(fun_Mesocosm_max_gallo)

# Nesting matters when evaluating thread count!!! 
# Not nested version - food significant effect
fun_Mesocosm_thread_count_gallo <- lm(trans_thread_count ~ Temp * Food, data=gallo)
summary(fun_Mesocosm_thread_count_gallo)
Anova(fun_Mesocosm_thread_count_gallo)

# Nested version - food not significant effect
fun_Mesocosm_thread_count_gallo <- lmer(trans_thread_count ~ Temp * Food + (1 | Mesocosm), data=gallo, REML = FALSE)
summary(fun_Mesocosm_thread_count_gallo)
Anova(fun_Mesocosm_thread_count_gallo)

# Not nested version - not significant, except interaction
fun_Mesocosm <- lm(transvec ~ Temp * Food, data=gallo)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

# Nested version - same answer
fun_Mesocosm <- lmer(transvec ~ Temp * Food + (1 | Mesocosm), data=gallo, REML = FALSE)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

gallo_model <- fun_Mesocosm #This is nested in Mescocosm 


# =========================== #
# Nice plot with both species ####
# =========================== #
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

color.spe <- c(rep("blue",times = 8),rep("red",times = 8))
spe.plot <- ggplot(data = x.backtrans, aes(x$Temp, y = fit))+
  coord_cartesian(ylim = c(0, .4))+
  geom_point(stat = 'identity', 
             #position = pd, 
             color = color.spe, 
             shape = c(16,16,16,16,21,21,21,21,16,16,16,16,21,21,21,21),
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
write.csv(file = "ANOVA_gallo.csv", x = Anova(fn_g))
write.csv(file = "ANOVA_tross.csv", x = Anova(fn_t))


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

upper <- 10^(x$fit+x$se)-1
lower <- 10^(x$fit-x$se)-1

head(x)
x.backtrans <- data.frame(
  x$species,
  x$Temp,
  x$Food,
  fit = 10^x$fit-1,
  se = 10^x$se-1,
  upper = upper <- 10^(x$fit+x$se)-1,
  lower <- 10^(x$fit-x$se)-1
)

color.spe <- c(rep("blue",times = 8),rep("red",times = 8))
spe.plot <- ggplot(data = x.backtrans, aes(x$Temp, y =fit))+
  coord_cartesian(ylim = c(0, 3))+
  geom_point(stat = 'identity', 
             #position = pd, 
             color = color.spe, 
             shape = c(16,16,16,16,21,21,21,21,16,16,16,16,21,21,21,21),
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

upper <- 10^(x$fit+x$se)-1
lower <- 10^(x$fit-x$se)-1

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

color.spe <- c(rep("blue",times = 8),rep("red",times = 8))
spe.plot <- ggplot(data = x.backtrans, aes(x$Temp, y =fit))+
  coord_cartesian(ylim = c(0, 20))+
  geom_point(stat = 'identity', 
             #position = pd, 
             color = color.spe, 
             shape = c(16,16,16,16,21,21,21,21,16,16,16,16,21,21,21,21),
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







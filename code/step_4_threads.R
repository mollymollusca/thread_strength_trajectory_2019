#step_4: organization for stats and plotting ####
# and stats and plotting ####

require(lme4)
require(MASS)
require(car)
require(ggplot2)
library(tidyr) # For command spread convert long form into wide form
require(stats)
require(effects)


rm(list=ls())

# Set file locations and names
wd_thread_labels <- "~/thread_strength_trajectory_2019"
file_thread_labels <- "thread_labels.csv"

wd_summary <- "~/thread_strength_trajectory_2019/analysis2019-04-10 17:01:18"
file_summary <- "summary2019-04-10 17:01:18.csv"
# There should be about 300 samples. Why are there only about 180 samples?

wd_treatment_levels <- "~/thread_strength_trajectory_2019"
file_treatment_levels <- "treatment_levels.csv"

# Import thread label information ####
setwd(wd_thread_labels)
specimen <- read.csv(file = file_thread_labels, stringsAsFactors = FALSE)
specimen$new_label
specimen_list <- unlist(strsplit(specimen$new_label, "_"))#Now look at every other
treatment <- rep(x = c(1,0), times = length(specimen_list))
IDnumber <- rep(x = c(0,1), times = length(specimen_list))
specimen_df <- data.frame(
  new_label=specimen_list,
  treatment,
  IDnumber
)
head(specimen_df)
(specimen_trt <- specimen_df[treatment==1,"new_label"])
(specimen_ID <- specimen_df[treatment==1,"new_label"])

mesocosm <- substr(specimen_trt, start = 1, stop = 2)
level_plexiglass <- substr(specimen_trt, start = 3, stop = 3)
species <- substr(specimen_trt, start = 4, stop = 4)

df.specimen <- data.frame(
  specimen$num,
  old_label = specimen$old_label,
  new_label = specimen$new_label,
  mesocosm,
  level_plexiglass,
  species,
  specimen_ID
)


# I think this file was written twice 
# and just the same thing added twice, so to just have unique rows:
# nrow(df.specimen)
# df.specimen <- unique(df.specimen)
# nrow(df.specimen)

# Import delta sample summary ####
df.specimen[species =="T",]
setwd(wd_summary)
data <- read.csv(file = file_summary, stringsAsFactors = FALSE)
#data <- read.csv(file = "2 summary.csv", stringsAsFactors = FALSE)

head(df.specimen)
head(data)

total_1 <- merge(x=df.specimen,data,by.x ="old_label",by.y ="specimen_label")
head(total_1)

plot(total_1$mesocosm)
str(total_1)

# Import treatment levels
setwd(wd_treatment_levels)
treatment_levels <- read.csv(file = file_treatment_levels, stringsAsFactors = FALSE)
str(treatment_levels)

# Merge ####
setwd(wd_summary)
head(treatment_levels)

total_2 <- merge(x = treatment_levels, y = total_1, by.x = c("Mesocosm"), by.y = c("mesocosm"))
head(total_2)
str(total_2)
# The temp should be a factor.
# The label should not be a factor...


plot(total_2$Mesocosm)


# When there were no threads or too few threads for the calculation, 
# the median was flagged with a number of 9999. 
# Here these are converted to NA. 
total_2[total_2$median>100,] <- NA 


treatvec_trans <- data.frame(
  transvec = log(total_2$median+1),
  Temp = factor(total_2$deg.C),
  Food = factor(total_2$Food),
  Mesocosm = factor(total_2$Mesocosm),
  species = factor(total_2$species)
)

head(treatvec_trans)
treatvec_trans <- treatvec_trans[!is.na(treatvec_trans$transvec),]


#stats!!! ####
#non-hierarchical check
fun_Mesocosm <- lm(transvec ~ species * Temp * Food, data=treatvec_trans)
summary(fun_Mesocosm)

#nested model
fun_Mesocosm <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm), data=treatvec_trans, REML = FALSE)
# checking which ranks are dropped:
summary(fun_Mesocosm)

# Tross####
tross <- treatvec_trans[treatvec_trans$species == "T", ]
tross_HF <- tross[tross$Food == "H",]
tross_LF <- tross[tross$Food == "L",]
tross_LTemp <- tross[as.numeric(tross$Temp)<21,]

plot(tross$Mesocosm, tross$transvec)


plot(tross$Food, tross$transvec)
plot(tross$Temp, tross$transvec)
plot(tross_HF$Temp, tross_HF$transvec)
plot(tross_LF$Temp, tross_LF$transvec)

# For the low temperature only
fun_Mesocosm_LTemp <- lmer(transvec ~ Temp * Food + (1|Mesocosm), data = tross_LTemp, REML = FALSE)
#fun_Mesocosm <- lm(transvec ~ Temp + Food, data = tross_LTemp)
summary(fun_Mesocosm_LTemp)
Anova(fun_Mesocosm_LTemp)

str(tross)
fun_Mesocosm <- lmer(transvec ~ Temp * Food + (1| Mesocosm), data=tross, REML = FALSE)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

fun_Mesocosm <- lm(transvec ~ Temp * Food, data=tross)
# plot(fun_Mesocosm)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

tross_model <- fun_Mesocosm #Right now I'm using the not nested form because the nested version isn't plotting. 

# Tross
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: transvec
# Chisq Df Pr(>Chisq)   
# Temp       0.0262  2   0.986978   
# Food       0.1852  1   0.666961   
# Temp:Food 11.8861  2   0.002624 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#

data(cake, package="lme4")
fm1 <- lmer(angle ~ recipe * temperature + (1|recipe:replicate), cake,
            REML = FALSE)
plot(Effect(c("recipe", "temperature"), fm1))
plot(effect("recipe:temperature", fm1),
     axes=list(grid=TRUE)) # equivalent (plus grid)



#plot Tross ####
# tross <- tross_LTemp
# fun_Mesocosm <- fun_Mesocosm_LTemp
library(effects)
require(ggplot2)
species <- tross
str(species)
# A special type III ANOVA is needed since there is an interaction
options(contrasts = c('contr.sum','contr.poly'))
species$Food <- factor(species$Food)
species$Temp <- factor(species$Temp, ordered = T)
species$Mesocosm <- factor(species$Mesocosm)
m <- fun_Mesocosm
#drop1(m, .~., test='F')
Anova(m)
coef(summary(m))
?effect
# effect isn't working because 1 combination is dropped. 
# Here is info on this:
# https://stackoverflow.com/questions/34221564/non-conformable-arguments-error-from-lmer-when-trying-to-extract-information-fro
ef <- effect("Temp:Food", m) # This only makes sense to do as a 2-way ANOVA. 

summary(ef)
x <- as.data.frame(ef)
#drop1(m, .~., test='F')

upper <- x$fit+x$se
lower <- x$fit-x$se

#untransform... 

!is.na(x)
par(mfrow = c(1,1))
plot(c(1,2), c(4,5))
plot(c(1,2,3,4,5,6),10^x$fit-1)
dev.off()

# This plot is working for lmer when I don't have the nesting going on...
plot1 <- ggplot(data = x, aes(x = as.factor(x$Temp), y = 10^x$fit-1, fill = Food))+
  geom_bar(stat = 'identity', position = position_dodge(), color = "black") + 
  geom_errorbar(stat = "identity", aes(ymax=10^upper-1, ymin=10^lower-1),
                position = position_dodge(0.9),
                data=x)+
  scale_fill_manual(values=c("#006600", "#66CCFF"))+
  labs(x="Temperature (deg C)", y= "Median thread strength", title= "M. trossulus") +
  ylim(0, .9)+
  geom_hline(yintercept=0, size=1, color="black")+
  theme_bw(base_size = 16) 
plot1

table(treatvec_trans$Temp, treatvec_trans$Food)
table(treatvec_trans$Temp, treatvec_trans$Food, treatvec_trans$Mesocosm)
# Now I'm wondering, maybe I should have within each level a replicate number 1-3, not mescocosm label. 
# https://ourcodingclub.github.io/2017/03/15/mixed-models.html

# gallo ####

gallo <- treatvec_trans[treatvec_trans$species == "G", ]

plot(gallo$Mesocosm, gallo$transvec)
str(gallo)
head(gallo)
fun_Mesocosm <- lmer(transvec ~ Temp * Food + (1 | Mesocosm), data=gallo, REML = FALSE)
#fun_Mesocosm <- lm(transvec ~ Temp * Food, data=gallo)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

gallo_model <- fun_Mesocosm #This is nested in Mescocosm 


# Gallo
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: transvec
# Chisq Df Pr(>Chisq)   
# Temp      11.6750  3   0.008584 **
#   Food       2.2328  1   0.135105   
# Temp:Food  8.6190  3   0.034810 * 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#plot Gallo ####
library(effects)
require(ggplot2)
species <- gallo
str(species)
# A special type III ANOVA is needed since there is an interaction
options(contrasts = c('contr.sum','contr.poly'))
species$Food <- factor(species$Food)
species$Temp <- factor(species$Temp, ordered = T)
species$Mesocosm <- factor(species$Mesocosm)
m <- fun_Mesocosm
#drop1(m, .~., test='F')
Anova(m)
coef(summary(m))
ef <- effect("Temp:Food", m) # This only makes sense to do as a 2-way ANOVA. 
summary(ef)
x <- as.data.frame(ef)
#drop1(m, .~., test='F')

upper <- x$fit+x$se
lower <- x$fit-x$se


plot1 <- ggplot(data = x, aes(x = as.factor(x$Temp), y = 10^x$fit-1, fill = Food))+
  geom_bar(stat = 'identity', position = position_dodge(), color = "black") + 
  geom_errorbar(stat = "identity", aes(ymax=10^upper-1, ymin=10^lower-1),
                position = position_dodge(0.9),
                data=x)+
  scale_fill_manual(values=c("#006600", "#66CCFF"))+
  labs(x="Temperature (deg C)", y= "Median thread strength", title= "M. galloprovincialis") +
  ylim(0, .9)+
  geom_hline(yintercept=0, size=1, color="black")+
  theme_bw(base_size = 16) 
plot1

# I'm getting really frustrated with the nesting 
# so I'm going to average by mesocosm and species. 
# I will only have n=3 so I doubt I'll see an effect, 
# but hopefully it gives a similar answer:

require(stats)
head(presidents)
tapply(presidents, cycle(presidents), mean, na.rm = TRUE)
tapply(X = tross$transvec, INDEX = tross$Mesocosm, mean, na.rm = TRUE)

# Nice plot with both species ####

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

upper <- x$fit+x$se
lower <- x$fit-x$se
color.spe <- c(rep("blue",times = 8),rep("red",times = 8))
spe.plot <- ggplot(data = x, aes(x$Temp, y = x$fit))+
  coord_cartesian(ylim = c(0, .2))+
  geom_point(stat = 'identity', 
             #position = pd, 
             color = color.spe, 
             shape = c(16,16,16,16,21,21,21,21,16,16,16,16,21,21,21,21),
             size = 3) +
  geom_errorbar(stat = "identity", 
                aes(ymax=upper, ymin=lower), 
                color = color.spe,
                #position = pd,
                data=x, 
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
setwd(wd_summary)
pdf(file = "Thread_strength.pdf", width = 7, height = 3)
spe.plot
dev.off()
pdf(file = "Thread_strength.png", width=500,height=200)
spe.plot
dev.off()


# Stats try 2 ####
#Now I'm discovering that tross is only from the last mesocosms - this is an error.
# BUT I'M GETTING A WEIRD ERROR... so oh well???? 
# I'm trying out the ANOVA stats posted here:
# http://www.flutterbys.com.au/stats/tut/tut9.2a.html
# In our case we have two factors, lets call them C and D that are fixed, and one random factor E.
# Actually, we should even include another random factor D, which is top vs. bottom tile. 
# In a balanced design, an anova would be enough, but this is not 100 balanced so:
 

tross
gallo
 library(nlme)

lme(gallo$transvec ~ gallo$Temp, random = gallo$Mesocosm)

Orthodont
fm1 <- lme(distance ~ age, data = Orthodont)
summary(fm1)

# A fixed/random, B random
# Balanced:
  # summary(aov(y~A+Error(B), data))
  # library(nlme)
  # VarCorr(lme(y~A,random=1|B, data))
# Unbalanced:	
  # library(nlme)
  # anova(lme(y~A,random=1|B, data), type='marginal')

# A fixed/random, B fixed
# Balanced
  # summary(aov(y~A+B, data))
# Unbalanced	
  # contrasts(data$B) <- contr.sum
  # library(car)
  # Anova(aov(y~A/B, data), type='III')
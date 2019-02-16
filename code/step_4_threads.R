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

setwd("~/threads")
specimen <- read.csv(file = "thread_labels.csv", stringsAsFactors = FALSE)
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

df.specimen[species =="T",]
setwd("~/threads")
data <- read.csv(file = "summary_20180404.csv", stringsAsFactors = FALSE)
data <- read.csv(file = "2 summary.csv", stringsAsFactors = FALSE)


total_1 <- merge(x=df.specimen,data,by.x ="old_label",by.y ="specimen_label")
head(total_1)

treatment_levels <- read.csv(file = "treatment_levels.csv", stringsAsFactors = FALSE)
head(treatment_levels)

total_2 <- merge(x = treatment_levels, y = total_1, by.x = c("Mesocosm"), by.y = c("mesocosm"))
head(total_2)
str(total_2)
# The temp should be a factor.
# The label should not be a factor...

total_2[total_2$median>100,] <- NA

treatvec_trans <- data.frame(
  transvec = log(total_2$median+1),
  Temp = factor(total_2$deg.C),
  Food = factor(total_2$Food),
  Mesocosm = factor(total_2$Mesocosm),
  species = factor(total_2$species)
)


#stats!!! ####

fun_Mesocosm <- lmer(transvec ~ species * Temp * Food + (1 | Mesocosm), data=treatvec_trans, REML = FALSE)
summary(fun_Mesocosm)

# Tross####
tross <- treatvec_trans[treatvec_trans$species == "T", ]
str(tross)
fun_Mesocosm <- lmer(transvec ~ Temp * Food + (1 | Mesocosm), data=tross, REML = FALSE)
#fun_Mesocosm <- lm(transvec ~ Temp * Food, data=tross)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

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

#plot Tross ####
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


# gallo ####

gallo <- treatvec_trans[treatvec_trans$species == "G", ]
str(gallo)
head(gallo)
fun_Mesocosm <- lmer(transvec ~ Temp * Food + (1 | Mesocosm), data=gallo, REML = FALSE)
#fun_Mesocosm <- lm(transvec ~ Temp * Food, data=gallo)
summary(fun_Mesocosm)
Anova(fun_Mesocosm)

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

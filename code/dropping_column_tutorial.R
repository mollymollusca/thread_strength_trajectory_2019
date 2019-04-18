# How to get effect when dropping one column??? Well here is a tutorial from: 
# https://stackoverflow.com/questions/34221564/non-conformable-arguments-error-from-lmer-when-trying-to-extract-information-fro
# I want to verify it makes sense, etc... 

library("lme4")
library("splines")
sleep <- sleepstudy  #get the sleep data
set.seed(1234567)
## next line happens to sample only 2 and 3 ...
sleep$age <- as.factor(sample(1:3,length(sleep),rep=TRUE))
length(levels(sleep$age))  ## 2
m4 <- lmer(Reaction ~ Days + ns(Days, df=4) +
             age + Days:age + (Days | Subject), sleep)

plot(Reaction~Days, data = sleep)
plot(Reaction~Subject, data = sleep)
plot(age~Subject, data = sleep)
plot(Reaction~Subject+Days, data = sleep)


## message; fixed-effect model matrix is 
##    rank deficient so dropping 1 column / coefficient
summary(m4)

f1 <- fixef(m4)
length(f1)  ## 7
f2 <- fixef(m4,add.dropped=TRUE)
length(f2)  ## 8

?getME
X <- getME(m4,"X")
ncol(X)  ## 7
(which.dropped <- attr(getME(m4,"X"),"col.dropped"))
## ns(Days, df = 4)4 
##             6

d <- 0:9  
## best to use data.frame() directly, avoid cbind()
##   generate age based on *actual* levels in data
newdat <- data.frame(Days=d,
                     age=factor(rep(levels(sleep$age),length(d))))

head(newdat)
mm <- model.matrix(formula(m4,fixed.only=TRUE)[-2], newdat)
mm <- mm[,-which.dropped]   ## drop redundant columns
## newdat$pred <- mm%*%fixef(m4)    ## works now

predFun <- function(x) predict(x,newdata=newdat,re.form=NA)
# This is where the prediction is
newdat$pred <- predFun(m4)
# Here are upper and lower
bb <- bootMer(m4,
              FUN=predFun,
              nsim=200)  
## nb. this produces an error message on its first run, 
## but not on subsequent runs (using the development version of lme4)
bb_ci <- as.data.frame(t(apply(bb$t,2,quantile,c(0.025,0.975))))
names(bb_ci) <- c("lwr","upr")
newdat <- cbind(newdat,bb_ci)

plot(Reaction~Days,sleep)
with(newdat,
     matlines(Days,cbind(pred,lwr,upr),
              col=c("red","green","green"),
              lty=2,
              lwd=c(3,2,2)))
?matlines


# HW 7
# Walter Reilly
# 11.28.16


library(dplyr)
library(tidyr)

d <- read.csv("~/walter/204/confidence.csv")

# 1 format long

# add subject variable
mutate(d, Subject = 1:100)

# format long
dlong<-reshape(d,                   #1. input wide data
              varying=c("t1","t2","t3"),  #2. specify time varying variables that will be collapsed into one variable
              v.names = "confidence",               #3. name of the new long variable
              timevar="obs",                   #4. name of the new time identifying variable
              times=c("t1","t2","t3"),    #5. values to populate the timevar column
              direction="long")                #6. direction, can also specify "wide" if going other direction

head(dlong)
tail(dlong)

# make time non-categorical variable
dlong<-dlong[order(dlong$id),]
time <- rep(c(0.5,1.5,2.5), 100)
dlong = mutate(dlong,time)

# 2 test whether public and private universities differ in confidence
summary(aov(confidence ~ public + Error(id), data = dlong))
mod1 = aov(confidence ~ public + Error(id), data = dlong)

# group means
summarise(group_by(dlong, public), mean = mean(confidence))


# 3 test whether confidence differs over time 
summary(aov(confidence ~ obs + Error(id), data = dlong))

# needed this for contra.poly to work
#options(contrasts=c("contr.sum","contr.poly"))

#Polynomial Contrasts to describe trajectories
dlong[,6] <- contr.poly(3)[,1] 
dlong[,7] <- contr.poly(3)[,2] 

# add contrasts to df
colnames(dlong) <- c(colnames(dlong[,1:5]),c("l","q"))

# evaluate anova
summary(aov(confidence ~ l*obs + q*obs + 
              Error(id), data = dlong))


summary(aov(confidence ~ l*obs + q*obs + Error(id), data = dlong))
















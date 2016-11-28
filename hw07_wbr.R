# HW 7
# Walter Reilly
# 11.28.16


library(tidyverse)

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







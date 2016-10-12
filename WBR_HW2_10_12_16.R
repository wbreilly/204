# Walter Reilly
# 10_12_16
# HW 2 for stats 204A

# set wd
setwd("walter/204")

# load ggplot
library("ggplot2")

#load ggplot
library("tidyverse")

# load data
HWD <- read.csv("~/walter/204/HW01Data.csv")


# 5a. Create a histogram of weight
qplot(HWD$weight_lbs, geom="histogram", binwidth = 1, main = "Hist of Weight" , xlab = "Weight(lbs)" )

# 5b
# plot the density
dplot = qplot(HWD$height_in, geom="density",  main = "Density of Height" , xlab = "Height(Inches)" )

# plot verticle line at mean height
dplot + geom_vline(xintercept = mean(HWD$height_in))







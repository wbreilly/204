# Walter Reilly
# 10_14_16
# PSC 204 HW3
library("tidyverse")
setwd("walter/204")
HW3D = read.csv("HW03data.csv")

# 4a Gather mean, median, and SD summary statistics about these samples. 
HW3Dsum = data.frame(HW3D %>%
                        summarise(davis.means = mean(davis),
                                  davis.medians = median(davis),
                                  davis.sd = sd(davis, na.rm = FALSE),
                                  la.means = mean(la),
                                  la.medians = median(la),
                                  la.sd = sd(la, na.rm = FALSE),
                                  merced.means = mean(merced),
                                  merced.medians = median(merced),
                                  merced.sd = sd(merced, na.rm = FALSE)
                      ))

# 4b
library(ggplot2)
t.test(HW3D$davis,HW3D$la, mu = 0)
t.test(HW3D$davis,HW3D$merced, mu = 0)                          
t.test(HW3D$la,HW3D$merced, mu = 0)                                     

# 5 plot density of anxiety at each campus
dplot = qplot(HW3D$davis, geom="density",  main = "Davis" , 
              xlab = "Anxiety")
laplot =qplot(HW3D$la, geom="density",  main = "LA" , 
                                      xlab = "Anxiety")
mplot =qplot(HW3D$merced, geom="density",  main = "Merced" , 
               xlab = "Anxiety")
# plot verticle line at mean height
dplot = dplot + geom_vline(xintercept = mean(HW3D$davis), linetype = "longdash") + xlim(-3,7.5) + ylim(0,.5)
laplot = laplot + geom_vline(xintercept = mean(HW3D$la), linetype ="longdash") + xlim(-3,7.5) + ylim(0,.5)
mplot = mplot + geom_vline(xintercept = mean(HW3D$merced),linetype ="longdash") + xlim(-3,7.5) + ylim(0,.5)

# display all 3 together
library(gridExtra)
 grid.arrange(dplot,laplot,mplot, ncol=1)


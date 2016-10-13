# Walter Reilly
# 10_12_16
# HW 2 for stats 204A

# set wd
setwd("walter/204_stats")

# load ggplot
library("ggplot2")

#load ggplot
library("tidyverse")

# load data
# HWD <- read.csv("~/walter/204/HW01Data.csv")
HWD <- read.csv("~/walter/204_stats/HW01Data.csv")

# 5a. Create a histogram of weight
qplot(HWD$weight_lbs, geom="histogram", binwidth = 1, main = "Hist of Weight" ,
        xlab = "Weight(lbs)" )

# 5b
# plot the density
dplot = qplot(HWD$height_in, geom="density",  main = "Density of Height" , 
              xlab = "Height(Inches)")

# plot verticle line at mean height
dplot + geom_vline(xintercept = mean(HWD$height_in))

# 6 
#bmi function
calc.bmi = function(weight_lbs, height_ins)
{
  bmi = weight_lbs / height_ins^2 * 703
  return(bmi)
}
# add BMI column
HWD = HWD %>%
  mutate(BMI = weight_lbs / height_in^2 * 703)

# group biosex and ed
my.table = data.frame(HWD %>%
  group_by(biosex, ed_cmplt) %>%
  summarise(bmi.means = mean(BMI),
            bmi.medians = median(BMI),
            bmi.variance = var(BMI, na.rm = FALSE),
            bmi.max = max(BMI),
            bmi.min = min(BMI)
            ))

# 7a
#ungroup 
ungroup(HWD)

max.bmi = max(HWD$BMI)
HWD[HWD$BMI == max.bmi, ]

min.bmi = min(HWD$BMI)
HWD[HWD$BMI == min.bmi, ]

#7b
#scale
HWD = mutate(HWD, z.BMI = scale(HWD$BMI))

max.Zbmi = max(HWD$z.BMI)
HWD[HWD$z.BMI == max.Zbmi, ]

min.Zbmi = min(HWD$z.BMI)
HWD[HWD$z.BMI == min.Zbmi, ]

#7c
# make z.BMI numeric
HWD[,7] = drop(HWD$z.BMI)
# new rescaled column
HWD = mutate(HWD, rescale.BMI = (HWD$z.BMI*15)+100)






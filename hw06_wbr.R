# Walter Reilly
# 11.17.16
# HW 6

setwd("walter/204_stats")
 data = read.csv("hw06data.csv")
d = data.frame(data)

library(tidyverse)

# count n by factor combinations
d %>%
  group_by(biosex, workout) %>%
  count()

# summary stats - grand mean
sums1 = summarise(d, mean=mean(pctgain), sd=sd(pctgain))

# summary stats - main effect biosex
sums2 = summarise(group_by(d, biosex),
          mean=mean(pctgain), sd=sd(pctgain))

# summary stats - interactions
sums3 = summarise(group_by(d, biosex, workout),
         mean=mean(pctgain), sd=sd(pctgain))

# factorial anova
anova(lm(d$pctgain ~ d$biosex + d$workout + d$biosex*d$workout))



# PSC - 204b Final
# Walter Reilly
# Last update: 12/3/16
#
#

 ########################################
# #2
# using algrebra (attached hand calculations), filled in covariance table. Now will use matrix multiplication
# to fill in remainder of correlation table and check work

 # first create matrix of 1/SDs with SDs on diagonal 
 tocorr.mtx = matrix( rep( 0, len=25), nrow = 5)
 tocorr.mtx[1,1] = 1/sqrt(4)
 tocorr.mtx[2,2] = 1/sqrt(.25)
 tocorr.mtx[3,3] = 1/sqrt(9)
 tocorr.mtx[4,4] = 1/sqrt(7.29)
 tocorr.mtx[5,5] = 1/sqrt(29.16)
 
 # imported csv of the full covariance matrix, which was copied from the Word doc.
 # I used Excel to copy the matrix across the diagonal
corrmx = tocorr.mtx %*% covmtx %*% tocorr.mtx
corrmx
 
# double check covariance matrix by reproducing from corr matrix
sd.mtx = matrix( rep( 0, len=25), nrow = 5)
sd.mtx[1,1] = sqrt(4)
sd.mtx[2,2] = sqrt(.25)
sd.mtx[3,3] = sqrt(9)
sd.mtx[4,4] = sqrt(7.29)
sd.mtx[5,5] = sqrt(29.16)

covarmx = sd.mtx %*% corrmx %*% sd.mtx
covarmx

################################################################
#1
library("ggplot2")
library(tidyverse)
d <- read.csv("~/walter/204_stats/tempmood.csv")

# get group mean and sd for mood and temp
mean(d$mood)
sd(d$mood)
mean(d$temp)
sd(d$temp)

# add obs var
d = mutate(d,obs = 1:70)

# make subject a grouping variable
d = gather(d, subject,mood,c(2:5))

#1a plot
p1 =  ggplot(data=d, aes(x=temp, y=mood, group=subject, colour = subject)) +
  stat_smooth() +
  geom_point() +
  ggtitle("The Mood of 4 Individuals, Modulated by Temperature (Loess)")
p1

#1b  compute means and sds
summarise(group_by(d, subject), mean = mean(mood), sd = sd(mood))

# plot 2
p2 =  ggplot(data=d, aes(x=temp, y=mood, group=1)) +
  stat_smooth(method = glm) +
  geom_point() +
  ggtitle("Mood, Modulated by Temperature (GLM)")
p2


##################################################
#1c
# read in data again to original format
d2 <- read.csv("~/walter/204_stats/tempmood.csv")

# add mean mood across ps
d2 = mutate(d2,moodmn = (part1 + part2 + part3 + part4)/4)

# mean temp
mntemp = mean(d2$temp)
# mnmnmood
mnmoodmn = mean(d2$moodmn)

# subtract mean from everyvalue
d2 = mutate(d2,temp2 = temp-mntemp)
d2 = mutate(d2,mood2 = moodmn-mnmoodmn)

# create matrix from the above 
mat = as.matrix(cbind(d2$temp2,d2$mood2))

# off diagonal is the sum of cross products
crossp = t(mat) %*% mat
crossp
# covariance is simply the mean of the cross products
cov1 = crossp/69 # divide by n-1
cov1
# calculate covariance the easy way for comparison
 cov(d2$temp,d2$moodmn)
 
 ##################################################
 
 #1c
 
 # correlation between weather and mood, easy way
 cor(d2$temp,d2$moodmn)
 # other way
 cov1[2,1]/(sd(d2$temp2)*sd(d2$mood2))
 
 ##########################
 
 #1d estimate regression equation of the line for all individuals as single group
 lm(d2$moodmn ~ d2$temp)
 
 #1e estimate regression line for each individual 
 lm(d2$part1 ~ d2$temp)
 lm(d2$part2 ~ d2$temp)
 lm(d2$part3 ~ d2$temp)
 lm(d2$part4 ~ d2$temp)
 
 #1f Fisher's z transform
 
 cor.p1 <- cor(d[which(d$subject == "part1"), "mood"],
               d[which(d$subject == "part1"), "temp"])
 cor.p2 <- cor(d[which(d$subject == "part2"), "mood"],
               d[which(d$subject == "part2"), "temp"])
 cor.p3 <- cor(d[which(d$subject == "part3"), "mood"],
               d[which(d$subject == "part3"), "temp"])
 cor.p4 <- cor(d[which(d$subject == "part4"), "mood"],
               d[which(d$subject == "part4"), "temp"])
 

corz.p1 <- .5 * log((1 + cor.p1) / (1 - cor.p1))
corz.p1
corz.p2 <- .5 * log((1 + cor.p2) / (1 - cor.p2))
corz.p2
corz.p3 <- .5 * log((1 + cor.p3) / (1 - cor.p3))
corz.p3
corz.p4 <- .5 * log((1 + cor.p4) / (1 - cor.p4))
corz.p4
 
 zp1p2 <- (corz.p1- corz.p2) / sqrt((1/67) + (1/67))
 zp1p2
 zp1p3 <- (corz.p1- corz.p3) / sqrt((1/67) + (1/67))
 zp1p3
 zp1p4 <- (corz.p1- corz.p4) / sqrt((1/67) + (1/67))
 zp1p4
 zp2p3 <- (corz.p2- corz.p3) / sqrt((1/67) + (1/67))
 zp2p3
 zp2p4 <- (corz.p2- corz.p4) / sqrt((1/67) + (1/67))
 zp2p4
 zp3p4 <- (corz.p3- corz.p4) / sqrt((1/67) + (1/67))
 zp3p4
 
 ##############################################################
 #3 
 library(tidyverse)
 d <- read.csv("~/walter/204_stats/socialacceptance.csv")
 View(d)
 
 # main effect sum stats
 sportstats = d %>% group_by(sports) %>% summarise(mean = mean(psa), SD = sd(psa))
 biostats = d %>% group_by(female) %>% summarise(mean = mean(psa), SD = sd(psa))
  
# take a look at summary statistics, interaction
 sumstats = d %>% group_by(sports, female) %>% summarise(mean = mean(psa), SD = sd(psa))
 
 # get n
counts = count(group_by(d,sports, female))
 
# add n to sumstats
sumstats[,5] = counts[,3]

 # add SE
 sumstats = mutate(sumstats , SE = SD/sqrt(n))
 
# create a bar graph
 limits <- aes(ymax = sumstats$mean + sumstats$SE,
               ymin = sumstats$mean - sumstats$SE)
 
 p3 <- ggplot(data = sumstats, aes(x = factor(female), y = mean,
                                 fill = factor(sports)))
 p3 = p3 + geom_bar(stat = "identity",
              position = position_dodge(0.9)) +
   geom_errorbar(limits, position = position_dodge(0.9),
                 width = 0.15) +
   labs(x = "Biological Sex", y = "PSA") +
   ggtitle("Influence of Sports Participantion on PSA") +
   scale_fill_discrete(name = "Sports")
p3

# factorial ANOVA with interaction
library(car)
Anova(lm(d$psa ~ d$sports * d$female, type=c(2)))

# significant interaction observed, so do simple effects to investigate further
# grab the right data for females and males
psaf <- d$psa[which(d$female == 1)]
sportsf <- d$sports[which(d$female == 1)]

psam <- d$psa[which(d$female == 0)]
sportsm <- d$sports[which(d$female == 0)]

# estimate the female model
anova(lm(psaf ~ sportsf))

# estimate the male model 
anova(lm(psam ~ sportsm))

#Pairwise comparisons
pairwise.t.test(psaf, sportsf, p.adjust = c("bonf"))
by(psaf, sportsf, mean)

pairwise.t.test(psam, sportsm, p.adjust = c("bonf"))
by(psam, sportsm, mean)

######################

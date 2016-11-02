#Kristine Christianson:
#     Lab: Fridays 10-11:50am
#Nate Smith
#     Lab: Fridays 12:10-2:00pm

#204a R Lab 5
#Fall 2016

#######################################################################
#######################################################################

#ANOVA

#######################################################################
#######################################################################

#We'll make some data (same that we used last week)
cneg <- rep(c(1,0,0), each = 20)
cpos <- rep(c(0,1,0), each = 20)

set.seed(1023)
score <- 5 + 1.5*cpos + -1.3*cneg + rnorm(60, 0, 2)
condition <- rep(c("Negative","Positive","Control"), each = 20)
d <- data.frame(score,condition,cneg,cpos)
by(d$score,d$condition,mean)



######################################################

#The actual model in R


lm(score ~ condition)
	#lm - linear model
	#This is the function to specify linear models
	#in R, we'll use this for most of the rest of class
	#In particular, we'll use it for regression models

aov(score ~ condition)
	#Is essentially a variation of lm in R
	#It is specific to reporting ANOVA and results
	#in different output.
	#I don't really care for it and prefer lm()
	#For the purposes of ANOVA, this is more informative
	#than lm however.
	
#Summary reports

anova(lm(score ~ condition))
	#for lm() we can use anova() to get
	#an ANOVA table from the lm object

summary(aov(score ~ condition))
	#for aov() we can use summary() to
	#get the ANOVA table from the aov object

summary(lm(score ~ condition))
	#Note that summary of the lm() object
	#is quite different from summary of
	#an aov object

summary(aov(score ~ condition))
	#So, what's our conclusion here?


#----------------------------------------
#Orthogonal contrasts
#----------------------------------------

#You'll hear a lot about these

#Orthogonal contrasts can be made a priori and used
#without ever conducting an ANOVA.
#They partition variance so that each test is orthogonal to
#the other. This means that you do not have to adjust your
#alpha level for the comparisons. However, you are limited
#To the number and nature of comparisons you can make.

#Specifically, you can make j-1 orthogonal comparisons.
	#j = N of groups/conditions
	#In my example I have 3 conditions, so I can have 2 contrasts.

#I can do any of the following:

#Control Vs. Neg; Control and Neg. Vs. Pos
#Control Vs. Neg; Control and Pos. Vs. Neg


by(d$score, d$condition, mean)
	#It looks like the first option is the most reasonable
	#Contrast 1: Control vs. Neg
	#Contrast 2: Control and Neg vs. Pos

d$c1 <- 0; d$c2 <- 0

d[which(d$condition == "Control"),"c1"] <- 1
d[which(d$condition == "Negative"),"c1"] <- -1

d[which(d$condition == "Control"),"c2"] <- -.5
d[which(d$condition == "Negative"),"c2"] <- -.5
d[which(d$condition == "Positive"),"c2"] <- 1

d$c1
d$c2

	#Now we can put the contrasts into a model

summary(aov(score ~ c1 + c2, data = d))
anova(lm(score ~ c1 + c2, data = d))
	#We would conclude that there is no
	#difference between negative and control.
	#Positive is greater than both
	#negative and control.

#What do you mean by partitioned variance?

anova(lm(score ~ condition, data = d))
anova(lm(score ~ c1 + c2, data = d))

	#Or, to be real precise
anova(lm(score ~ condition, data = d))[1,2]
sum(anova(lm(score ~ c1 + c2, data = d))[1:2,2])

#############################################################

# Graphing some of this

#############################################################


tempm <- by(d$score, d$condition, mean)

install.packages('gplots')
library(gplots)

#To plot means with confidence intervals
plotmeans(d$score ~ d$condition, xlab = 'Condition',
          ylab = 'Score', main = "Condition Effects on Score",
          barcol = 'black', connect = FALSE, n.label = FALSE,
          pch = c(16, 17, 18))
abline(h = mean(tempm[1]), lty = 5, col = 'red')
abline(h = mean(tempm[2]), lty = 5, col = 'red')
abline(h = mean(tempm[3]), lty = 5, col = 'red')


#################################################################
#Practice


#Conditioning avoidance by pairing neutral stimulus with stressful stimulus

rat <- rep(c(1,0,0,0), each = 30)
moth <- rep(c(0,1,0,0), each = 30)
germ <- rep(c(0,0,1,0), each = 30)

stimulus <- rep(c("rat","moth","germ","control"), each = 30)

time <- 60 - 45*rat + -15*moth + -30*germ + rnorm(120,0,2)

d2 <- data.frame(time,stimulus,rat,moth,germ)
head(d2)

#Time is duration of exposure to neutral stimulus paired by condition
#with a stressful stimulus (e.g., rat). Time is in s, and participants
#are free to discontinue contact with neutral stimulus whenever they want
#but are encouraged to maintain contact for at least 60 s.
#Shorter duration contacts suggest greater avoidance and aversion to
#neutral stimulus.

#1. Means and SD of time by condition.
#2. ANOVA of relation between condition and time
#3. Planned orthogonal contrast of time and conditions.
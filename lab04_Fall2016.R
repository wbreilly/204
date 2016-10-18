#Kristine Christianson:
#     Lab: Fridays 10-11:50am
#Nate Smith
#     Lab: Fridays 12:10-2:00pm

#204a R Lab 4
#Fall 2016

#######################################

#Two independent samples t-test

t.test(temp1, temp2)
t.test(temp1, temp2, alt = c("greater"))
t.test(temp1, temp2, alt = c("less"))

#Are the variances different, or can we change
#the var.equal default from FALSE to TRUE?

?var.test
#var.test(x, y, ratio = 1,
#         alternative = c("two.sided", "less", "greater"),
#         conf.level = 0.95, ...)

#x is the vector of one sample
#y is the vector of the other sample
#ratio = 1, indicates that the ratio of variances should
#be 1, or, that the variances are non-zero and 
#equal in value
#alternative and conf.level are the same as in t.test()

var.test(temp1, temp2)
#We would conclude that the variances do not
#differ significantly

t.test(temp1, temp2, var.equal = TRUE)
t.test(temp1, temp2)

#We see that the Welch test makes an adjustment to
#the df, which results in a difference in the pvalue

t.test(temp1, temp2, var.equal = TRUE)$parameter
t.test(temp1, temp2)$parameter

###########################

#Paired t-tests

#Make some paired data
mean(temp1);sd(temp1);length(temp1)
set.seed(300)
temp3 <- 3 + .2*temp1 + rnorm(30, 0, 2)
cor(temp1,temp3)
mean(temp3);sd(temp3)

set.seed(300)
temp4 <- -.2 + .9*temp1 + rnorm(30, 0, 1)
cor(temp1,temp4)
mean(temp4);sd(temp4)
#This is just a basic linear equation
#showing the relation between temp1
#and both temp3 & temp4, which allows
#us to generate those values

#---------------------------------------------------
#Paired vs. Independent t-tests

t.test(temp1, temp3, paired = FALSE)
#Here we are not using a paired t.test

t.test(temp1, temp3, paired = TRUE)
#Here we use the paired t.test

t.test(temp1, temp4, paired = FALSE)
#Here we are not using a paired t.test

t.test(temp1, temp4, paired = TRUE)

#Compare the paired and independent t-tests
#with each other

t.test(temp1, temp3, paired = TRUE)$statistic -
  t.test(temp1, temp3, paired = FALSE)$statistic
cor(temp1,temp3)

t.test(temp1, temp4, paired = TRUE)$statistic -
  t.test(temp1, temp4, paired = FALSE)$statistic
cor(temp1,temp4)

#The difference between the paired and independent
#samples t-test depends on the strength of association
#between the two scores.
#The weaker the association, the less difference between
#paired and independent, the stronger the association
#the more powerful the paired vs. the independent.



#######################################################################
#######################################################################

#ANOVA

#######################################################################
#######################################################################

#We'll make some data
      #rep() will repeat values for a specified number of times
      #here, we are repeating values of 1, 0, 0 for cneg 20 times each
cneg <- rep(c(1, 0, 0), each = 20)
cpos <- rep(c(0, 1, 0), each = 20)

#Creating a linear combination of cpos and cneg
      #the rnorm() function is used to add in error
      #Here, we specify the residuals to have mean =
      #0 and SD = 2
set.seed(1023)
score <- 5 + 1.5*cpos + -1.3*cneg + rnorm(60, 0, 2)
score
mean(score)

condition <- rep(c("Negative", "Positive", "Control"), each = 20)
condition


d <- data.frame(score, condition, cneg, cpos)
dim(d)
head(d)
tail(d)

#Looking at mean scores for each condition
by(d$score, d$condition, mean)



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
	#For the purposes of ANOVA, this is more informative
	#than lm however.
	
#Summary reports

anova(lm(score ~ condition))
	#for lm() we can use anova() to get
	#an ANOVA table from the lm object

summary(aov(score ~ condition))
	#for aov() we can use summary() to
	#get the ANOVA table from the aov object

#You can index values from summaries
anova(lm(score ~ condition))[1,2]
	#This is the SS for the between

anova(lm(score ~ condition))[2,2]
	#This is the SS for the within

#Eta Squared - SSB/SST
anova(lm(score ~ condition))[1,2]/ #indexing the SSB
	sum(anova(lm(score ~ condition))[,2]) #summing the SSB and SSW

#R reports this as r.square
#We can get this value from the output directly
#by using $r.square
summary(lm(score ~ condition))$r.square

summary(lm(score ~ condition))
	#Note that summary of the lm() object
	#is quite different from summary of
	#an aov object

summary(aov(score ~ condition))
	#So, what's our conclusion here?



#################################################################
#Practice


#Conditioning avoidance by pairing neutral stimulus with stressful stimulus

rat <- rep(c(1, 0, 0, 0), each = 30)
moth <- rep(c(0, 1, 0, 0), each = 30)
germ <- rep(c(0, 0, 1, 0), each = 30)

stimulus <- rep(c("rat","moth","germ","control"), each = 30)

time <- 60 - 45*rat + -15*moth + -30*germ + rnorm(120, 0, 2)

d2 <- data.frame(time, stimulus, rat, moth, germ)
head(d2)

#Time is duration of exposure to neutral stimulus paired by condition
#with a stressful stimulus (e.g., rat). Time is in s, and participants
#are free to discontinue contact with neutral stimulus whenever they want
#but are encouraged to maintain contact for at least 60 s.
#Shorter duration contacts suggest greater avoidance and aversion to
#neutral stimulus.

#1. Means and SD of time by condition.
#2. ANOVA of relation between condition and time
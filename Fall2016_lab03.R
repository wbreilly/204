#Kristine Christianson:
#     Lab: Fridays 10-11:50am
#Nate Smith
#     Lab: Fridays 12:10-2:00pm

#204a R Lab 3
#Fall 2016

#########################################
#Problem 4 example

set.seed(2)
happiness <- rnorm(100, 0, 1)
sex <- sample(c("male", "female"), 100, replace=TRUE)
marital <- sample(c("single", "married", "divorced"), 100, replace=TRUE)
d <- data.frame(happiness, sex, marital)

#Let's take a look at the data we created
head(d)

#calc mean, median, etc., for happiness by subsets of sex and marital status

by(d$happiness, list(d$sex, d$marital), mean)
by(d$happiness, list(d$sex, d$marital), median)

#Some plotting functions to try
plot(density(d$happiness), xlab = "Level of Happiness",
     main = "Density Distribution of Happiness")
abline(v = mean(d$happiness))
abline(h = .25)
lines(c(-2, 0),c(.2, .15), col = 'red')
lines(c(0,2),c(.15,.2), col = 'red')
lines(c(-2,2),c(.2,.2), col = 'red')
points(c(4, -1), c(.2, .3), pch = 'O')
points(c(4, 1), c(.2, .3), pch = 'O')


##################### Other helpful tips

temp <- sample(1:100, 15)
temp

#Remember to respect the order of operations

##########
#Example 1
##########

#Subtracting the mean of temp from each value of temp, then summing these values
sum(temp-mean(temp))

#Squaring the result above
sum(temp-mean(temp))^2

#Squaring the difference between the temp and the mean of temp
sum((temp-mean(temp))^2)

##########
#Example 2
##########

length(temp)

#Subtracts 1 from each value of temp
length(temp-1)

#Subtracts 1 from the length of temp
(length(temp)-1)


####Transforming a variable with mean(x) and sd(x)
####to a variable with mean(y) and sd(y), from raw scores
####In this case we want mean(y)=100, and sd(y)=15

x<-c(1,2,3,4,5)
mean(x)
sd(x)
#Rescale for standard deviation 
#by multiplying every value of x by sd(y)/sd(x)
#For this example we want the new sd(y) to be 15

y<-x*(15/sd(x))
mean(y)
sd(y)

#Next rescale for the mean, which we want to  be 100 by
#adding mean(y)-mean(x) to every new value in y

y<-y+(100-mean(y))
mean(y)
sd(y)

###Option #2 is to standardized all scores and then scale for sd and mean

#standardizing first
x2<-scale(x)
mean(x2)
sd(x2)

#rescaling by sd(y) which we want to be 15
y2<-x2*15
mean(y2)
sd(y2)

#rescaling by mean(y) which we want to be 100
y2<-y2+100
mean(y2)
sd(y2)

##########################################################
##########################################################
##########################################################
##########################################################

##########################################################
#The z distribution
##########################################################

#Probability of a normal distribution value
pnorm(1.96, 0, 1)
pnorm(-1.96, 0, 1)

#z value associated with a proportion in the normal distribution
qnorm(.025, 0, 1, lower.tail = TRUE)
qnorm(.025, 0, 1, lower.tail = FALSE)

##########################################################
#The t distribution
##########################################################


#When sample size is large, t is normally distributed 
plot(density(rnorm(100000, 0, 1)))
lines(density(rt(100000, 14)), col = 'red')
abline(v=c(-1.96,1.96))

#Probability of a t distribution value
pt(1.9, 14, lower.tail = FALSE)
pt(-1.9, 14, lower.tail = TRUE)

#We can look at the effect of increasing df
#on this probability
pt(1.9, 49, lower.tail = FALSE)
pt(-1.9, 49, lower.tail = TRUE)

pt(1.9, 99, lower.tail = FALSE)
pt(-1.9, 99, lower.tail = TRUE)

#t value associated with a proportion in the t the distribution
# p = .05
qt(.025, 4, lower.tail = TRUE)
qt(.025, 4, lower.tail = FALSE)

#Now looking at the effect of increasing df
#on the t value
qt(.025, 14, lower.tail = TRUE)
qt(.025, 14, lower.tail = FALSE)

qt(.025, 29, lower.tail = TRUE)
qt(.025, 29, lower.tail = FALSE)

#######################################

#z vs. t via histogram and SD

#######################################

par(mfrow = c(2, 2))

hist(rnorm(1000, 0, 1), xlim = c(-7, 7))
hist(rt(1000, df = 4), xlim = c(-7, 7))
hist(rt(1000, df = 14), xlim = c(-7, 7))
hist(rt(1000, df = 29), xlim = c(-7, 7))

sd(rnorm(1000, 0, 1))
sd(rt(1000, df = 4))
sd(rt(1000, df = 14))
sd(rt(1000, df = 29))


#######################################

#Z tests of mean differences

#Imagine you have 3 samples from sub-populations, 
#each with the following characteristics:

#pop1: xbar = 82, N = 15
#pop2: xbar = 96, N = 10
#pop3: xbar = 110, N = 11

#IQ scores are ~N(100, 15) in the population

#Do any of these samples differ significantly from the population?

#Computing a z score for pop1
pop1z <- (82-100)/(15/sqrt(15))
pop1z

#Is the probability of obtaining this z score
#less than our specified alpha level in either 
#of the tails?
.025 > pnorm(pop1z, lower.tail = TRUE)
	#Two-tailed, alpha = .05
.005 > pnorm(pop1z, lower.tail = TRUE)
	#Two-tailed, alpha = .01

pop2z <- (96-100)/(15/sqrt(10))
pop2z
.025 > pnorm(pop2z, lower.tail = TRUE)
	#Two-tailed, alpha = .05
.005 > pnorm(pop2z, lower.tail = TRUE)
	#Two-tailed, alpha = .01

pop3z <- (110-100)/(15/sqrt(11))
pop3z
.025 > pnorm(pop3z, lower.tail = FALSE)
	#Two-tailed, alpha = .05
.005 > pnorm(pop3z, lower.tail = FALSE)
	#Two-tailed, alpha = .01

###################################################

#We can do the same thing with Confidence Intervals

###################################################

#Recall that CI equals
#xbar +/- Std.Error*Critical.z.score

#qnorm can give critical z-score

critz <- qnorm(.025, 0, 1, lower.tail = FALSE)

#Std.Error is sigma/sqrt(N)

sepop1 <- 15/sqrt(15)
sepop2 <- 15/sqrt(10)
sepop3 <- 15/sqrt(11)

#95% CI to test pop1

c((82 - sepop1*critz),(82 + sepop1*critz))
	#Is 100 within the 95% CI?

#95% CI to test pop2

c((96 - sepop2*critz),(96 + sepop2*critz))
     #Is 100 within the 95% CI?

#95% CI to test pop3

c((110 - sepop3*critz),(110 + sepop3*critz))
     #Is 100 within the 95% CI?

#################################################################
#################################################################
#################################################################


#t Tests


#Create 2 samples of 30 individuals
#with means of 5 and 5, sds = 1
temp1 <- round(rnorm(30, 5, 1), 2)
temp2 <- round(rnorm(30, 6, 1), 2)



#############################################################
#############################################################

#The t-test arguments

?t.test
#t.test(x, y = NULL,
#       alternative = c("two.sided", "less", "greater"),
#       mu = 0, paired = FALSE, var.equal = FALSE,
#       conf.level = 0.95, ...)

#x is a vector, you need to specify at least one to test
	#if you only have one vector it is a one-sample
	#t-test

#y is Null by default, so t-test is a one-sample t-test
	#by default. For a two-sample or paired t-test,
	#you specify another vector

#alternative is the specification of the alternative hypothesis.
	#By default alternative is two.tailed, meaning that t-test
	#assumes alpha should be split between both tails.
	#We can specify one-tailed hypotheses in either tail
	#using "less" and "greater"

#mu = 0 specifies the difference to be tested. So, in a one-sample
	#t-test, this is the population mu (xbar-mu). For the
	#two-sample t-test, this is the expected difference
	#between the two-samples. Generally, we would assume the
	#difference is zero, because we are testing whether the
	#means differ. However, one could imagine a situation where
	#one would want to test whether a difference between
	#two groups was consistent across different samples.

t.test(temp1,temp2, mu = 0)
t.test(temp1,temp2, mu = 1)
t.test(temp1,temp2, mu = -1)

#paired = FALSE is the default. This is whether the two vectors
	#are independent, or if they have some dependency.
	#So, a random, independent sample of students drawn from 
	#two schools and assessed on self-esteem would have no 
	#dependency. A random sample of students from two schools
	#who are assessed on self-esteem at the beginning of the
	#academic year and end of the academic year would have
	#a dependency if you were comparing pre- and post-academic
	#year self-esteem scores. But not if you were comparing
	#school one vs. school two student self-esteem scores

#var.equal = FALSE is the default. This means that t.test
	#assumes that variances are not equal between two
	#samples, regardless of whether this is paired or
	#independent. This is not a bad assumption since
	#and consequently the Welch Two Sample t-test is implemented.
	#We can test if the variances are equal before
	#conducting the t-test. If they do not differ significantly
	#then we can change the default to TRUE and conduct a
	#general t-test.
		#we can use the var.test() for this and will discuss
		#it in a moment.

#conf.level = .95 is the default. By default, t.test assumes an
	#alpha of .05. We can change this by specifying another
	#proportion value (e.g., .90 for alpha of .10).


#The one-tailed test

t.test(temp1, mu = 0)
t.test(temp1, mu = 5)

t.test(temp2, mu = 5)

	#Obviously, the further mu is from the mean
	#of the population the data was generated from
	#the more significant the difference is
	#We can visualize this pretty easily
	#considering that 0 <= p <= 1

pval <- unlist(
	lapply(seq(0, 10, .01), function(x) {

		t.test(temp1, mu = x)$p.value

	}))

plot(seq(0, 10, .01),
	pval,
	ylim = c(0,1),
	xlim = c(3,7),
	xlab = "Mu Values",
	ylab = "P-Values"
	)
	abline(v = mean(temp1), lty = 3)

#We would see the same thing using the two-tailed test
	#when mu -> xbar1 - xbar2

#You may have noted in the lapply statement that I had
	#t.test(temp1, mu = x)$p.value
	#Many of the t.test components can
	#be called as objects using a call name

#statistic - the value of the t-statistic.
 
#parameter - the degrees of freedom for the t-statistic.
 
#p.value - the p-value for the test.
 
#conf.int - a confidence interval for the mean 
	#appropriate to the specified alternative hypothesis.
 
#estimate - the estimated mean or difference in 
	#means depending on whether it was a one-sample 
	#test or a two-sample test.
 
#null.value - the specified hypothesized value of 
	#the mean or mean difference depending on 
	#whether it was a one-sample test or a two-sample test.
 
#alternative - a character string describing the alternative hypothesis.
 
#method - a character string indicating what type of t-test was performed.
 
#data.name - a character string giving the name(s) of the data.

#so, for example

t.test(temp1)$p.value
t.test(temp1)$statistic
t.test(temp1)$conf.int
t.test(temp1)$parameter

	#This is useful if you are ever running many
	#tests iteratively, but only want certain 
	#values, rather than the entire test,
	#or if you're building tables in R


############################################
#Practice

v1 <- rnorm(100, 0, 1)
v2 <- rnorm(100, 2, 2)
v3 <- rnorm(100, 4, .5)
v4 <- rnorm(100, -2, 6)
v5 <- rnorm(100, 3, .25)

d <- data.frame(v1, v2, v3, v4, v5)

#Practice conducting one-sample, independent samples, and paired
#t-tests using v1-v5.
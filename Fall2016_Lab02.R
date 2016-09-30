#Kristine Christianson:
#     Lab: Fridays 10-11:50am
#Nate Smith
#     Lab: Fridays 12:10-2:00pm

#204a R Lab 2 
#Fall 2016
#####################################################################

#These are the same populations we were using during the demo
set.seed(100215)
pop1 <- rnorm(400000, 0, 1)
pop2 <- runif(400000, -5, 5)
pop3 <- rf(400000, 1, 30)

#Functions to describe the data

mean(pop1)
median(pop1)
max(pop1)
min(pop1)
sd(pop1)
var(pop1)

#all functions use the na.rm = TRUE argument
#the default argument is na.rm = FALSE

#Plots from today to demonstrate the density of the data

hist(pop1)
hist(pop2)
hist(pop3)


#Denstiy plots
plot(density(pop1))
plot(density(pop2))
plot(density(pop3))


#Some possible modifications
hist(pop1, main = "Histogram of Standard Normal Pop",
	xlab = "Values", xlim = c(-2, 2), breaks = 200, col = 'slategray')
hist(rnorm(25, 0, 1), main = "Histogram of Standard Normal Pop",
	xlab = "Values", xlim = c(-2, 2), breaks = 200, col = 'tomato')
hist(rnorm(25, 0, 1), main = "Histogram of Standard Normal Pop",
	xlab = "Values", xlim = c(-2, 2), breaks = 10, col = 'cyan')


##################################

#Standardizing data

set.seed(10090)
temp <- rnorm(500, 5, 2.3)
hist(temp)

#z-score
temp2 <- (temp-mean(temp))/sd(temp)
hist(temp2)

temp3 <- scale(temp)[,1]
hist(temp3)


tdat <- data.frame(temp,temp2,temp3)
cor(tdat)

	#cor() is the correlation function, we'll talk about
	#it much more later, for now it can be used to
	#show us that the variability between temp versions
	#is identical. So, even though the values have changed
	#the relative location of all the observations are
	#identical in each version of temp.



##########Creating New Variables/Adding to existing data sets################

###Create a data.frame from existing vectors(one-dimensional matrices)###
#In this example we create three normally distributed variables
#(gender, age, and favorite NBA team)using the sample() function
#and then attach them to each other using cbind() function. Note that
#cbind() will combine vectors into a matrix by column rather than a data frame unless
#otherwise specified. 


gender <- sample(c("male","female"),10,replace=TRUE)
age <- sample(c(21:30),10,replace=TRUE)
nba <- sample(c("Kings"),10,replace=TRUE)
my.data <- data.frame(gender,age,nba)
my.data

#We can also use cbind() to append a vector to an existing matrix or data.frame
#Note that we do not need to respecify data.frame because the first argument in the cbind()
#fuction is already specified as a data.frame.  Remember that one of the major differences
#between a data.frame and a matrix in R is that a matrix may contain only one type of 
#data, i.e. numeric or character, whereas a data.frame can contain multiple variable types

depression <- sample(c("depressed","very depressed","despondent"),10,replace=TRUE)
my.data <- cbind(my.data, depression)
my.data

#Variables can also be added directly to an existing data set by referencing the data
#by name and using the '$' to name the new variable

my.data$newvar <- c(1:10)

#Lets add new variables to this data based on the "dating rule"
#https://www.psychologytoday.com/blog/meet-catch-and-keep/201405/who-is-too-young-or-too-old-you-date
#Lower limit "half your age plus seven"
#Upper limit "take your age, subtract seven, then double it"
#

my.data$datemin <- (my.data$age / 2) + 7
my.data$datemax <- (my.data$age - 7) * 2
my.data$daterange <- my.data$datemax - my.data$datemin
my.data


#############################################
#############################################


#Now you try!

#Create the data set
set.seed(10913)
v1 <- rnorm(500, 5, 3.3)
v2 <- rnorm(500, 3.3, .5)
v3 <- sample(c(0,1),500, replace = TRUE)
v4 <- sample(c(1,2,2,3,3,3,4,4,4,4,4,5,5,5,6,6,7),500,replace = TRUE)
d <- data.frame(v1,v2,v3,v4)

#Some exercises to conduct

#1. Get the mean, median, min, max for v1 & v2
v1.mn = mean(v1)
v1.med = median(v1)
v1.min = min(v1)
v1.max = max(v1)

v2.mn = mean(v2)
v2.med = median(v2)
v2.min = min(v2)
v2.max = max(v2)

#2. Plot v1-v4, describe the distribution of the data
hist(v1) #normal
hist(v2) #normal
hist(v3) # all values are either 0 or 1, two bars for 0 and 1
hist(v4) # symmetrical

#3. Calculate the biased and unbiased SD for v1-v2 & v4
v1.bsd = b.sd(v1)
v1.usd = u.sd(v1)

v4.bsd = b.sd(v4)
v4.usd = u.sd(v4)

#4. Calculate the frequency of 0 and 1 for v3
v3.zero = v3 ==0
#239 zeros
#261 ones

#5. Transform v1 and v2 into normal standard distributions
zv1 = (v1-mean(v1))/sd(v1)
hist(zv1)

zv2 = (v2-mean(v2))/sd(v2)
hist(zv2)
#	by "hand" and using scale()
scalev1 = scale(v1)[,1]
hist(scalev1)
scalev2 = scale(v2)[,1]
hist(scalev2)





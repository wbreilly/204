#Kristine Christianson:
#     Lab: Fridays 10-11:50am
#Nate Smith
#     Lab: Fridays 12:10-2:00pm

#204a R Lab 7
#Fall 2016

#######################################################################
#######################################################################

#RM ANOVA

###################################################

#DATA GEN:

#In this hypothetical data set, we are generating values for two groups of
#diabetics. The first group receives a drug that is meant to reduce
#glucose levels, and the other group only receives a placebo, which is not
#expected to affect glucose levels. Subjects were measured at a total of
#4 occasions. 

glucp1 <- rnorm(50,160,10) #glucose for placebo group at time 1
glucd1 <- rnorm(50,160,10) #glucose for drug group at time 1

glucp2 <- glucp1 + rnorm(50,0,5)
glucp3 <- glucp2 + rnorm(50,0,5)
glucp4 <- glucp3 + rnorm(50,0,5)

glucd2 <- glucd1 - 30 + rnorm(50,0,5)
glucd3 <- glucd2 - 15 + rnorm(50,0,5)
glucd4 <- glucd3 + rnorm(50,0,5)

drug <- rep(c(0,1), each = 50) #create dummy variable for placebo versus drug group
t1 <- c(glucp1,glucd1) #group by time
t2 <- c(glucp2,glucd2)
t3 <- c(glucp3,glucd3)
t4 <- c(glucp4,glucd4)
gwide <- data.frame(drug, t1, t2, t3, t4)
head(gwide)
tail(gwide)


###############################
####Going from Wide to Long####
###############################

##Create id variable/add to wide data##

id <- (1:100) 
gwide <- cbind(id, gwide)
head(gwide)

##We will use the reshape() function to change the form of the data
##to prepare the data for RM ANOVA

#Arguments					
melt<-reshape(gwide,                   #1. input wide data
              varying=c("t1","t2","t3","t4"),  #2. specify time varying variables that will be collapsed into one variable
              v.names="glucose",               #3. name of the new long variable
              timevar="obs",                   #4. name of the new time identifying variable
              times=c("t1","t2","t3","t4"),    #5. values to populate the timevar column
              direction="long")                #6. direction, can also specify "wide" if going other direction

head(melt)

##Notice that the data is sorted by the "obs" variable,
##all 100 entries of t1 will be followed by 100 respective
##entries of t2,t3, and t4

##If we sort by "id", we can get the data in the form we want for analyses

glong<-melt[order(melt$id),]
glong[1:10,]

##Add a new variable, non-categorical corresponding to time

time <- rep(c(0.5,1.5,2.5,3.5), 100)
glong<-cbind(glong,time)
head(glong)
tail(glong)

####################################################

#Plot Data
#Many ways to do this!

# Create observed 95% confidence interval
gpcl <- sapply(gwide[which(gwide$drug == 0), 3:6], quantile, .025)
gpcu <- sapply(gwide[which(gwide$drug == 0), 3:6], quantile, .975)
gdcl <- sapply(gwide[which(gwide$drug == 1), 3:6], quantile, .025)
gdcu <- sapply(gwide[which(gwide$drug == 1), 3:6], quantile, .975)


with(glong, interaction.plot(obs, drug, glucose, col = c('tomato', 'turquoise'),
                             lwd = c(3, 3), legend = FALSE, ylab = 'Glucose',
                             main = 'Plot of Glucose Levels by Group across Time',
                             xlab = 'Time', ylim = c(80, 200)))
lines(1:4, gpcl, col = 'tomato', lty = 3, lwd = 2)
lines(1:4, gpcu, col = 'tomato', lty = 3, lwd = 2)
lines(1:4, gdcl, col = 'turquoise', lty = 3, lwd = 2)
lines(1:4, gdcu, col = 'turquoise', lty = 3, lwd = 2)
legend('bottom', c('Placebo', 'Drug'), col = c('tomato', 'turquoise'), 
       lwd = c(3, 3), bty = 'n', lty = c(2, 1), horiz = TRUE)



##########################################


#RM ANOVA Model methods
#Also, may ways to do this as well

#Also, for those of you who would like a more detailed
#explanation of compound symmetry/sphericity, refer here:
#http://homepages.gold.ac.uk/aphome/spheric.html


#Let's run a one-way ANOVA of the effect of drug on glucose:
summary(aov(glucose ~ drug, data = glong))
#What's wrong here?


#Remeber the nature of the data
head(glong);head(gwide)
dim(glong);dim(gwide)


#Effect of Drug on Glucose Levels
summary(aov(glucose ~ drug + Error(id), data = glong))

#Effect of Time on Glucose Levels
summary(aov(glucose ~ obs + Error(id), data = glong))

#Effects of Drug and Time on Glucose Levels
summary(aov(glucose ~ drug + obs + Error(id), data = glong))

#Adding a Drug by Time Interaction
summary(aov(glucose ~ drug*obs + Error(id), data = glong))


#Polynomial Contrasts to describe trajectories
contr.poly(4)

glong[,6] <- contr.poly(4)[,1] 
glong[,7] <- contr.poly(4)[,2] 
glong[,8] <- contr.poly(4)[,3] 


colnames(glong) <- c(colnames(glong[,1:5]),c("l","q","c"))
head(glong)
tail(glong)


summary(aov(glucose ~ l + q + c + Error(id), data = glong))


summary(aov(glucose ~ l*drug + q*drug + c*drug + 
                 Error(id), data = glong))


########

#Follow-up options

#Simple Effects split on interaction: e.g., drug vs. placebo
summary(aov(glucose ~ obs + Error(id), glong[which(glong$drug == 0),]))
summary(aov(glucose ~ obs + Error(id), glong[which(glong$drug == 1),]))

pairwise.t.test(glong[which(glong$drug == 1),"glucose"],
                glong[which(glong$drug == 1),"obs"],
                paired = TRUE, p.adjust = c("bonf"))

#we shouldn't do this for the placebo group given
#that the test was not significant, but we can
#for the sake of practice here

#Using Bonferroni Correction for familywise error
pairwise.t.test(glong[which(glong$drug == 0),"glucose"],
                glong[which(glong$drug == 0),"obs"],
                paired = TRUE, p.adjust = c("bonf"))

#Without applying a correction for familywise error
pairwise.t.test(glong[which(glong$drug == 0),"glucose"],
                glong[which(glong$drug == 0),"obs"],
                paired = TRUE, p.adjust = c("none"))


#or, using the polynomial contrasts

summary(aov(glucose ~ l + q + c + 
                 Error(id), glong[which(glong$drug == 0),]))

summary(aov(glucose ~ l + q + c + 
                 Error(id), glong[which(glong$drug == 1),]))


######################################

#CORRELATION

######################################

#Load R Packages

install.packages('MASS', 'car', 'lavaan')

library(MASS); library(car); library(lavann)

######################################


#Correlation: The degree of association between two measurements

#cor(x, y)

#cov(x, y)

#cor.test(x, y,
#         alternative = c("two.sided", "less", "greater"),
#         method = c("pearson", "kendall", "spearman"),
#         conf.level = 0.95, ...)

#plot(x, y)

#---------------------------


#A correlation matrix
corm <- matrix(c(
     1, .5,
     .5,  1 ), nrow = 2, ncol = 2, byrow = TRUE)
corm
#symmetric with 1 in the diagonal


#A covariance matrix

#cor2cov(cor.mat, sd) : from lavaan

covm <- cor2cov(corm, c(2, 3)) #specify the correlation matrix and SDs
covm
#symmetic with variances in the diagonal and
#covariances in off-diagonal

#Matrix algebra for conversion to covariance from correlation
sdm <- matrix(c(
     2, 0,
     0, 3), nrow = 2, ncol = 2, byrow = TRUE)
sdm
#a matrix with SDs on the diagonal and zeros on off-diagonal

sdm %*% corm %*% sdm
covm

#Matrix algebra for conversion to correlation from covariance
sdm2 <- matrix(c(
     1/2,   0,
     0  , 1/3), nrow = 2, ncol = 2, byrow = TRUE)
sdm2
#using inverse of the SDs

sdm2 %*% covm %*% sdm2
corm

##############################

#mvrnorm(n = 1, mu, Sigma, empirical = FALSE) : From MASS

set.seed(25)
dat <- mvrnorm(50, c(0, 0), covm, empirical = TRUE) #empirical = T because we are dealing with sample

head(dat);tail(dat)
cor(dat)
corm

cov(dat)
covm

plot(dat[,1], dat[,2])
abline(lm(dat[,2] ~ dat[,1]))
#abline : a, b line. In other words, slope and intercept
#lm means linear model, so what is the linear model?

summary(lm(dat[,2] ~ dat[,1]))
coef(summary(lm(dat[,2] ~ dat[,1])))[2, 1]
#not very informative

#Let's get standardized coefficients
summary(lm(scale(dat[,2])[,1] ~ scale(dat[,1])[,1]))
#scale() returns a matrix of normed data
#the default is to make standard normal: N(0,1)

#let's grab what I want
coef(summary(lm(scale(dat[,2])[,1] ~ scale(dat[,1])[,1])))[2,1]
#coef() : extracts coefficients from model

cor(dat[,1], dat[,2])
#so, the standardized regression weight is the same as our
#correlation value
#this is a special condition of the bivariate regression

##################################
############################################

#Is correlation smart?

corm2 <- matrix(c(
     1,  .85,
     .85,  1 ), nrow = 2, ncol = 2, byrow = TRUE)
corm2

set.seed(25)
dat2 <- mvrnorm(10000, c(0, 0), corm2, empirical = TRUE)
cor(dat2)
par(mfrow = c(1, 2))
plot(dat2[,1], dat2[,2])
abline(lm(dat2[,2] ~ dat2[,1]), lwd = 5, col = "red")

#Let's change the relation to quadratic
dat2[which(dat2[,1] > 0),2] <- dat2[which(dat2[,1] > 0),2]*-1
plot(dat2[,1], dat2[,2])

cor(dat2)
abline(lm(dat2[,2] ~ dat2[,1]), lwd = 5, col = "red")
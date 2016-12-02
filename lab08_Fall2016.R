#Kristine Christianson:
#     Lab: Fridays 10-11:50am
#Nate Smith
#     Lab: Fridays 12:10-2:00pm

#204a R Lab 8
#Fall 2016

#############################################

#Load R Packages

library(MASS); library(car); library(lavaan)

#NOTE: If you do not have any of these packages already installed
#use install.packages('packagename')

#############################################


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

covm <- cor2cov(corm, c(2, 3))
covm
#symmetic with variances on the diagonal and
#covariances on off-diagonal

#Matrix algebra for conversion to covariance from correlation
sdm <- matrix(c(
     2, 0,
     0, 3), nrow = 2, ncol = 2, byrow = TRUE)
sdm
#a matrix with SD in the diagonal and zero in off-diagonal

sdm %*% corm %*% sdm
covm

#Matrix algebra for conversion to covariance from correlation
sdm2 <- matrix(c(
     1/2,   0,
     0  , 1/3), nrow = 2, ncol = 2, byrow = TRUE)
sdm2

sdm2 %*% covm %*% sdm2
corm

##############################

#mvrnorm(n = 1, mu, Sigma, empirical = FALSE) : From MASS

set.seed(25)
dat <- mvrnorm(50, c(0,0), covm, empirical = TRUE)
#Generating 2 variables with 50 observations from a multivariate 
#normal distribution with means both equal to 0, and covariance 
#matrix equal to covm

head(dat);tail(dat)
cor(dat)
corm

cov(dat)
covm

plot(dat[,1],dat[,2])
abline(lm(dat[,2] ~ dat[,1]))
#abline : a, b line. In other words, slope and intercept
#lm means linear model, so what is the linear model?

summary(lm(dat[,2] ~ dat[,1]))
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
plot(dat2[,1],dat2[,2])
abline(lm(dat2[,2] ~ dat2[,1]), lwd = 5, col = "red")

#Let's change the relation to quadratic
dat2[which(dat2[,1] > 0),2] <- dat2[which(dat2[,1] > 0),2]*-1
plot(dat2[,1],dat2[,2])

cor(dat2)
abline(lm(dat2[,2] ~ dat2[,1]), lwd = 5, col = "red")

#########################################################
#########################################################
#########################################################


#Multiple Regression

#Data Generation

set.seed(1204)
d4m <- as.data.frame(mvrnorm(100, c(4, 3.1), 
	matrix(c(1, .05, .05, 1), nrow = 2, ncol = 2, byrow = TRUE), 
	empirical = TRUE))
colnames(d4m) <- c("lonely", "depress")
#Draw values from a multivariate normal distribution for males

d4f <- as.data.frame(mvrnorm(100, c(3.5, 2.2), 
	matrix(c(1, .23, .23, 1),nrow=2,ncol=2,byrow=TRUE), 
	empirical = TRUE))
colnames(d4f) <- c("lonely", "depress")
#Draw values from a multivariate normal distribution for females

d <- rbind(d4m, d4f)

d$lonely <- d$lonely - min(d$lonely)
d$lonely <- round(d$lonely, 2)
d$depress <- d$depress - min(d$depress)
d$depress <- round(d$depress, 2)

d$female <- rep(c(0,1), each=100)

d$substance <- unlist(lapply(1:200, function(x){
	use <- .35 + .4 * d[x, "depress"] + .2 * d[x,"depress"] * d[x,"lonely"]
	use2 <- round((100 * (exp(use) / (1 + exp(use)))), 0)
	sample(c(rep(0, (100 - use2)), rep(1, use2)), 1)
	}))


head(d)
summary(d)

##############################################


#Factorial ANOVA
m1 <- anova(lm(lonely ~ female + substance, data = d))
m1
m1r <- sum(m1[1:2, 2]) / sum(m1[1:3, 2]); m1r

#ANCOVA
m2 <- anova(lm(depress ~ substance + lonely, data = d))
m2
m2r <- sum(m2[1:2, 2])/sum(m2[1:3, 2]); m2r

#How do these differ from a multiple regression?
summary(lm(lonely ~ female + substance, data = d))
m1r

summary(lm(depress ~ substance + lonely, data = d))
m2r
	#They don't, this is all the same type of model


########################

#REGRESSION

########################

#Same form as ANOVA models

# y = B0 + B1*x1 + B2*x2 + ... Bk*xk + e

#dichotomous variables
m3 <- summary(lm(depress ~ female, data = d))
m3

	#this is the same as a t-test

m3t <- t.test(d[which(d$female == 0), "depress"],
	 d[which(d$female == 1), "depress"], var.equal=TRUE)
m3t

m30 <- mean(d[which(d$female == 0), "depress"])
m31 <- mean(d[which(d$female == 1), "depress"])
m30; m31
m31 - m30

coef(m3)


	#The relation between F and t values

anova(lm(depress ~ female, data = d))
sqrt(anova(lm(depress ~ female, data = d))[1,4])
m3t$statistic
m3t$statistic^2

#All this is to say that
# t(df)^2 = F(1, df) & sqrt(F(1, df)) = t(df)


#Interpreting simple regression - Dichotomous Predictor
m3
#Intercept refers to the value of the DV when predictor is equal to 0
     #So the intercept here is the mean value when female = 0 (i.e., for males)
#Slope is the mean difference when predictor is equal to 1
     #So the slope here is the difference between males and females


#Interpreting simple regression - Continuous Predictor
m4 <- summary(lm(depress ~ lonely, data = d))
m4
#Intercept refers to the value of the DV when predictor is equal to 0
     #Here, this is the value of depress when lonely is 0
#Slope is expected change in the DV when the predictor increases by 1 unit 
     #Here, a 1 unit increase in lonely results in a .238 unit increase in depress


#Interpretting Multiple Regression
m5 <- summary(lm(depress ~ lonely + female, data = d))
m5
#Intercept is the value of the DV when all predictors = 0
     #Here, the value of depress when lonely is 0 and female = 0 (males)
#Slope is the change in DV for a one unit change in predictor when other predictor(s) = 0
     #Here, slope of lonely refers to the change in depress for a one unit increase in lonely
     #Slope of female is the difference from males in mean depress score


#An interaction
m6 <- summary(lm(depress ~ lonely*female, data = d))
m6

#Interpretation of Regression Coefficients: Categorical*Continuous Variable Interaction
#                
#(Intercept)   : (Expected value of depress when lonely and female = 0) 
#lonely        : (Slope of the regression line when female=0, or for males)  
#female        : (Difference in the intercepts between males and females when lonely=0) 
#lonely:female : (The difference in slope between the male and female group))


m6z <- summary(lm(depress ~ lonely*female, 
	data = as.data.frame(scale(d)[,])))
m6z

	#Why do the results differ when using z-scored data?

d$inter <- d$lonely * d$female

dz <- as.data.frame(scale(d)[,])
dz$inter <- dz$lonely * dz$female

cor(d)
cor(dz)

	#Correlation between interaction terms and main terms
	#is not as strong when main terms are z-scored


#Comparing correlations
#Does the association of depression and loneliness
#differ between men and women?

m6

	#What do we think?

#Let's reconsider what this model really says

#depress = Intercept + B1*lonely + B2*female + B3*lonely*female

#Intercept = Fixed value
#Female = 0,1 => B2 and B3 are either in or out of equation

#Therefore:

#Male equation
#depress = Intercept + B1*lonely

#Female equation
#depress = (Intercept + B1) + (B2 + B3)*lonely 


#######################################################
#######################################################
#######################################################

#Logistic Regression

#Generate data

exp(0) / (1 + exp(0))
exp(1) / (1 + exp(1))
exp(-1) / (1 + exp(-1))

set.seed(12042)
bmidif<- rnorm(300, 0, 2)

ptfail <- unlist(lapply(1:300, function(r){
	temp <- .5 + .7 * bmidif[r] + rnorm(1, 0, .1)
	temp2 <- round((100 * (exp(temp) / (1 + exp(temp)))), 0)
	sample(c(rep(0, (100 - temp2)), rep(1, temp2)), 1)
	}))

##Bedrock police officers are required to pass a yearly physical test, if they fail they have two weeks
##to train and retake it before they are docked pay
##bmidif represents change in bmi score after 2 week grace period
##ptfail is whether they passed the physical test the second time around, 0=pass, 1=fail

plot(bmidif,ptfail)

m8 <- summary(glm(ptfail ~ bmidif, family = binomial(link = logit)))
m8

#Interpretation of Regression Coefficients: Logistic Regression Log-Odds
# Coefficients for logistic regression are expressed in terms of log odds               
#(Intercept)   : (Expected log odds value when bmidiff=0) 
# x            : (one unit change in bmidiff corresponds to a 0.7443 change in log odds for ptfail)  

#Exponentiate the coefficients, this helps with interpretation!

coef<-coef(m8)
intercept<-exp(coef[1,1])
slope<-exp(coef[2,1])

#Interpretation of Regression Coefficients: Logistic Regression Odds
#(Intercept)   : 1.46(Expected odds to fail when bmidiff=0) 
# bmidff       : 2.105(one unit change in bmidiff corresponds to a 2.105 increase in odds to fail) 

m81 <- glm(ptfail ~ bmidif, family = binomial(link = logit))

#Quick Plots
par(mfrow = c(1, 2))
plot(bmidif, predict(m81),
	main = "Plot of Predicted loglikelihood")
abline(h = 0)

plot(bmidif, ptfail ,
	main = "Plot of observed Values and Sigmoid Probability")
points(bmidif,
	(exp(predict(m81)) / (1 + exp(predict(m81)))),
	col = "green")
abline(h = .5)





#############################################################
#############################################################

#Fisher's Z-test (Continued from Multiple Regression Example)

mcor <- cor(d[which(d$female == 0), "lonely"],
            d[which(d$female == 0), "depress"])

fcor <- cor(d[which(d$female == 1), "lonely"],
            d[which(d$female == 1), "depress"])

mcorz <- .5 * log((1 + mcor) / (1 - mcor))
mcorz

fcorz <- .5 * log((1 + fcor) / (1 - fcor))
fcorz

zres <- (fcorz - mcorz) / sqrt((1/97) + (1/97))


#t vs. z value comparison
zres


#p-value comparison
2 * pnorm(zres, lower.tail = FALSE)

m6


#What if the difference were larger?

d4m <- as.data.frame(mvrnorm(100, c(4, 3.1), 
                             matrix(c(1, .05, .05, 1), nrow = 2, ncol = 2, byrow = TRUE), 
                             empirical = TRUE))
colnames(d4m) <- c("lonely", "depress")

d4f <- as.data.frame(mvrnorm(100, c(3.5, 2.2), 
                             matrix(c(1, .35, .35, 1),nrow = 2,ncol = 2, byrow = TRUE), 
                             empirical = TRUE))
colnames(d4f) <- c("lonely", "depress")

d2 <- rbind(d4m, d4f)
d2$female <- rep(c(0, 1), each = 100)

m7 <- summary(lm(depress ~ lonely*female, data = d2))

mcor2 <- cor(d2[which(d2$female == 0), "lonely"],
             d2[which(d2$female == 0), "depress"])

fcor2 <- cor(d2[which(d2$female == 1), "lonely"],
             d2[which(d2$female == 1), "depress"])

mcorz2 <- .5 * log((1 + mcor2) / (1 - mcor2))
mcorz2

fcorz2 <- .5 * log((1 + fcor2) / (1 - fcor2))
fcorz2

zres2 <- (fcorz2 - mcorz2) / sqrt((1/97) + (1/97))


#t vs. z value comparison
zres2


#p-value comparison
2 * pnorm(zres2, lower.tail = FALSE)
m7
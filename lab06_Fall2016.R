#Kristine Christianson:
#     Lab: Fridays 10-11:50am
#Nate Smith
#     Lab: Fridays 12:10-2:00pm

#204a R Lab 6
#Fall 2016
#####################################################################

#We'll make some data (same that we used last week)
cneg <- rep(c(1,0,0), each = 20)
cpos <- rep(c(0,1,0), each = 20)

set.seed(1023)
score <- 5 + 1.5*cpos + -1.3*cneg + rnorm(60, 0, 2)
condition <- rep(c("Negative","Positive","Control"), each = 20)
d <- data.frame(score,condition,cneg,cpos)
by(d$score,d$condition,mean)



########################################################


#Post-hoc comparions


########################################################

#Let's save our model as an object

mod1 <- aov(score ~ condition, data = d)
#check ?aov and ?lm for more details, but essentially
#both require you to describe a model and data set
#to fit the model to.
#e.g.,
#aov(outcome ~ predictor1, data = data)


#Tukey's Honestly Significant Difference pairwise

TukeyHSD(mod1)
#You need to wrap TukeHSD() around a model object from aov()

#Other pairwise comparison options

?pairwise.t.test
#pairwise.t.test(x, g, p.adjust.method = p.adjust.methods,
#                pool.sd = !paired, paired = FALSE,
#                alternative = c("two.sided", "less", "greater"),
#                ...)

#This works much like the t.test function with a few
#important differences
#x is the outcome
#g is the grouping factor for comparisons
#p.adjust.method has a special value

?p.adjust
#p.adjust.methods
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")

#The above list gives the adjustment methods available.
#Look at the p.adjust description page for some details
#about the full range of adjustments and references for them.

pairwise.t.test(d$score, d$condition,
                p.adjust.method = c("bonferroni"))
#bonferroni essentially divids your alpha by the number
#of comparisons you're making.

pairwise.t.test(d$score, d$condition,
                p.adjust.method = c("none"))
#These are the p-values if no adjustment was made.

pairwise.t.test(d$score, d$condition,
                p.adjust.method = c("holm"))
#This is the adjustment the R help file for paired.t.test()
#seems to recommend.

#The adjustment you use can very easily alter your conclusions.
#Thus, selection criteria will generally need be justified.
#Unless you use Bonferroni, it tends to be the most stringent
#correction and is therefore less likely to be criticized
#as being too liberal.

######################################
#Install R package
######################################

#Install package
install.packages("car")
library(car)


######################################

#Factorial ANOVA

######################################

#Data Description
#The A1C is a blood test that provides information about a person's average levels
#of blood glucose over the past 3 months.  In this example(using contructed data),
#we have a sample of male and female diabetics, who are randomly assigned to one
#three groups: Drug 1, Drug 2, or Placebo.  Drug 1 and Drug 2 are new drugs designed
#to help combat high blood glucose levels.  

######################################


#Data
set.seed(1113)
female <- rep(c(0,1), each = 50)

drug1 <- drug2 <- rep(0, 100)
drug1;drug2

drug1[sample(1:100, 33, replace = FALSE)] <- 1
drug1

drug2[sample(which(drug1 == 0), 33, replace = FALSE)] <- 1
drug2

dose <- rep("P", 100)
dose[which(drug1 == 1)] <- "D1"
dose[which(drug2 == 1)] <- "D2"

a1c <- 12 - 2*drug1 - 7*drug2 - 2*female -3*drug1*female + 
     5.5*drug2*female + rnorm(100,0,3)

d = data.frame(a1c, female, dose)


#####################################
#Descriptives
#####################################

by(d$a1c, d$dose, mean)

by(d$a1c, d$female, mean)

by(d$a1c, list(d$dose, d$female), mean)



#####################################

#ANOVA and Factorial ANOVA

#####################################
#One-way ANOVAS

anova(lm(d$a1c ~ d$dose))

anova(lm(d$a1c ~ d$female))


##################################
#Factorial ANOVA, no Interaction

anova(lm(d$a1c ~ d$female + d$dose))


##################################
#Factorial ANOVA, Interaction

anova(lm(d$a1c ~ d$female + d$dose + d$female*d$dose))

anova(lm(d$a1c ~ d$female*d$dose))

anova(lm(d$a1c ~ d$dose*d$female))
#These differ, why? They are the same model ...
#The default in R is Type 1 Sums of Squares
#For this type of SS, order matters!

library(car)

Anova(lm(d$a1c ~ d$female*d$dose), type = c(2))
#Type 2 vs. 1 SS


##################################################

#Simple Effects

#We can evaluate the effect of Bio Sex across different
#Dose conditions, or we can evaluate the effect of
#Dose across different Bio Sex conditions
#Which makes more sense given the questions you are trying
#to evaluate?

#I'll split data by bio sex, as I'm interested in how dose
#condition effects may differ by bio sex

a1cf <- d$a1c[which(d$female == 1)]
dosef <- d$dose[which(d$female == 1)]

a1cm <- d$a1c[which(d$female == 0)]
dosem <- d$dose[which(d$female == 0)]

#females
anova(lm(a1cf ~ dosef))

#males
anova(lm(a1cm ~ dosem))

#Pairwise comparisons
#####################

pairwise.t.test(a1cf, dosef, p.adjust = c("bonf"))
by(a1cf, dosef, mean)

pairwise.t.test(a1cm, dosem, p.adjust=c("bonf"))
by(a1cm, dosem, mean)

#Got it, so Drug1 is best for female diabetics
#and Drug2 is best for male diabetics looking to control
#blood glucose levels

##############################

#Plot it

##############################

Sex = factor(d$female)

interaction.plot(factor(d$dose), Sex, d$a1c, 
                 type = 'b', leg.bty = "o", leg.bg = "gray", lwd = 2,
                 pch = c(18, 24), xlab = 'Dose', ylab = 'A1C Score',
                 main = 'Interaction Plot of Dose*Sex')

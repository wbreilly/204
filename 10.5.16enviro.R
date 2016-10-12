#   > tapply(HW01Data$age_yr, HW01Data$ed_cmplt, mean)
# College2YR College4YR         HS         MA        PhD 
# 20.01176   23.14118   24.80256   25.10182   29.83784 
# > 
#   > females = filter(HW01Data$biosex == "FEMALE")
# Error in filter(HW01Data$biosex == "FEMALE") : 
#   argument "filter" is missing, with no default
# > ? filter
# > ? dplyr
# No documentation for ‘dplyr’ in specified packages and libraries:
#   you could try ‘??dplyr’
# > females = HW01Data[HW01Data$biosex == "FEMALE"]
# Error in `[.data.frame`(HW01Data, HW01Data$biosex == "FEMALE") : 
#   undefined columns selected
# > females = HW01Data[:,HW01Data$biosex == "FEMALE"]
# Error: unexpected ':' in "females = HW01Data[:"
# > females = subset(HW01Data, HW01Data$biosex == "FEMALE")
# > View(females)
# > View(females)
# > males = subset(HW01Data, HW01Data$biosex == "MALE")
# > fe.age.ed = tapply(females$age_yr, females$ed_cmplt, mean)
# > fe.age.ed
# College2YR College4YR         HS         MA        PhD 
# 19.97895   23.08378   25.63333   25.06486   30.01765 
# > ma.age.ed = tapply(males$age_yr, males$ed_cmplt, mean)
# > ma.age.ed
# College2YR College4YR         HS         MA        PhD 
# 20.03125   23.29286   24.55333   25.17778   29.68500 
# > ?table
# > sex.age.ed = cbind(fe.age.ed,ma.age.ed)
# > sex.age.ed
# fe.age.ed ma.age.ed
# College2YR  19.97895  20.03125
# College4YR  23.08378  23.29286
# HS          25.63333  24.55333
# MA          25.06486  25.17778
# PhD         30.01765  29.68500
# > table(sex.age.ed)
# sex.age.ed
# 19.9789473684211         20.03125 23.0837837837838 23.2928571428571 24.5533333333333 
# 1                1                1                1                1 
# 25.0648648648649 25.1777777777778 25.6333333333333           29.685 30.0176470588235 
# 1                1                1                1                1 
# > as.table(sex.age.ed)
# fe.age.ed ma.age.ed
# College2YR  19.97895  20.03125
# College4YR  23.08378  23.29286
# HS          25.63333  24.55333
# MA          25.06486  25.17778
# PhD         30.01765  29.68500


# add a column
HW01Data[, "BMI"] <- HW01Data

calc.bmi = function(weight_lbs, height_ins)
{
  bmi = weight_lbs / height_ins^2 * 703
  return(bmi)
}


#HW01Data$BMI = sapply(HW01Data, calc.bmi(weight_lbs,height_in))
# HW01Data[,"BMI"]  = apply(HW01Data, 1, mean)

library(tidyverse)

# 7a
HW01Data = HW01Data %>%
  mutate(BMI = weight_lbs / height_in^2 * 703)
   
HW01Data %>%
  group_by(biosex, ed_cmplt) %>%
  mutate(mean_age = mean(age_yr))

#7b
HW01Data %>%
  group_by(biosex) %>%
    summarise(sex.bmi = mean(BMI))
  
#7c
# if bmi is in a certain range, apply the appropriate label labels

bmi.class = function(BMI)
{
  if (BMI < 18.5)
  return("underweight")
  else if (BMI >= 18.5 & BMI <= 24.9)
  return("normal")
  else if (BMI >= 25 )
  return("overweight")
}
HW01Data = HW01Data %>%
   mutate(cats = bmi.class(BMI))

#apply function to every BMI value
HW01Data$cats =  tapply(HW01Data, bmi.class(HW01Data$BMI))


# maybe it's the function



bmi.class = function(idx)
{
  if (idx < 18.5)
  {cat = "underweight"}
  else if (idx >= 18.5 & idx <= 24.9)
  {cat = "normal"}
  else if (idx >= 25 )
  {cat = "overweight"}
  return(cat)
}
nrows = nrow(bims)
#allocate matrix
cats = matrix(data=NA,nrow=nrows,ncol=1)

for(i in 1:nrows) 
  {
  cats[i,1] = bmi.class(bims[i,1]) 
  }

# more clumsy way
bmis = HW01Data[,"BMI"]

# finally

HW01Data <- mutate(HW01Data, 
                   underweight = BMI < 18.5,
                   normal = BMI >= 18.5 & BMI <= 24.9, 
                   overweight = BMI >= 25)

#this didnt work
# HW01Data %>%
#   group_by(biosex,underweight,normal,overweight) %>%
#   summary()
    

summary(HW01Data[HW01Data$underweight,"biosex"]) 
summary(HW01Data[HW01Data$normal,"biosex"]) 
summary(HW01Data[HW01Data$overweight,"biosex"]) 

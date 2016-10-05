# > load("/Users/WBR/walter/204_stats/9.30.16enviro.RData")
# > View(HW01Data)
# > View(HW01Data)
# > tapply(HW01Data$age_yr, HW01Data$ed_cmplt, HW01Data$biosex, mean)
# Error in match.fun(FUN) : 
#   'HW01Data$biosex' is not a function, character or symbol
# > 
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
HW01Data[ , "BMI"] <- HW01Data

calc.bmi = function(weight,height)
{
  bmi = weight / height^2 * 703
  return(bmi)
}


#HW01Data$BMI = sapply(HW01Data, calc.bmi(weight_lbs,height_in))
HW01Data[,"BMI"]  = sapply(HW01Data, calc.bmi, weight_lbs, height_in, simplify = F)
# comment
# annotate what you're doing 
# c("concatenate", "words", "or numbers")
# ? gives help info 
# ?? searches keywords 
# class()
# class(save_that) - tell you the type of object 

# you can name a variable with an = or ->
# deesedate = c(1,10)

# create the matrix
# thematrix = matrix(NA, nrow = 2, ncol = 4)

example = matrix(c(1:4), nrow = 2, ncol = 4)

# matrix(c(1:8), nrow = 2, ncol = 4, byrow = TRUE)
# makes #      [,1] [,2] [,3] [,4]
#             [1,]    1    2    3    4
#            [2,]    5    6    7    8

#indexing is basically the same at MATLAB
# index into matrix "example"
# example[1,] first row, all columns 

# make a dataframe
 my_first_df = data.frame(example, thematrix)
summary(my_first_df)

# table() is another summary function

table(thematrix)
table(example)

# tells you the class of every column 
sapply(my_first_df, class)

#how about the mean for every column 
sapply(my_first_df, mean)

% so anyway benefit of R in genereal is words and numbers in a data frame. 

#####################################################

names(my_first_df)

colnames(my_first_df)

colnames(my_first_df) = c("C1", "C2")

### missed a few things here 

# x != y # not equal to 

# & both conditions must be TRUE
# x | y # "pipe" one or other 

# which(my_first_df[,1] ==1) # look through all rows in first column...
# and return rows that equal 1

# what does $ do? 

by(my_first_df$C1, table)

View(my_first_df)

# which values are missing
# which(is.na(my_first_df$C1) == TRUE)

# summary stats when therer are "NAs" won't work
mean(my_first_df$C1, na.rm = TRUE)


















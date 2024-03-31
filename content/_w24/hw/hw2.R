library(oibiostat)

## Question 1
##############
data(frog)
?frog # read variable descriptions

# clutch volume distribution, center, and spread

# egg size distribution, center, and spread

# clutch size distribution, center, and spread


## Question 6
##############
data(yrbss)
?yrbss # read variable descriptions

# a. racial composition of survey respondents by grade

# b. distribution of hours of sleep on school nights

# c. do older students sleep more than younger students?

# d. distribution of number of physically active days

# e. physically active days per grade (boxplots)


## Question 7
##############
anger <- matrix(data = c(53, 110, 27, 3057, 4704, 606), 
                nrow = 2, 
                byrow = T, 
                dimnames = list(chd = c('yes', 'no'),
                                anger = c('low', 'moderate', 'high'))
                )
anger

# a. percentage of participants with moderate anger scores

# b. percentage of participants who experienced a CHD event with moderate scores

# c. percentage with high anger who experienced a CHD event

# d. percentage with low anger who experienced a CHD event

# e. relative risk of CHD, high anger to low anger

# f. proportional bar plot
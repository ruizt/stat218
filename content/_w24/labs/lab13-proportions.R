## ----------------------------------------------------------------------------------------------------------
library(oibiostat)
data("nhanes.samp.adult.500")
nhanes <- nhanes.samp.adult.500
load('data/prop-cases.RData')


## ----creating a factor-------------------------------------------------------------------------------------
# format as factor
diabetes <- factor(nhanes$Diabetes, levels = c('Yes', 'No'))

# inspect
str(diabetes)


## ----computing sample statistics---------------------------------------------------------------------------
# sample proportions
diabetes.props <- table(diabetes) |> prop.table()

# equivalent to...
diabetes.tbl <- table(diabetes)
prop.table(diabetes.tbl)

# point estimate and se
p.hat <- diabetes.props[1]
p.hat.se <- sqrt(p.hat*(1 - p.hat)/length(diabetes))


## ----inference for one proportion--------------------------------------------------------------------------
# test whether prevalence is 10% or not
table(diabetes) |> prop.test(p = 0.1, alternative = 'two.sided', conf.level = 0.99)

# equivalent to
prop.test(diabetes.tbl, p = 0.1, alternative = 'two.sided', conf.level = 0.99)

## ----your turn 1-------------------------------------------------------------------------------------------
# does prevalence exceed 9%? modify below
table(diabetes) |> prop.test(p = 0.09, alternative = 'greater', conf.level = 0.95,
                             correct = F)

# to get you started
trouble <- factor(nhanes$SleepTrouble, 
                  levels = c('Yes', 'No'))

# frequency distribution table
trouble.tbl <- table(trouble)

# inference: test whether proportion exceeds 0.2
trouble.test <- prop.test(trouble.tbl, p = 0.2, alternative = 'greater', conf.level = 0.95)
p.hat <- trouble.test$estimate
sqrt(p.hat*(1 - p.hat)/500)

## ----summary statistics for two proportions----------------------------------------------------------------
# summary statistics
cold |> prop.table(margin = 1)


## ----inference for two proportions-------------------------------------------------------------------------
# test whether the proportions are the same
prop.test(cold, alternative = 'two.sided', conf.level = 0.95)


## ----your turn 2-------------------------------------------------------------------------------------------
# does vitamin C reduce the probability of a cold? modify the command below
prop.test(cold, alternative = 'greater', conf.level = 0.95)


## ----practice problem 1------------------------------------------------------------------------------------
# data
obesity
cbind(obesity, n = rowSums(obesity))

# compute summary statistics
prop.table(obesity, margin = 1)

# perform test
prop.test(obesity, alternative = 'two.sided', conf.level = 0.95)


## ----practice problem 2------------------------------------------------------------------------------------
# data
smoking

# compute summary statistics
prop.table(smoking, margin = 1)

# perform test
prop.test(smoking, alternative = "two.sided", conf.level = 0.95)


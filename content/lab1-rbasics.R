## ----load a package-------------------------------------------------------
library(tidyverse)


## ----loading data---------------------------------------------------------
# load nhanes data
load('data/nhanes.RData')


## ----previewing data------------------------------------------------------
# first few rows
head(nhanes)


## ----your turn 1----------------------------------------------------------
# load famuss dataset

# preview first few rows



## ----selecting a variable-------------------------------------------------
# extract total cholesterol
total.cholesterol <- nhanes$totchol

# preview first few values
head(total.cholesterol)


## ----your turn 2----------------------------------------------------------
# store the change in arm strength variable as a vector called 'strength'

# preview the first few values



## ----numerical summary stats----------------------------------------------
# average total cholesterol
mean(total.cholesterol)

# minimum
min(total.cholesterol)

# maximum
max(total.cholesterol)


## ----your turn 3----------------------------------------------------------
# compute mean change in nondominant arm strength



## ----categorical variable counts------------------------------------------
# retreive sex variable
sex <- nhanes$gender

# counts
table(sex)


## ----categorical variable proportions-------------------------------------
# proportions
table(sex) |> proportions()


## ----pipe operator--------------------------------------------------------
# same as above
sex |> table() |> proportions()


## ----your turn 4----------------------------------------------------------
# retrieve genotype

# counts

# proportions


## ----putting previous steps together, eval = F----------------------------
# import nhanes data
load('data/nhanes.RData')

# inspect data
head(nhanes)

# extract total cholesterol
total.cholesterol <- nhanes$totchol

# compute average total cholesterol
mean(total.cholesterol)

# extract sex
sex <- nhanes$gender

# proportions of men and women in sample
table(sex) |> proportions()


## ----your turn 5----------------------------------------------------------
# load famuss dataset

# inspect data

# extract nondominant change in arm strength

# compute average change in strength

# extract genotype

# compute genotype frequencies (proportions)


## ----reading csv files, results = 'hide'----------------------------------
# parse a csv file
read.csv('data/gss.csv')

# store the result in the environment
gss <- read.csv('data/gss.csv')


## ----problem 1, echo = F, eval = F---------------------------------------
# load dataset

# part a: youngest and oldest individual in sample

# part b: average total personal income

# part c: average total family income

# parts d, e: number of variables and categorical variables


## ----problem 2, echo = F, eval = F---------------------------------------
# load data
data('cdc.samp', package = 'oibiostat')

# part a: what do 0s and 1s mean in exerany variable?
?oibiostat::cdc.samp

# part b: proportion of men and women

# part c: general health proportions

# part d: count and percentage of respondents with health coverage

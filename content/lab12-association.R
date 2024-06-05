## ----setup-------------------------------------------------------------------------------------------------
library(tidyverse)
load('data/asthma.RData')
load('data/diabetes_meds.RData')
load('data/famuss.RData')


## ----basic use of chisq.test-------------------------------------------------------------------------------
# make a two-way table
table(asthma$sex, asthma$asthma) 

# pass to chisq.test
table(asthma$sex, asthma$asthma) |>
  chisq.test()


## ----your turn 1-------------------------------------------------------------------------------------------
# test whether diabetes medication is associated with cardiovascular problems



## ----expected counts---------------------------------------------------------------------------------------
# store test result
asthma.rslt <- table(asthma$sex, asthma$asthma) |>
  chisq.test()

# view expected counts to check assumptions
asthma.rslt$expected


## ----your turn 2-------------------------------------------------------------------------------------------
# store test result

# view expected counts to check assumptions



## ----warning if conditions fail, warning = T---------------------------------------------------------------
# example using a dataset with low counts
openintro::malaria |> table() |> chisq.test()


## ----verify assumptions fail-------------------------------------------------------------------------------
# all expected counts are under ten
malaria.rslt <- openintro::malaria |> table() |> chisq.test()
malaria.rslt$expected


## ----residual analysis-------------------------------------------------------------------------------------
# view residuals
asthma.rslt$residual


## ----your turn 3-------------------------------------------------------------------------------------------
# check residuals from your inference above; what explains the association?



## ----measuring association by a difference in proportions--------------------------------------------------
# use prop.test to get estimates and confidence interval
table(asthma$sex, asthma$asthma) |>
  prop.test(conf.level = 0.90)


## ----your turn 4-------------------------------------------------------------------------------------------
# estimate the difference in proportions of patients experiencing cardiovascular problems



## ----chisq test for general twoway tables------------------------------------------------------------------
# perform test
famuss.rslt <- table(famuss$race, famuss$genotype) |>
  chisq.test()

# check expected counts to assess assumptions
famuss.rslt$expected

# interpret test result
famuss.rslt

# residual analysis
famuss.rslt$residuals


## ----your turn 5-------------------------------------------------------------------------------------------
# perform test

# check expected counts to assess assumptions

# interpret test result

# residual analysis



## ----practice problem 1, include = F-----------------------------------------------------------------------
# load and inspect data

# perform calculations for chi-square test

# part b: check assumptions

# part c: if appropriate, interpret test result

# part c: if test result is significant, perform a residual analysis


## ----practice problem 2, include = F-----------------------------------------------------------------------
# load and inspect data

# part a: construct a contingency table

# perform chi-square test calculations

# part c: check assumptions

# part d: if appropriate, perform inference at the 5% level

# part e: make a plot of residuals (extra credit)


## ----practice problem 3, include = F-----------------------------------------------------------------------
# load and inspect data

# extract variable of interest

# part a: point estimate and standard error for proportion with college degree

# part b: 99% interval

# part c: test whether proportion exceeds 3 in 10 at 5% level


## ----practice problem 4, include = F-----------------------------------------------------------------------
# load and inspect data

# part a: check assumptions

# part b: if appropriate, perform test and residual analysis at 1% significance level


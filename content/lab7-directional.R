## ----data and packages-------------------------------------------
library(tidyverse)
load('data/temps.RData')
load('data/nhanes.RData')
ncbirths <- read_csv('data/ncbirths.csv')


## ----the t.test function-----------------------------------------
# extract sleep variable
sleep <- nhanes$sleephrsnight

# default behavior (these are equivalent)
t.test(sleep)
t.test(sleep, mu = 0, alternative = 'two.sided', conf.level = 0.95)

# change null value to 7 hours of sleep
t.test(sleep, mu = 7, alternative = 'two.sided', conf.level = 0.95)

# change the confidence level
t.test(sleep, mu = 7, alternative = 'two.sided', conf.level = 0.99)

# change the direction of the alternative
t.test(sleep, mu = 7, alternative = 'less', conf.level = 0.99)


## ----your turn 1-------------------------------------------------
# obtain a 90% confidence interval for the mean hours of sleep

# test whether mean sleep is 6.9 at the 5% level

# test whether mean sleep is 6.9 at the 1% level

# test whether mean sleep exceeds 6.9 at the 5% level

# test whether mean sleep exceeds 6.9 at the 1% level



## ----distinguishing directions, echo = T, results = 'hide'-------
# extract body temperature variable
bodytemps <- temps$body.temp

# is mean temperature different from 98.6 at the 5% significance level?
t.test(bodytemps, mu = 98.6, alternative = 'two.sided', conf.level = 0.95)

# is mean temperature less than 98.6 at the 5% significance level?
t.test(bodytemps, mu = 98.6, alternative = 'less', conf.level = 0.95)

# is mean temperature greater than 98.1 at the 5% significance level?
t.test(bodytemps, mu = 98.1, alternative = 'greater', conf.level = 0.95)

# is mean temperature greater than 98.1 at the 1% significance level?
t.test(bodytemps, mu = 98.1, alternative = 'greater', conf.level = 0.99)

# is mean temperature less than 98.9 at the 5% significance level?
t.test(bodytemps, mu = 98.9, alternative = 'less', conf.level = 0.95)


## ----your turn 2-------------------------------------------------
# extract heart rate variable

# is mean heart rate 65bpm at the 5% level?

# is mean heart rate 70bpm at the 1% level?

# is mean heart rate greater than 70bpm at the 1% level?

# is mean heart rate less than 75bpm at the 10% level?

# is mean heart rate greater than 75bpm at the 10% level?



## ----analysis example: checking assumptions----------------------
# extract variable of interest
weeks <- ncbirths$weeks

# inspect distribution
hist(weeks, breaks = 15)


## ----analysis example: performing the test-----------------------
# inference
t.test(weeks, mu = 40, alternative = 'two.sided', conf.level = 0.95)


## ----practice problem 1, echo = F, eval = F----------------------
## # extract variable of interest
## 
## # inspect distribution to check assumptions
## 
## # perform test
## 


## ----practice problem 2, echo = F, eval = F----------------------
## # load data
## 
## # extract variable of interest
## 
## # inspect distribution to check assumptions
## 
## # perform test
## 


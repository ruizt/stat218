## ----packages and data----------------------------------------------------------------------------
library(tidyverse)
load('data/temps.RData')
head(temps)


## ----checking test assumptions, fig.width = 4, fig.height = 3-------------------------------------
# extract temperature variable
bodytemp <- temps$body.temp

# location measures
summary(bodytemp)

# histogram
hist(bodytemp, breaks = 10)


## ----your turn 1----------------------------------------------------------------------------------
# extract heartrate variable

# location measures

# histogram



## ----calculating the t statistic------------------------------------------------------------------
# store sample mean and standard error
bodytemp.mean <- mean(bodytemp)
bodytemp.mean.se <- sd(bodytemp)/sqrt(length(bodytemp))

# calculate t statistic
bodytemp.tstat <- (bodytemp.mean - 98.6)/bodytemp.mean.se
bodytemp.tstat


## ----your turn 2----------------------------------------------------------------------------------
# store sample mean and standard error for heart rate

# calculate t statistic

## ----computing critical values--------------------------------------------------------------------
# to control error rate at 10%, use this critical value
qt(0.95, df = 38)

# to control error rate at 5%, use this critical value
qt(0.975, df = 38)

# to control error rate at 1%, use this critical value
qt(0.995, df = 38)


## ----your turn 3----------------------------------------------------------------------------------
# to control error rate at 20%, use this critical value



## ----your turn 4----------------------------------------------------------------------------------
# to control error rate at 5%, use this critical value

# decision whether mean heart rate is 75bpm?



## ----computing a p value--------------------------------------------------------------------------
2*pt(abs(bodytemp.tstat), df = 38, lower.tail = F)


## ----your turn 5----------------------------------------------------------------------------------
# p value for test of whether mean heart rate is 75bpm



## ----practice problems----------------------------------------------------------------------------
# nhanes data

# extract sleep variable

# part a: assess assumptions

# part b: point estimate and se

# part d: calculate the test stat, critical value (5%), and p-value

# part f: calculate a 95% confidence interval



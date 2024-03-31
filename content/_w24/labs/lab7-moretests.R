## ----load datasets-----------------------------------------------------------------------------------------------------------------------------------------
library(oibiostat)

data(nhanes.samp.adult)
sleep <- nhanes.samp.adult$SleepHrsNight
str(sleep)


## ----review------------------------------------------------------------------------------------------------------------------------------------------------
# do US adults sleep either more or less than 8 hours a night? [result: yes]
t.test(sleep, mu = 8, alternative = 'two.sided')

# do US adults sleep more than 8 hours a night? [result: no]
t.test(sleep, mu = 8, alternative = 'greater')

# do US adults sleep less than 8 hours a night? [result: yes]
t.test(sleep, mu = 8, alternative = 'less')


## ----your turn 1-------------------------------------------------------------------------------------------------------------------------------------------
# 1. Do US adults sleep 7.5 hours per night on average?

# 2. Do US adults sleep less than 7.5 hours per night on average?

# 3. Do US adults sleep more than 7.5 hours per night on average?

# 4. Do US adults sleep more than 6.5 hours per night on average?



## ----changing confidence levels----------------------------------------------------------------------------------------------------------------------------
# a 99% interval
t.test(sleep, mu = 8, alternative = 'two.sided', conf.level = 0.99)

# an 80% interval
t.test(sleep, mu = 8, alternative = 'two.sided', conf.level = 0.8)


## ----your turn 2, eval = F---------------------------------------------------------------------------------------------------------------------------------
## # fill in the '...' parts to obtain an 87% upper confidence bound
## t.test(sleep,
##        mu = ...,
##        alternative = ...,
##        conf.level = ...)


## ----load nhanes data--------------------------------------------------------------------------------------------------------------------------------------
load('data/nhanes.RData')

# "true" population mean
pop_mean <- mean(nhanes$TotChol)


## ----simulating type i errors, results = 'hide'------------------------------------------------------------------------------------------------------------
# draw a sample 
samp <- sample(nhanes$TotChol, size = 20)

# test a true null
t.test(samp, mu = pop_mean, alternative = 'two.sided')


## ----simulating type ii errors, results = 'hide'-----------------------------------------------------------------------------------------------------------
# draw a sample 
samp <- sample(nhanes$TotChol, size = 20)

# test a false null
t.test(samp, 
       mu = 4.2, # change this for exercise
       alternative = 'two.sided')


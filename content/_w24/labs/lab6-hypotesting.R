## ----------------------------------------------------------------------------------------------------------------------------------------------------------
library(oibiostat)

ddt <- MASS::DDT
str(ddt)

data(nhanes.samp.adult)
sleep <- nhanes.samp.adult$SleepHrsNight
str(sleep)

data(thermometry)
body.temps <- thermometry$body.temp
str(body.temps)


## ----upper-sided test--------------------------------------------------------------------------------------------------------------------------------------
# upper sided test H0: mean ddt <= 3 vs HA: mean ddt > 3
t.test(ddt, mu = 3, alternative = 'greater')


## ----lower-sided test--------------------------------------------------------------------------------------------------------------------------------------
# lower-sided test, H0: mean ddt >= 3.5 vs HA: mean ddt < 3.5
t.test(ddt, mu = 3.5, alternative = 'less')


## ----two-sided test----------------------------------------------------------------------------------------------------------------------------------------
# two-sided test, H0: mean ddt == 3 vs HA: mean ddt =!= 3
t.test(ddt, mu = 3, alternative = 'two.sided')


## ----your turn---------------------------------------------------------------------------------------------------------------------------------------------
# 1. Is the mean body temperature different from 98.6 °F?

# 2. Is the mean body temperature higher than 98 °F?

# 3. Is the mean body temperature higher than 98.2 °F?

# 4. Is the mean body temperature lower than 98.2 °F?

# 5. Is the mean body temperature actually 98.2 °F?

# 6. Is the mean body temperature actually 98.3 °F?



## ----body temp plot, echo = F------------------------------------------------------------------------------------------------------------------------------
hist(body.temps, 
     xlab = 'body temperature',
     ylab = 'frequency',
     main = '',
     breaks = 20)
abline(v = mean(body.temps), col = 2)
legend(x = 'topright', 
       legend = paste('sample mean', round(mean(body.temps), 2), sep = ' '), 
       col = 2, 
       lty = 1)


## ----your turn 2-------------------------------------------------------------------------------------------------------------------------------------------
# 1. Do US adults sleep 7.5 hours per night on average?

# 2. Do US adults sleep less than 7.5 hours per night on average?

# 3. Do US adults sleep more than 7.5 hours per night on average?

# 4. Do US adults sleep more than 6.5 hours per night on average?



## ----simulating type i errors------------------------------------------------------------------------------------------------------------------------------
load('data/nhanes.RData')

# "true" population mean
pop_mean <- mean(nhanes$TotChol)

# draw a sample 
samp <- sample(nhanes$TotChol, size = 20)

# test a true null
t.test(samp, mu = pop_mean, alternative = 'two.sided')


## ----simulating type ii errors-----------------------------------------------------------------------------------------------------------------------------
# draw a sample 
samp <- sample(nhanes$TotChol, size = 20)

# test a false null
t.test(samp, 
       mu = 4.2, # change this for exercise
       alternative = 'two.sided')


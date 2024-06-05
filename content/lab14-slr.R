## ----setup-------------------------------------------------------------
library(tidyverse)
load('data/prevend.RData')
load('data/kleiber.RData')
load('data/mammals.RData')


## ----scatterplots with formula input-----------------------------------
# scatterplot of rfft against age
plot(rfft ~ age, data = prevend)


## ----your turn 1-------------------------------------------------------
# scatterplot of log brain size against log body size



## ----fitting linear models with lm-------------------------------------
# slr model of rfft (response) by age (explanatory)
fit.rfft <- lm(rfft ~ age, data = prevend)


## ----your turn 2-------------------------------------------------------
# slr model of log brain size by log body size



## ----model visualization-----------------------------------------------
# reproduce the scatterplot, then add a line using abline
plot(rfft ~ age, data = prevend)
abline(reg = fit.rfft, col = 'blue', lwd = 2)


## ----your turn 3, include = F------------------------------------------
# reproduce the scatterplot, then add a line using abline



## ----model summary-----------------------------------------------------
# inspect model summary for estimates, inference, and measures of fit
summary(fit.rfft)


## ----your turn 4, include = F------------------------------------------
# inspect model summary for estimates, inference, and measures of fit



## ----confidence intervals for coefficients-----------------------------
# 95% confidence intervals
confint(object = fit.rfft, level = 0.95)

# 99% confidence intervals
confint(object = fit.rfft, level = 0.99)

# for slope only...
confint(object = fit.rfft, parm = 'age', level = 0.99)


## ----your turn 5, include = F------------------------------------------
# 95% confidence interval



## ----practice problem, include = F-------------------------------------
# part a: fit model of log metabolism on log mass

# part b: visualize fitted model

# part c-d: inspect model summary

# part e: 99% confidence interval for slope


## ----setup----------------------------------------------------------------------
library(tidyverse)
load('data/prevend.RData')
load('data/mammals.RData')
hand.fit <- readRDS('fns/handfit.rds')
sim.by.cor <- readRDS('fns/simbycor.rds')


## ----constructing scatterplots--------------------------------------------------
# variables of interest
age <- prevend$age
rfft <- prevend$rfft

# scatterplot of RFFT score against age
plot(age, rfft)

# alternate syntax: formula
plot(rfft ~ age)

# change labels
plot(rfft ~ age, xlab = 'age', ylab = 'RFFT score')


## ----your turn 1, include = F---------------------------------------------------
# variables of interest

# scatterplot of log brain weight against log body weight (use formula syntax and adjust labels)



## ----computing a correlation----------------------------------------------------
# correlation between age and rfft
cor(age, rfft)


## ----your turn 2----------------------------------------------------------------
# correlation between log brain and log body



## ----visualize data with different correlations---------------------------------
# adjust r to see what different correlations look like in simulated data
sim.by.cor(r = 0, n = 500)


## ----hand fitting a line to the prevend data------------------------------------
# i adjusted a and b until the line reflected the trend well
hand.fit(x = age, y = rfft, a = 145, b = -1.3)


## ----your turn 3, include = F---------------------------------------------------
# adjust a and b until the line reflects the trend between log brain and log body weights well



## ----visualize residuals--------------------------------------------------------
# add residuals to the plot
hand.fit(x = age, y = rfft, a = 145, b = -1.3, res = T)


## ----your turn 4, include = F---------------------------------------------------
# add residuals to your line from before




## ----more positive bias---------------------------------------------------------
# obvious positive bias
hand.fit(x = age, y = rfft, a = 165, b = -1.3, res = T)


## ----your turn 5, include = F---------------------------------------------------
# positive bias

# negative bias

# low bias




## ----no bias but high error-----------------------------------------------------
# no bias but high error
hand.fit(x = age, y = rfft, a = mean(rfft), b = 0, res = T)


## ----your turn 6, include = F---------------------------------------------------
# low bias but high error



## ----constrain the line to pass through center----------------------------------
# omit intercept to force the line through the center point
hand.fit(x = age, y = rfft, b = -1.2, res = T)


## ----see how the intercept is calculated----------------------------------------
# calculate intercept by direct arithmetic
mean(rfft) - (-1.2)*mean(age)


## ----approximate best unbiased line---------------------------------------------
# fine tune slope to minimize sse by hand
hand.fit(x = age, y = rfft, b = -1.19, res = T)


## ----exact best unbiased line---------------------------------------------------
# analytic solution 
hand.fit(age, rfft, res = T,
         b = cor(rfft, age)*sd(rfft)/sd(age))


## ----your turn 7, include = F---------------------------------------------------
# find exact best unbiased line for mammals data




## ----practice problem 1, include = F--------------------------------------------
# load and inspect data
load('data/kleiber.RData')

# part a: scatterplot

# part b: compute and interpret correlation

# part c: find the best unbiased line


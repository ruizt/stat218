## ----packages and data----------------------------------------------------------------------------------------------------------
library(tidyverse)
load('data/nhanes.RData')
load('data/temps.RData')


## ----point estimates for mean and sd--------------------------------------------------------------------------------------------
# retrieve total cholesterol variable
totchol <- nhanes$totchol

# compute and store sample mean
totchol.mean <- mean(totchol)
totchol.mean


## ----your turn 1----------------------------------------------------------------------------------------------------------------
# retrieve variable of interest

# compute and store sample mean



## ----standard error for the sample mean-----------------------------------------------------------------------------------------
# store sample sd and sample size
totchol.sd <- sd(totchol)
totchol.n <- length(totchol)

# compute standard error
totchol.se <- totchol.sd/sqrt(totchol.n)
totchol.se


## ----your turn 2----------------------------------------------------------------------------------------------------------------
# store sample sd and sample size

# compute standard error



## ----interval for the population mean-------------------------------------------------------------------------------------------
# add/subtract two standard errors from the mean
totchol.mean + c(-2, 2)*totchol.se


## ----your turn 3----------------------------------------------------------------------------------------------------------------
# interval estimate for mean body temp



## ----activity exploring coverage------------------------------------------------------------------------------------------------
# function to simulate body temp data from a population with mean 98.6
sample.bodytemps <- function(n){
  rnorm(n, mean = 98.6, sd = 1)
}

# simulate a sample of body temperatures
bodytemp <- sample.bodytemps(n = 150)

# compute interval 'ingredients'
bodytemp.mean <- mean(bodytemp)
bodytemp.sd <- sd(bodytemp)
bodytemp.n <- length(bodytemp)
bodytemp.se <- bodytemp.sd/sqrt(bodytemp.n)

# compute interval estimate
bodytemp.mean + c(-2, 2)*bodytemp.se

# margin of error
2*bodytemp.se


## ----practice problem 1, echo = F, results='hide'-------------------------------------------------------------------------------
# input summary statistics directly for this problem

# part a: point estimate for population mean
bgc.mean <- 10 # replace with correct answer 

# part b: point estimate for population sd

# part d: standard error for estimate in (a)

# part e: interval for the population mean

# part f: repeat e, but suppose only 30 nests were measured



## ----practice problem 2, echo = F, results='hide'-------------------------------------------------------------------------------
# load dataset and inspect
load('data/brfss.RData')
head(brfss)

# difference between actual and desired weight
weight.diff <- brfss$weight - brfss$wtdesire

# part a: point estimate and se for mean difference

# part c: interval estimate for mean difference



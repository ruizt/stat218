## ----packages and data-------------------------------------------------------------------------------
library(tidyverse)
load('data/famuss.RData')


## ----contingency tables------------------------------------------------------------------------------
# retrieve variables of interest
genotype <- famuss$genotype
sex <- famuss$sex

# make a contingency table
table(sex, genotype)


## ----your turn 1-------------------------------------------------------------------------------------
# retrieve variables of interest

# make a contingency table



## ----converting to proportions-----------------------------------------------------------------------
# combination frequencies (using grand total as denominator)
table(sex, genotype) |> proportions(margin = NULL)

# genotype frequencies by sex (using row margins as denominators)
table(sex, genotype) |> proportions(margin = 1)

# sex frequencies by genotype (using column margins as denominators)
table(sex, genotype) |> proportions(margin = 2)


## ----your turn 2-------------------------------------------------------------------------------------
# genotype frequencies by race



## ----stacked bar plot using row totals---------------------------------------------------------------
# stacked bar plot showing genotype frequencies by sex
table(genotype, sex) |> 
  proportions(margin = 2) |> 
  barplot(legend = T)


## ----stacked bar plot using column totals------------------------------------------------------------
# stacked bar plot showing sex frequencies by genotype
table(sex, genotype) |> 
  proportions(margin = 2) |> 
  barplot(legend = T)


## ----your turn 3-------------------------------------------------------------------------------------
# stacked bar plot showing genotype frequencies by race



## ----scatterplot and correlation---------------------------------------------------------------------
# retrieve height and weight columns
height <- famuss$height
weight <- famuss$weight

# basic scatterplot
plot(height, weight)

# correlation
cor(weight, height)


## ----your turn 4-------------------------------------------------------------------------------------
# retrieve the percent change variables

# construct a scatterplot

# compute the correlation



## ----num-cat, results = 'hide'-----------------------------------------------------------------------
# side-by-side boxplots for non-dominant arm
boxplot(ndrm.ch ~ genotype, data = famuss)

# change the orientation 
boxplot(ndrm.ch ~ genotype, data = famuss, horizontal = T)


## ----your turn 5-------------------------------------------------------------------------------------
# make side-by-side boxplots of percent change in dominant arm strength by genotype


## ----practice problem 1, echo = F, results = 'hide', fig.show = 'hide'-------------------------------
# load dataset

# preview

# part a: construct a contingency table

# part b: find chd frequency by anger group

# part c: make a stacked bar plot showing chd frequency by anger group

# part d: relative risk of CHD event in high compared with low groups (do by hand)


## ----practice problem 2, echo = F, results='hide', fig.show='hide'-----------------------------------
# load data

# preview

# part a: scatterplot of systolic and diastolic blood pressure

# part b: correlation between systolic and diastolic blood pressure

# part c

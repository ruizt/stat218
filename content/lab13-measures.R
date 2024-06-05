## ----setup-------------------------------------------------------------------------------------------------
library(tidyverse)
library(epitools)
load('data/smoking.RData')
load('data/asthma.RData')
load('data/chd.RData')
load('data/outbreak.RData')


## ----contingency table with row and column names-----------------------------------------------------------
# construct table with dimension names group, smoking
smoking.tbl <- table(group = smoking$group, smoking = smoking$smoking)
smoking.tbl


## ----check chi square test assumptions---------------------------------------------------------------------
# check whether expected counts are at least ten 
chisq.test(smoking.tbl)$expected


## ----inference with odds ratio-----------------------------------------------------------------------------
oddsratio(smoking.tbl, rev = 'both', conf.level = 0.95, method = 'wald', correction = T)


## ----your turn 1-------------------------------------------------------------------------------------------
# check odds ratio calculation by hand to ensure result is as intended


## ----your turn 2-------------------------------------------------------------------------------------------
# construct contingency table

# check assumptions for chi square test

# perform inference with odds ratio

# double-check your point estimate to verify data were arranged in correct orientation



## ----table and assumption check for asthma data------------------------------------------------------------
# put groups in rows and outcome in columns
asthma.tbl <- table(sex = asthma$sex, asthma = asthma$asthma)
asthma.tbl

# check assumptions for chi square test
chisq.test(asthma.tbl)$expected


## ----inference of association with risk ratio--------------------------------------------------------------
riskratio(asthma.tbl, rev = 'both', conf.level = 0.90, method = 'wald', correction = T)


## ----doublechecking point estimate-------------------------------------------------------------------------
# compute proportions
asthma.tbl |> prop.table(margin = 1)

# risk ratio by hand
0.05903614/0.03754693


## ----your turn 3-------------------------------------------------------------------------------------------
# construct contingency table in proper orientation for inference with relative risk

# check assumptions for chi square test

# carry out inference at 1% significance level

# double check point estimate by manual calculation



## ----practice problem 1, include = F-----------------------------------------------------------------------
# load dataset
load('data/diabetes_meds.RData')

# construct contingency table

# (re-check) chi square test assumptions

# perform inference with appropriate measure of association

# (recommended) double-check to make sure you got the right estimate



## ----practice problem 2, include = F-----------------------------------------------------------------------
load('data/leap.RData')

# construct contingency table

# check assumptions for chi square test

# perform inference at 1% level using appropriate measure of association

# (recommended) double-check to make sure you got the right estimate



## ----practice problem 3, include = F-----------------------------------------------------------------------
load('data/vitamin.RData')

# construct contingency table

# check assumptions for chi square test

# test for association at the 1% level with an appropriate measure of association

# (recommended) double-check to make sure you got the right estimate



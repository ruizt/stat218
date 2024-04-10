## ----packages and data------------------------------------------------------------------------------------------------
library(tidyverse)

# load famuss dataset 
load('data/famuss.RData')

# inspect data frame
head(famuss)


## ----refresher on retrieving variables, results = 'hide'--------------------------------------------------------------
# extract the age variable
famuss$age

# store the age column as a vector
age <- famuss$age


## ----barplots---------------------------------------------------------------------------------------------------------
# retrieve genotype
genotype <- famuss$genotype

# make a table, generate a barplot
table(genotype) |> barplot()


## ----your turn 1------------------------------------------------------------------------------------------------------
# retrieve race

# make a table, generate a barplot



## ----histograms-------------------------------------------------------------------------------------------------------
# retrieve age
age <- famuss$age

# effectively, a bar plot of ages
hist(age, breaks = 25)

# fewer bins
hist(age, breaks = 10)


## ----your turn 2------------------------------------------------------------------------------------------------------
# retrieve dominant arm percent change

# make a histogram; find a binning that captures the shape well



## ----location measures------------------------------------------------------------------------------------------------
# average age
mean(age)

# median age (middle value)
median(age)

# 25th percentile of age ("quantile" is another term for percentile)
quantile(age, probs = 0.25)

# 25th *and* 75th percentile of age
quantile(age, probs = c(0.25, 0.75))

# minimum age
min(age)

# maximum age
max(age)


## ----five number summary plus mean------------------------------------------------------------------------------------
# all common location measures
summary(age)


## ----your turn 3------------------------------------------------------------------------------------------------------
# compute the five-number summary for change in dominant arm strength



## ----measures of spread-----------------------------------------------------------------------------------------------
# age range
range(age)

# interquartile range of ages (width of interval containing middle 50% of data)
IQR(age)

# variance of age
var(age)

# standard deviation of age
sd(age)


## ----your turn 4------------------------------------------------------------------------------------------------------
# standard deviation of percent change in dominant arm strength



## ----original mean and median age-------------------------------------------------------------------------------------
# average age
mean(age)

# median age
median(age)

# range
range(age)


## ----mean and median age with artificial outliers included------------------------------------------------------------
# average age
mean(c(age, 96, 92, 87, 91))

# median age
median(c(age, 96, 92, 87, 91))


## ----original iqr and variance----------------------------------------------------------------------------------------
# variance of ages
var(age)

# interquartile range of ages
IQR(age)


## ----iqr and variance with artificial outliers included---------------------------------------------------------------
# age variance
var(c(age, 96, 92, 87, 91))

# age iqr
IQR(c(age, 96, 92, 87, 91))


## ----grouped summaries------------------------------------------------------------------------------------------------
# average dominant arm change by genotype
famuss |>
  group_by(genotype) |>
  summarize(avg.drm.ch = mean(drm.ch))


## ----multiple grouped summaries---------------------------------------------------------------------------------------
# average dominant and nondominant arm change by genotype
famuss |>
  group_by(genotype) |>
  summarize(avg.drm.ch = mean(drm.ch),
            avg.ndrm.ch = mean(ndrm.ch))


## ----your turn 5------------------------------------------------------------------------------------------------------
# median percent change in dominant arm strength by genotype


## ----practice problem 1, eval = F, echo = F---------------------------------------------------------------------------
# load census dataset

# inspect data frame

# part a: histogram of total personal income

# part b: measures of location for total personal income

# part c: iqr and sd; which is better?

# part d: median total personal income by sex


## ----practice problem 2, eval = F, echo = F---------------------------------------------------------------------------
# load frog data

# inspect

# part a: number of samples at each site

# part b: frequency distribution of altitude (table)

# part c: frequency distribution of altitude (barplot)

# part d: histogram of clutch volume

# part e: measures of location

# part f: standard deviation, iqr, variance

# part g: average clutch volume by altitude (grouped summary)

# part h (optional): average absolute deviation of clutch volumes



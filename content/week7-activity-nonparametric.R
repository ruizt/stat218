## ----setup-----------------------------------------------------------------------------
library(tidyverse)
load('data/brfss.RData')
load('data/temps.RData')
ddt <- MASS::DDT


## ----motivation 1: small sample sizes--------------------------------------------------
heart.m <- temps |> filter(sex == 'male') |> pull(heart.rate)
heart.f <- temps |> filter(sex == 'female') |> pull(heart.rate)

par(mfrow = c(1, 2))
hist(heart.m)
hist(heart.f)


## ----motivation 2: assumptions fail----------------------------------------------------
set.seed(51424)
weight.diff <- sample(brfss$weight - brfss$wtdesire, 12)
hist(weight.diff, breaks = 10)


## ----signed rank test: warm up---------------------------------------------------------
sort(ddt)


## ----signed rank test: warmup solution-------------------------------------------------
# number of observations below 3
sum(ddt < 3)


## ----signed rank test procedure: check your understanding------------------------------
# calculate deviations
di <- ddt - 3
sort(di)


## ----signed rank test procedure: check your understanding solution---------------------
# this shows the steps, column by column; focus on output
ddt.srank <- tibble(di = di) |>
  mutate(abs.di = abs(di), 
         rank = rank(abs.di),
         sign = sign(di),
         signed.rank = sign*rank) |>
  arrange(abs.di)
ddt.srank

# signed rank statistic
vstat <- ddt.srank |> filter(sign > 0) |> pull(signed.rank) |> sum()
vstat


## ----signed rank test: implementation--------------------------------------------------
# signed rank test at 1% level
wilcox.test(ddt, mu = 3, alternative = 'two.sided', 
            exact = F, conf.int = T, conf.level = 0.99)


## ----your turn 1, include = F----------------------------------------------------------
# perform t test at 1% level to compare


## ----your turn 2, include = F----------------------------------------------------------
# small sample of weight differences
set.seed(51424)
weight.diff <- sample(brfss$weight - brfss$wtdesire, 12)

# assumptions don't hold
hist(weight.diff, breaks = 10)

# use signed rank test to determine whether actual exceeds desired by at least 10lbs at 5% significance level

# check t test result to compare


## ----fish oil data, fig.width = 4, fig.height = 3--------------------------------------
# load dataset
fish.oil <- Sleuth3::ex0112 |> rename_with(tolower)

# make boxplot
boxplot(bp ~ diet, data = fish.oil, horizontal = T)


## ----rank sum test: check your understanding-------------------------------------------
fish.oil |> arrange(bp)


## ----rank sum test: check your understanding solution----------------------------------
# again, ignore the codes; look at output
fish.oil.ranksum <- fish.oil |> 
  mutate(rank = rank(bp),
         ranks.fish = rank*(diet == 'FishOil')) |>
  arrange(bp)
fish.oil.ranksum

# rank sum statistic
n1 <- count(fish.oil, diet)$n[1]
sum(fish.oil.ranksum$ranks.fish) - n1*(n1 + 1)/2


## ----echo=T----------------------------------------------------------------------------
wilcox.test(bp ~ diet, data = fish.oil, 
            mu = 0, alternative = 'greater', 
            exact = F, conf.int = T, conf.level = 0.99)


## ----your turn 3, include = F----------------------------------------------------------
# t test for effect of treatment at 1% level


## ----your turn 4, include = F----------------------------------------------------------
# load data
cancer <- read_csv('data/cancer.csv')

# test whether change in cancer rate is higher in years with high sunspot activity at the 5% level

## ----practice problem, include = F-----------------------------------------------------
# read in data and preview
cholesterol <- read_csv('data/cholesterol.csv') |> rename_with(tolower)
head(cholesterol)

# part a: make a boxplot and assess whether the nonparametric test is appropriate

# part b: carry out the test

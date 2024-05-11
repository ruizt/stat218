## ----packages and data--------------------------------------------------------------------------------------------------
library(tidyverse)
load('data/finch.RData')
load('data/temps2.RData')


## ----summary stats for finch data---------------------------------------------------------------------------------------
finch |>
  group_by(year) |>
  summarize(depth.mean = mean(depth),
            depth.sd = sd(depth),
            n = n())


## ----checking assumptions option A, fig.width = 4, fig.height = 3-------------------------------------------------------
# separate samples
finch.1978 <- finch |> filter(year == 1978)
finch.1976 <- finch |> filter(year == 1976)

# extract depths
depth.1978 <- finch.1978$depth
depth.1976 <- finch.1976$depth

# make histograms
hist(depth.1978)
hist(depth.1976)


## ----your turn 1--------------------------------------------------------------------------------------------------------
# separate samples

# extract heart rates

# make histograms



## ----checking assumptions option B, fig.width = 4, fig.height = 3-------------------------------------------------------
# side-by-side boxplots
boxplot(depth ~ year, data = finch, horizontal = T)


## ----your turn 2--------------------------------------------------------------------------------------------------------
# side-by-side boxplots for heart rate


## ----two sample t test--------------------------------------------------------------------------------------------------
# examine data to check which sample comes first; this will be group 1
head(finch, 2)

# perform t test
t.test(formula = depth ~ year, data = finch, mu = 0, alternative = 'less', conf.level = 0.95)


## ----retrieving estimate and se for difference in means-----------------------------------------------------------------
# store t test result
tt.rslt <- t.test(formula = depth ~ year, data = finch, mu = 0, alternative = 'less', conf.level = 0.95)

# estimates
tt.rslt$estimate

# estimate for difference in means
tt.rslt$estimate |> diff()

# standard error for estimate of difference in means
tt.rslt$stderr


## ----your turn 3--------------------------------------------------------------------------------------------------------
# examine data to determine which sample comes first

# perform t test

# store t test result

# estimate for difference in means

# standard error



## ----practice problem 1, echo = F---------------------------------------------------------------------------------------
# check assumptions via histograms

# ... and via boxplots

# examine data to determine which sample comes first

# perform t test

# store t test result

# estimate for difference in means

# standard error



## ----echo = F-----------------------------------------------------------------------------------------------------------
load('data/brfss.RData')
brfss |> 
  transmute(sex = gender, 
            weight = weight,
            wtdesire = wtdesire,
            weight.diff = weight - wtdesire) |>
write_csv(file = 'data/brfss2.csv')


## ----practice problem 2, echo = F---------------------------------------------------------------------------------------
# load data

# inspect

# check assumptions via histograms

# ... and via boxplots

# examine data to determine which sample comes first

# perform t test

# store t test result

# estimate for difference in means

# standard error



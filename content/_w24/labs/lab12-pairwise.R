## ----packages and data--------------------------------------------------------------------
# load packages
library(tidyverse)
library(emmeans)
library(pander)

# read in dataset and make variable names lowercase
longevity <- read.csv('data/longevity.csv') |> 
  rename_with(tolower)

# convert grouping variable to a factor
longevity$diet <- factor(longevity$diet, 
                         levels = c('NP', 'N/N85', 
                                    'N/R50', 'N/R40'),
                         ordered = T)

# preview
head(longevity) |> pander()


## ----basic anova, fig.width = 4, fig.height = 5-------------------------------------------
# boxplot for visual group comparisons
boxplot(lifetime ~ diet, data = longevity)

# grouped summaries
longevity |>
  group_by(diet) |> 
  summarize(mean = mean(lifetime),
            sd = sd(lifetime),
            n = n(),
            mean.se = sd/sqrt(n)) |>
  pander()

# fit analysis of variance model
fit <- aov(lifetime ~ diet, data = longevity)
summary(fit)


## ----intervals for group means------------------------------------------------------------
# intervals for group means
emmeans(fit, ~ diet) |> 
  confint(level = 0.95) 

# equivalent to...
fit.emm <- emmeans(fit, ~ diet)
confint(fit.emm, level = 0.95)

## ----visualize estimates, fig.width = 4, fig.height = 3-----------------------------------
# basic plotting method
emmeans(fit, ~ diet) |> 
  plot(level = 0.95, xlab = 'estimated mean lifetime')

# another option
emmip(fit, ~ diet, CIs = T, level = 0.95, ylab = 'estimated mean lifetime')


## ----your turn 1, eval = F----------------------------------------------------------------
# intervals for group means
emmeans(fit, ~ diet) |>
  confint(level = 1 - 0.05/4) # change here

emmeans(fit, ~ diet) |>
  confint(level = 1 - 0.05, adjust = 'bonferroni') # change here

# plot
emmeans(fit, ~ diet) |>
  plot(level = 1 - 0.05/4, # change here
       xlab = 'estimated mean lifetime')


## ----tests and intervals for group means--------------------------------------------------
# test whether lifetimes exceed 30 days
emmeans(fit, ~ diet) |>
  test(null = 30, side = '>') 

# one-sided intervals
emmeans(fit, ~ diet) |>
  confint(level = 0.95, side = '>')


## ----adding bonferroni correction, eval = F-----------------------------------------------
# test whether lifetimes exceed 30 days
emmeans(fit, ~ diet) |>
  test(null = 30, side = '>', adjust = 'bonferroni')

# one-sided intervals
emmeans(fit, ~ diet) |>
  confint(level = 0.95, side = '>', adjust = 'bonferroni')


## ----your turn 2, eval = F----------------------------------------------------------------
# modify this code to test whether mean lifetime is less than 40
emmeans(fit, ~ diet) |>
  test(null = 40, side = '<', adjust = 'bonferroni')


## ----pairwise comparisons-----------------------------------------------------------------
# pairwise comparisons: tests
emmeans(fit, ~ diet) |> 
  pairs() |>
  test() 

# pairwise comparisons: intervals
emmeans(fit, ~ diet) |> 
  pairs() |>
  confint() 

# plotting method
emmeans(fit, ~ diet) |>
  pairs() |>
  plot()


## ----your turn 3, eval = F----------------------------------------------------------------
# try changing the multiple inference adjustment
emmeans(fit, ~ diet) |>
  pairs() |>
  test(adjust = 'holm')


## ----comparisons with a control-----------------------------------------------------------
# generate contrasts that compare with control
emmeans(fit, ~ diet)
emmeans(fit, trt.vs.ctrl ~ diet)
emmeans(fit, pairwise ~ diet)


## ----your turn 4, eval = F----------------------------------------------------------------
# modify the default to test whether levels increase mean lifespan by at least 10 days
emmeans(fit, trt.vs.ctrl ~ diet)$contrast |>
  test(null = 10, side = '>') # modify here


## ----practice problem, eval = F-----------------------------------------------------------
# read in data
mussels <- read.csv('data/mussels.csv')
head(mussels)

# visualization via boxplots

# grouped summaries, check SD's

# anova fit and table
fit <- aov(aam.length ~ location, data = mussels)

# point and interval estimates of group means, with bonferroni correction

# visualization of estimates

# pairwise comparisons

# visualization
emmeans(fit, ~ location) |>
  pairs() |>
  plot() +
  geom_vline(xintercept = 0, color = 'black')


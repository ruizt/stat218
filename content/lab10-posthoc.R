## ----setup-------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(emmeans)
load('data/longevity.RData')
load('data/anorexia.RData')


## ----fitting an anova model--------------------------------------------------------------------------------------------------------
# fit the model
fit.longevity <- aov(lifetime ~ diet, data = longevity)

# generate the ANOVA table
summary(fit.longevity)


## ----your turn 1, echo = F---------------------------------------------------------------------------------------------------------
# inspect data
str(anorexia)

# fit the model

# generate the ANOVA table



## ----basic emmeans usage-----------------------------------------------------------------------------------------------------------
# default behavior is to produce unadjusted 95% confidence intervals for group means
emmeans(object = fit.longevity, specs = ~ diet)


## ----interval estimates for group means--------------------------------------------------------------------------------------------
# simultaneous 95% confidence intervals for group means with bonferroni adjustment (correct)
emmeans(object = fit.longevity, specs = ~ diet) |>
  confint(level = 0.95, adjust = 'bonferroni')


## ----your turn 2, echo = F---------------------------------------------------------------------------------------------------------
# simultaneous 99% confidence intervals for group means with bonferroni adjustment



## ----plotting intervals for group means, fig.width = 4, fig.height = 3-------------------------------------------------------------
# plot via: emmeans(...) |> confint(...) |> plot(...)
emmeans(object = fit.longevity, specs = ~ diet) |> 
  confint(level = 0.95, adjust = 'bonferroni') |> 
  plot(xlab = 'mean lifetime (months)', ylab = 'diet')


## ----your turn 3, echo = F---------------------------------------------------------------------------------------------------------
# plot your simultaneous 99% confidence intervals for group means with bonferroni adjustment


## ----pairwise contrasts------------------------------------------------------------------------------------------------------------
# test for pairwise differences at the 5% significance level
emmeans(object = fit.longevity, specs = ~ diet) |>
  contrast('pairwise') |>
  test(adjust = 'bonferroni')

# simultaneous 95% intervals for all pairwise contrasts
emmeans(object = fit.longevity, specs = ~ diet) |>
  contrast('pairwise') |>
  confint(level = 0.95, adjust = 'bonferroni')


## ----your turn 4, echo = F---------------------------------------------------------------------------------------------------------
# simultaneous 90% intervals for all pairwise contrasts

# test for pairwise differences at the 10% significance level



## ----contrasts with a control------------------------------------------------------------------------------------------------------
# tests for contrasts with a control group
emmeans(object = fit.longevity, specs = ~ diet) |>
  contrast('trt.vs.ctrl') |>
  test(adjust = 'dunnett')

# simultaneous 95% intervals for contrasts with a control group
emmeans(object = fit.longevity, specs = ~ diet) |>
  contrast('trt.vs.ctrl') |>
  confint(level = 0.95, adjust = 'dunnett')


## ----your turn 5-------------------------------------------------------------------------------------------------------------------
# test for differences relative to control at 1% level

# estimate differences at the appropriate confidence level



## ----testing for a minimum difference----------------------------------------------------------------------------------------------
# test whether mean lifetime exceeds control by more than 1 year
emmeans(object = fit.longevity, specs = ~ diet) |>
  contrast('trt.vs.ctrl') |>
  test(null = 12, side = '>')

# interval estimate
emmeans(object = fit.longevity, specs = ~ diet) |>
  contrast('trt.vs.ctrl') |>
  confint(level = 0.95, side = '>')


## ----practice problem 1, include = F-----------------------------------------------------------------------------------------------
# load and inspect dataset

# part a: fit anova model and construct table

# part b: estimate mean aam length for each population

# part c: test for pairwise differences at 1% level

# part d: intervals at the corresponding level


## ----practice problem 2, include = F-----------------------------------------------------------------------------------------------
# load and inspect data

# fit anova model

# test for contrasts with control


## ----extra credit, include = F-----------------------------------------------------------------------------------------------------
# load longevity data

# fit anova model to log lifetimes

# estimate contrasts with control

# back-transform interval estimates



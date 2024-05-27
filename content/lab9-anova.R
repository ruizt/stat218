## ----setup-------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
load('data/chicks-20d.RData')
anorexia <- read_csv('data/anorexia.csv')


## ----descriptive analysis prior to ANOVA-------------------------------------------------------------------------------------------
# groupwise summaries: mean, sd, and n
chicks |>
  group_by(diet) |>
  summarize(mean = mean(weight),
            sd = sd(weight),
            n = n(),
            se = sd(weight)/sqrt(n()),
            se2 = sd/sqrt(n))

# boxplots by group
boxplot(weight ~ diet, data = chicks)


## ----your turn 1-------------------------------------------------------------------------------------------------------------------
# groupwise summaries: mean, sd, and n

# boxplots by group



## ----fitting anova model-----------------------------------------------------------------------------------------------------------
# fit anova model
fit <- aov(formula = weight ~ diet, data = chicks)
fit

# construct anova table
summary.aov(fit)


## ----your turn 2-------------------------------------------------------------------------------------------------------------------
# fit anova model
fit.anx <- aov(change ~ treat, data = anorexia)
fit.anx

# construct anova table
summary(fit.anx)


## ----f stat and p value by hand----------------------------------------------------------------------------------------------------
# view ss and df
fit

# run but ignore -- this extracts sums of squares and degrees of freedom
fit.ss <- summary.aov(fit)[[1]]$`Sum Sq`
fit.df <- summary.aov(fit)[[1]]$Df
# names(fit.df) <- names(fit.ss) <- c('treatment', 'error')

# mean squares
fit.ms <- fit.ss/fit.df
fit.ms

# f statistic
# fstat <- fit.ms['treatment']/fit.ms['error']
fstat <- fit.ms[1]/fit.ms[2]
fstat

# p value
pf(fstat, df1 = fit.df['treatment'], df2 = fit.df['error'], lower.tail = F)


## ----your turn 3-------------------------------------------------------------------------------------------------------------------
# plug in the sums of squares and degrees of freedom from your fitted model above
fit.ss <- c(614.644, 3910.742)
fit.df <- c(2, 69) # REPLACE

# mean squares
fit.ms <- fit.ss/fit.df
fit.ms

# f statistic
fstat <- fit.ms[1]/fit.ms[2]
fstat

# p value
pf(fstat, df1 = 2, df2 = 69, lower.tail = F)


## ----practice problem 1, include = F-----------------------------------------------------------------------------------------------
# read in data and preview

# part a: boxplot and summary statistics; check assumptions

# part b: point estimates and standard errors

# part c: fit anova model and produce table


## ----practice problem 2, include = F-----------------------------------------------------------------------------------------------
# plug in ss and df

# mean squares

# f statistic

# p value

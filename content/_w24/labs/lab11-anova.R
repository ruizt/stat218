## ----data and packages, results = 'hide'---------------------------------------------------------------------------------------------------------
library(tidyverse)
anorexia <- read_csv('data/anorexia.csv')
head(anorexia)


## ----step1: graphical summary--------------------------------------------------------------------------------------------------------------------
# boxplot
boxplot(change ~ Treat, data = anorexia,
        xlab = 'treatment group', ylab = 'weight change (lbs)')


## ----your turn 1, eval = F-----------------------------------------------------------------------------------------------------------------------
# rearrange order of treatment groups
boxplot(change ~ factor(Treat, levels = c('Cont', 'CBT', 'FT')), data = anorexia,
        xlab = 'treatment group', ylab = 'weight change (lbs)')


## ----step 2: grouped summaries-------------------------------------------------------------------------------------------------------------------
# grouped summaries: mean, standard deviation, sample size
anorexia |>
  group_by(Treat) |>
  summarize(group.mean = mean(change),
            group.sd = sd(change),
            group.n = n())


## ----your turn 2, eval = F-----------------------------------------------------------------------------------------------------------------------
anorexia |>
  group_by(Treat) |>
  summarize(group.mean = mean(change),
            group.sd = sd(change),
            group.n = n(),
            group.median = median(change),
            group.mean.se = sd(change)/sqrt(n())) # add a summary here


## ----step 3: fitting anova model-----------------------------------------------------------------------------------------------------------------
# fit anova model
fit <- aov(change ~ Treat, data = anorexia)

# generate table
summary(fit)


## ----your turn 3---------------------------------------------------------------------------------------------------------------------------------
# inspect fitted anova model
fit


## ----practice problem----------------------------------------------------------------------------------------------------------------------------
# read in data and preview
longevity <- read.csv('data/longevity.csv')
head(longevity)

# visualize the data -- make a boxplot
boxplot(Lifetime ~ factor(Diet, levels = c("NP", "N/N85", "N/R50", "N/R40")), 
        data = longevity)

# calculate grouped summaries
longevity |>
  group_by(Diet) |>
  summarize(group.mean = mean(Lifetime),
            group.sd = sd(Lifetime),
            group.n = n()) |>
  arrange(group.mean)

# fit anova model and produce table
fit <- aov(Lifetime ~ Diet, data = longevity)
summary(fit)

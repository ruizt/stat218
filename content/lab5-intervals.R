## ----echo = F-------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(openintro)
births |> filter(smoke == 'nonsmoker') |>
  rename(mother.age = m_age,
         birth.weight = weight,
         sex = sex_baby) |>
  select(mother.age, weeks, birth.weight, sex) |>
  write_csv(file = 'data/ncbirths.csv')


## ----data-----------------------------------------------------------------------------------------------------------------------
# read in data and preview
ncbirths <- read_csv('data/ncbirths.csv')
head(ncbirths)


## ----95% confidence interval using empirical rule-------------------------------------------------------------------------------
# retrieve variable of interest
bweight <- ncbirths$birth.weight

# interval ingredients
bweight.mean <- mean(bweight)
bweight.sd <- sd(bweight)
bweight.n <- length(bweight)

# standard error
bweight.se <- bweight.sd/sqrt(bweight.n)

# 95% interval using empirical rule
bweight.mean + c(-2, 2)*bweight.se


## ----68% interval using empireical rule-----------------------------------------------------------------------------------------
# 68% interval using empirical rule
bweight.mean + c(-1, 1)*bweight.se


## ----your turn 1----------------------------------------------------------------------------------------------------------------
# retrieve variable of interest (no. weeks at birth)

# interval ingredients

# standard error

# 99.7% interval for mean number of weeks at birth using empirical rule



## ----function to calculate interval---------------------------------------------------------------------------------------------
# run this before continuing, but ignore unless interested
make_ci <- function(vec, cval){
  vec.mean <- mean(vec)
  vec.mean.se <- sd(vec)/sqrt(length(vec))
  interval <- vec.mean + c(-1, 1)*cval*vec.mean.se 
  names(interval) <- c('lwr', 'upr')
  return(interval)
}


## ----computing intervals using function-----------------------------------------------------------------------------------------
# 95% interval
make_ci(bweight, cval = 2)

# 68% interval
make_ci(bweight, cval = 1)


## ----your turn 2----------------------------------------------------------------------------------------------------------------
# 99.7% interval for mean number of weeks at birth, using make_ci(...)


# 95% interval for mean number of weeks at birth, using make_ci(...)



## ----your turn 3, echo = F------------------------------------------------------------------------------------------------------
# no R computations, but see prompts for a quick exercise


## ----calculating t quantiles----------------------------------------------------------------------------------------------------
# for 80% interval from n = 15 observations, use this quantile
qt(p = 0.9, df = 14)

# for a 92% interval from n = 30 observations, use this quantile
qt(p = 0.96, df = 29)


## ----your turn 4----------------------------------------------------------------------------------------------------------------
# quantile for a 90% interval from 27 observations
qt(p = 0.95, df = 26)

# quantile for a 95% interval from 18 observations

# quantile for a 99% interval from 51 observations



## ----example intervals using t quantile-----------------------------------------------------------------------------------------
# 98% ci for mean birth weight
crit.val <- qt(p = 0.99, df = 99)
make_ci(bweight, cval = crit.val)

# 95% ci for mean birth weight
crit.val <- qt(p = 0.975, df = 99)
make_ci(bweight, cval = crit.val)

# 90% ci for mean birth weight
crit.val <- qt(p = 0.95, df = 99)
make_ci(bweight, cval = crit.val)


## ----your turn 5----------------------------------------------------------------------------------------------------------------
# 99% ci for mean number of weeks at birth
bweeks <- ncbirths$weeks
crit.val <- qt(p = 0.995, df = 99)
make_ci(bweeks, cval = crit.val)

## ----your turn 6----------------------------------------------------------------------------------------------------------------
## repeat last calculation but fully 'by hand'

# point estimate and standard error
bweeks.mean <- mean(bweeks)
bweeks.mean.se <- sd(bweeks)/sqrt(100)

# critical value
crit.val <- qt(p = 0.995, df = 99)

# interval
bweeks.mean + c(-crit.val, crit.val)*bweeks.mean.se
bweeks.mean + c(-1, 1)*crit.val*bweeks.mean.se

# bweight.mean + c(-2, 2)*bweight.se


## ----determining coverage in reverse--------------------------------------------------------------------------------------------
# example interval for mean birth weight
bweight.ci <- c(7.030077, 7.328923)

# margin of error (half the width)
bweight.ci.me <- diff(bweight.ci)/2

# divide out standard error to get critical value
crit.val <- bweight.ci.me/bweight.se

# coverage
pt(q = crit.val, df = bweight.n - 1)

## ----your turn 7----------------------------------------------------------------------------------------------------------------
# interval for mean number of weeks at birth
bweeks.ci <- c(37.79485, 39.30515)

# margin of error (half the width)

# divide out standard error to get critical value

# coverage



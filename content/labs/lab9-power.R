## ----load datasets----------------------------------------------------------------------------------------------------------
library(Sleuth3)
library(tidyverse)
cloud <- case0301
load('data/temps.RData')


## ----eval = F---------------------------------------------------------------------------------------------------------------
## type2sim <- function(delta, n, sd, alpha, nsim = 1000){
##   # simulate nsim tests by...
##   sim.pvals <- sapply(1:nsim, function(i){
##     # draw sample with true group difference of delta
##     samp <- data.frame(variable = c(rnorm(n, mean = 0, sd = sd), rnorm(n, mean = delta, sd = sd)),
##                        group = rep(1:2, each = n))
##     # perform test and compute p value
##     pval <- t.test(variable ~ group, data = samp, mu = 0, alternative = 'two.sided')$p.value
##     return(pval)
##   })
##   # compute proportion of tests that failed to reject (made a type ii error)
##   err.rate <- mean(sim.pvals > alpha)
##   return(err.rate)
## }
## 
## save(type2sim, file = 'type2sim.RData')


## ---------------------------------------------------------------------------------------------------------------------------
# two-sided test
cloud.test.out <- t.test(Rainfall ~ Treatment, data = cloud, mu = 0, alternative = 'two.sided')

# estimated difference in means
diff(cloud.test.out$estimate)

# population SDs (don't worry about syntax here)
cloud |> group_by(Treatment) |> summarize(sd(Rainfall))


## ----simulate type 2 errors-------------------------------------------------------------------------------------------------
# load simulation function
load('type2sim.RData')

# simulation-based estimate of type 2 error
type2sim(delta = 277, n = 26, sd = 650, alpha = 0.05)


## ----your turn 1, eval = F--------------------------------------------------------------------------------------------------
## # store 'baseline' type ii error rate
## estimated.error <- type2sim(delta = 277, n = 26, sd = 650, alpha = 0.05)
## 
## # increase delta
## type2sim(delta = ..., n = 26, sd = 650, alpha = 0.05) - estimated.error
## 
## # decrease delta
## type2sim(delta = ..., n = 26, sd = 650, alpha = 0.05) - estimated.error
## 
## # increase sd
## type2sim(delta = 277, n = 26, sd = ..., alpha = 0.05) - estimated.error
## 
## # decrease sd
## type2sim(delta = 277, n = 26, sd = ..., alpha = 0.05) - estimated.error
## 
## # increase sample size
## type2sim(delta = 277, n = ..., sd = 650, alpha = 0.05) - estimated.error
## 
## # decrease sample size
## type2sim(delta = 277, n = ..., sd = 650, alpha = 0.05) - estimated.error
## 
## # increase significance level
## type2sim(delta = 277, n = 26, sd = 650, alpha = ...) - estimated.error
## 
## # decrease significance level
## type2sim(delta = 277, n = 26, sd = 650, alpha = ...) - estimated.error


## ----theoretical power calculation, results = 'hide'------------------------------------------------------------------------
# minimal arguments
power.t.test(delta = 277, n = 26, sd = 650)

# verbose
power.t.test(delta = 277, n = 26, sd = 650, sig.level = 0.05, type = 'two.sample', alternative = 'two.sided')


## ----power curve example----------------------------------------------------------------------------------------------------
# plot power curve for cloud seeding test 
curve(power.t.test(delta = x, n = 26, sd = 650)$power, 
      from = 0, to = 1000, xlab = 'true difference', ylab = 'power')


## ----your turn 2------------------------------------------------------------------------------------------------------------
# plot power curve for cloud seeding test 
curve(power.t.test(delta = x, n = 26, sd = 650, sig.level = 0.05)$power, 
      from = 0, to = 1000, xlab = 'true difference', ylab = 'power')


## ----body temp test---------------------------------------------------------------------------------------------------------
# summary stats (don't worry about syntax here)
temps |> group_by(sex) |> summarize(mean = mean(body.temp), sd = sd(body.temp))

# two-sided test
t.test(body.temp ~ sex, data = temps, mu = 0, alternative = 'two.sided')


## ----your turn 3, eval = F--------------------------------------------------------------------------------------------------
## # power with smaller sd
## power.t.test(n = ..., delta = ..., sd = ..., sig.level = ..., alternative = ...)
## 
## # power with larger sd
## power.t.test(n = ..., delta = ..., sd = ..., sig.level = ..., alternative = ...)
## 
## # power for one-sided test
## power.t.test(n = ..., delta = ..., sd = ..., sig.level = ..., alternative = ...)
## 


## ---------------------------------------------------------------------------------------------------------------------------
power.t.test(power = 0.9, delta = 250, sd = 650, sig.level = 0.05, alternative = 'two.sided')


## ----your turn 4, eval = F--------------------------------------------------------------------------------------------------
## # 1. two sided test, population sd = 1
## power.t.test(power = ..., delta = ..., sd = ..., sig.level = ..., alternative = ...)
## 
## # 2. two sided test, population sd = 1.2
## power.t.test(power = ..., delta = ..., sd = ..., sig.level = ..., alternative = ...)
## 
## # 3. one sided test, population sd = 1
## power.t.test(power = ..., delta = ..., sd = ..., sig.level = ..., alternative = ...)
## 
## # 4. one sided test, population sd = 1.2
## power.t.test(power = ..., delta = ..., sd = ..., sig.level = ..., alternative = ...)


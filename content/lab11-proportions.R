## ----setup------------------------------------------------------------------
library(tidyverse)
load('data/vitamin.RData')
load('data/nhanes500.RData')
load('data/obesity.RData')


## ----frequency distribution refresher---------------------------------------
# extract variable of interest
dia <- nhanes$diabetes

# construct table of counts (frequency distribution)
table(dia)

# render as proportions (still frequency distribution, but normalized)
table(dia) |> prop.table()

# barplot
table(dia) |> prop.table() |> barplot()


## ----your turn 1, echo = F--------------------------------------------------
# extract variable of interest

# construct table of counts

# render as proportions

# barplot



## ----point estimate and standard error--------------------------------------
# point estimates *are* sample proportions in the frequency distribution table
dia.p <- table(dia) |> prop.table()

# point estimate (sample proportion of interest)
dia.p.hat <- dia.p[1]
dia.p.hat

# standard error
dia.n <- length(dia)
dia.p.hat.se <- sqrt(prod(dia.p)/dia.n)
dia.p.hat.se


## ----your turn 2, echo = F--------------------------------------------------
# store frequency distribution table (proportions)

# point estimate (sample proportion of interest)

# standard error



## ----confidence intervals---------------------------------------------------
# 90% interval
cval <- qnorm(1 - 0.10/2)
dia.p.hat + c(-1, 1)*cval*dia.p.hat.se

# 95% interval
cval <- qnorm(1 - 0.05/2)
dia.p.hat + c(-1, 1)*cval*dia.p.hat.se

# 99% interval
cval <- qnorm(1 - 0.01/2)
dia.p.hat + c(-1, 1)*cval*dia.p.hat.se

# 99.9% interval
cval <- qnorm(1 - 0.001/2)
dia.p.hat + c(-1, 1)*cval*dia.p.hat.se


## ----your turn 3, echo = F--------------------------------------------------
# 98% interval for the proportion of us adults who are physically active



## ----two sided test by hand-------------------------------------------------
# test stat
dia.z <- (dia.p.hat - 0.1)/sqrt(0.1*0.9/dia.n)

# two sided p value
dia.pval <- 2*pnorm(abs(dia.z), lower.tail = F)
dia.pval


## ----your turn 4------------------------------------------------------------
# test stat

# two sided p value



## ----using proptest for inference-------------------------------------------
# pass table (counts) to prop.test
table(dia) |> 
  prop.test(p = 0.1, alternative = 'two.sided', 
            conf.level = 0.95, correct = F)


## ----directional inference--------------------------------------------------
# upper sided test/interval: does p exceed 0.09?
table(dia) |> 
  prop.test(p = 0.09, alternative = 'greater', 
            conf.level = 0.95, correct = F)

# upper sided test/interval: is p under 0.14?
table(dia) |> 
  prop.test(p = 0.14, alternative = 'less', 
            conf.level = 0.95, correct = F)


## ----your turn 5------------------------------------------------------------
# test whether the proportion with sleep trouble is under 0.3



## ----contingency table------------------------------------------------------
# variables of interest
trt <- vitamin$treatment
out <- vitamin$outcome

# construct contingency table
vitamin.tbl <- table(trt, out)
vitamin.tbl


## ----your turn 6------------------------------------------------------------
# variables of interest

# contingency table



## ---------------------------------------------------------------------------
# test for difference in proportions
prop.test(vitamin.tbl, 
          alternative = 'two.sided', 
          conf.level = 0.95)


## ----your turn 7------------------------------------------------------------



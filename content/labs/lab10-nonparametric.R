## ----load example data--------------------------------------------------------------------------
# load example datasets
ddt <- MASS::DDT
sleep <- read.csv('data/sleep.csv')
load('data/cancer.RData')


## ----checking assumptions-----------------------------------------------------------------------
# for one-sample inference, check histogram for symmetry
hist(ddt, breaks = 5)

# for paired inference, check histogram *of differences* for symmetry
hist(sleep$diff, breaks = 10)

# for two-sample inference, check groupwise histograms for similar shape
par(mfrow = c(1, 2))
hist(cancer$delta[cancer$sunspot.activity == 'High'], main = 'high activity', xlab = 'delta')
hist(cancer$delta[cancer$sunspot.activity == 'Low'], main = 'low activity', xlab = 'delta')


## ----rank procedure implementations-------------------------------------------------------------
# signed rank test
wilcox.test(ddt, mu = 3, alternative = 'greater')

# signed rank test, paired differences
wilcox.test(sleep$diff, mu = 0, alternative = 'less')

# rank sum test
wilcox.test(delta ~ sunspot.activity, data = cancer, alternative = 'greater')


## ----permutation test implementation------------------------------------------------------------
# permutation test
library(perm)
permTS(delta ~ sunspot.activity, data = cancer, alternative = 'greater')


## ----your turn 1--------------------------------------------------------------------------------
# use rank procedure to test whether difference exceeds 1 hour



## ----cholesterol data---------------------------------------------------------------------------
# load cholesterol data
cholesterol <- read.csv('data/cholesterol.csv')
head(cholesterol, 4)


## ----your turn 2--------------------------------------------------------------------------------
# check assumptions

# determine and perform appropriate test



## ----zinc data----------------------------------------------------------------------------------
# load cholesterol data
zinc <- Sleuth3::ex0125
head(zinc, 3)


## ----your turn 3--------------------------------------------------------------------------------
# check assumptions

# determine and perform appropriate test



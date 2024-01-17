## ----load package and data----------------------------------------------------------------
# load openintro biostat package
library(oibiostat)

# load famuss data
data(famuss)

# open data documentation
?famuss


## ----view data----------------------------------------------------------------------------
head(famuss)


## ----using dataframes, results = 'hide'---------------------------------------------------
# extract the age variable
famuss$age

# extract the bmi variable
famuss$bmi

# store the age column as a vector
age <- famuss$age


## ----your turn 0--------------------------------------------------------------------------
# try it yourself: pick a variable to extract and store



## ----easy graphs and tables, results='hide', fig.show='hide'------------------------------
## categorical summaries
race <- famuss$race

# frequency distribution
table(race)

# barplot of frequency distribution
plot(race)

## quantitative summaries
age <- famuss$age

# histogram; 'breaks = ...' controls the binning
hist(age, breaks = 20)


## ----your turn 1--------------------------------------------------------------------------
# make a table of the frequency distribution of genotypes

# make a barplot of the frequency distribution of genotypes

# make a histogram of dominant arm change; play with the binning!



## ----manually binning numeric variables, results = 'hide'---------------------------------
# notice what happens by default
table(age)

# bin age into 6 brackets
age_binned <- cut(age, breaks = 6)

# then make a table
table(age_binned)

# manually specify the cutoffs
age_binned <- cut(age, breaks = c(15, 20, 30, 40))
table(age_binned)

# show *relative* frequency distribution
table(age_binned)/length(age_binned)


## ----your turn 2--------------------------------------------------------------------------
# bin body weight into 10 intervals 

# tabulate frequency distribution

# repeat with bin limits 80-130, 130-180, 180-220, 220-320



## ----descriptive stats, results = 'hide'--------------------------------------------------
# 30th percentile of age
quantile(age, probs = 0.3)

# 30th *and* 60th percentile of age
quantile(age, probs = c(0.3, 0.6))

# the median gets its own function
median(age)

# ditto mean
mean(age)

# ... and standard deviation
sd(age)

# ... aaand interquartile range
IQR(age)

# ... AAAAAND minimum and maximum
min(age)
max(age)


## ----your turn 3--------------------------------------------------------------------------
# choose a *quantiative* variable from the dataset

# compute the five-number summary using quantile()

# compare the mean and the median. are they close?



## ----summary command----------------------------------------------------------------------
summary(age)


## ----median robustness, results = 'hide'--------------------------------------------------
# make up some observations between 1 and 100
x <- sample(1:100, size = 20)
x

# median of made up observations, plus 101
median(c(x, 101))

# median of made up observations, plus 1M
median(c(x, 1000000))

# same comparison, but with mean
mean(c(x, 101))
mean(c(x, 1000000))


## ----skewness-----------------------------------------------------------------------------
# distribution of ages is right-skewed
hist(age)

# plot the mean and median on top of the histogram
hist(age)
abline(v = mean(age), col = 2)
abline(v = median(age), col = 4)


## ----skewness in summary------------------------------------------------------------------
# mean > median ---> right-skewed
summary(age)


## ----your turn 4--------------------------------------------------------------------------
# check the numeric summary for percent change in dominant arm strength
# can you tell the direction of skewness?? does it seem very skewed??



## ----IQR-sd comparison, results = 'hide'--------------------------------------------------
# extract dominant arm percent change in strength
drm <- famuss$drm.ch

# drop the observations over 80%
drm.drop <- drm[drm < 100]

# compute the numeric summary with and without outliers
summary(drm)
summary(drm.drop)

# compare standard deviations
sd(drm)/sd(drm.drop)

# compare interquartile ranges
IQR(drm)/IQR(drm.drop)


## ----robustness of IQR, results = 'hide'--------------------------------------------------
# add a large observation
drm.add <- c(drm, 1000)

# compare IQR with and without
IQR(drm.add)/IQR(drm)

# compare SD with and without
sd(drm.add)/sd(drm)


## -----------------------------------------------------------------------------------------
# load a new dataset (census)
data(census.2010)

# number of doctors per state (thousands)
doctors <- census.2010$doctors

# compute numeric summary -- guess whether there are outliers?

# make a histogram or boxplot to confirm your guess

# bonus: which state??



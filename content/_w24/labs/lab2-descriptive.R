## ----load package and data---------------------------------------------------------------
# load openintro biostat package
library(oibiostat)

# load famuss data
data(famuss)

# open data documentation
?famuss


## ----view data---------------------------------------------------------------------------
head(famuss)


## ----using dataframes, results = 'hide'--------------------------------------------------
# extract the age variable
famuss$age

# extract the bmi variable
famuss$bmi

# store the age column as a vector
age <- famuss$age


## ----your turn 0-------------------------------------------------------------------------
# try it yourself: pick a variable to extract and store



## ----easy graphs and tables, results='hide', fig.show='hide'-----------------------------
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


## ----your turn 1-------------------------------------------------------------------------
# make a table of the frequency distribution of genotypes

# make a barplot of the frequency distribution of genotypes

# make a histogram of dominant arm change; play with the binning!


## ----descriptive stats, results = 'hide'-------------------------------------------------
# the median gets its own function
median(age)

# ditto mean
mean(age)

# ... and minimum and maximum
min(age)
max(age)

# 30th percentile of age ("quantile" is another term for percentile)
quantile(age, probs = 0.3)

# 30th *and* 60th percentile of age
quantile(age, probs = c(0.3, 0.6))


## ----your turn 2-------------------------------------------------------------------------
# choose a *quantiative* variable from the dataset

# compute the five-number summary using quantile()
quantile(age, probs = c(0, .25, .5, .75, 1))

# compare the mean and the median. are they close?



## ----summary command---------------------------------------------------------------------
summary(age)


## ----median robustness, results = 'hide'-------------------------------------------------
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


## ----skewness----------------------------------------------------------------------------
# distribution of ages is right-skewed
hist(age)

# plot the mean and median on top of the histogram
hist(age)
abline(v = mean(age), col = 2)
abline(v = median(age), col = 4)


## ----skewness in summary-----------------------------------------------------------------
# mean > median ---> right-skewed
summary(age)


## ----your turn 3-------------------------------------------------------------------------
# check the numeric summary for percent change in dominant arm strength
# can you tell the direction of skewness?? does it seem very skewed??



## ----------------------------------------------------------------------------------------
knitr::purl(input = 'content/labs/lab2-descriptive.qmd',
            output = 'content/labs/lab2-descriptive.R')


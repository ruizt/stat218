## ----load packages and data--------------------------------------------------------------------------------------------------------
# openintro biostat package
library(oibiostat)

# famuss data
data(famuss)


## ----calculating measures of spread, results = 'hide'------------------------------------------------------------------------------
# extract dominant arm percent change in strength
drm <- famuss$drm.ch

# calculate standard deviation
sd(drm)

# interquartile range
IQR(drm)

# average deviation
mean(abs(drm - mean(drm)))

# range
max(drm) - min(drm)

# range endpoints
range(drm)


## ----graphical check---------------------------------------------------------------------------------------------------------------
# boxplot of percent change in dominant arm strength
boxplot(drm, horizontal = T, range = 2)


## ----IQR-sd comparison, results = 'hide'-------------------------------------------------------------------------------------------
# drop the observations over 60%
drm.drop <- drm[drm < 60]

# compute the numeric summary with and without outliers
summary(drm)
summary(drm.drop)

# compare standard deviations
sd(drm)/sd(drm.drop)

# compare interquartile ranges
IQR(drm)/IQR(drm.drop)


## ----robustness of IQR, results = 'hide'-------------------------------------------------------------------------------------------
# add a large observation
drm.add <- c(drm, 1000)

# compare IQR with and without
IQR(drm.add)/IQR(drm)

# compare SD with and without
sd(drm.add)/sd(drm)


## ----your turn 1-------------------------------------------------------------------------------------------------------------------
# load a new dataset (census)
data(census.2010)

# number of doctors per state (thousands)
doctors <- census.2010$doctors

# compute numeric summary -- guess whether there are outliers?

# make a histogram or boxplot to confirm your guess

# bonus: can you figure out which state??



## ----cat-cat example, results = 'hide'---------------------------------------------------------------------------------------------
# retrieve the genotype and sex columns
genotype <- famuss$actn3.r577x
sex <- famuss$sex

# construct a contingency table
table(genotype, sex)

# stacked bar plots -- automatically groups by column
tbl <- table(famuss$actn3.r577x, famuss$sex)
barplot(tbl, legend = T)

# turn the table on its side with t() to group by row
barplot(t(tbl), legend = T)

# row and column margins
rowSums(tbl)
colSums(tbl)

# proportions, grouping by row
tbl_row <- tbl/rowSums(tbl)
tbl_row

# proportional stacked bar plot, grouped by row
barplot(t(tbl_row), legend = T)

# proportions, grouping by column (a little trickier)
tbl_col <- t(t(tbl)/colSums(tbl))
tbl_col

# proportional stacked bar plot, groupde by column
barplot(tbl_col, legend = T, horiz = T)


## ----your turn 2-------------------------------------------------------------------------------------------------------------------
# retrieve the genotype and race columns

# make a contingency table of genotype and race

# are there apparent genotype differences by race? make an appropriate bar plot



## ----num-num, results = 'hide'-----------------------------------------------------------------------------------------------------
# retrieve height and weight columns
height <- famuss$height
weight <- famuss$weight

# basic scatterplot
plot(height, weight)

# correlation
cor(weight, height)


## ----your turn 3-------------------------------------------------------------------------------------------------------------------
# retrieve the percent change variables

# construct a scatterplot

# compute the correlation



## ----num-cat, results = 'hide'-----------------------------------------------------------------------------------------------------
# side-by-side boxplots for non-dominant arm
boxplot(ndrm.ch ~ actn3.r577x, data = famuss)

# change the orientation 
boxplot(ndrm.ch ~ actn3.r577x, data = famuss, horizontal = T)

# change the whisker length (range = multiples of IQR)
boxplot(ndrm.ch ~ actn3.r577x, data = famuss, horizontal = T, range = 2)

# side-by-side boxplots for dominant arm
boxplot(drm.ch ~ actn3.r577x, data = famuss, horizontal = T)


## ----your turn 4-------------------------------------------------------------------------------------------------------------------
# make side-by-side boxplots of BMI by race



## ----packages and data------------------------------------------------------------------------------
library(modelr)
library(Sleuth3)
load('data/prevend.RData')


## ----slr review, echo = T---------------------------------------------------------------------------
# fit model and check summary
fit <- lm(RFFT ~ Age, data = prevend)
summary(fit)

# residual diagnostics: residual-fit plot
plot(fit$fitted.values, fit$residuals)
abline(h = 0)

# residual diagnostics: residual histogram
hist(fit$residuals)

# residual diagnostics: qq plot
qqnorm(fit$residuals)
qqline(fit$residuals)

# significance tests
summary(fit)$coef

# confidence intervals for parameter estimates
confint(fit, level = 0.95)

# model visualization
coefs <- coef(fit)
plot(prevend)
abline(a = coefs[1], b = coefs[2])


## ----predictions, echo = T--------------------------------------------------------------------------
# point prediction
predict(fit, newdata = data.frame(Age = 55))

# prediction interval for an observation
predict(fit, newdata = data.frame(Age = 55), interval = 'prediction', level = 0.95)

# confidence interval for the mean
predict(fit, newdata = data.frame(Age = 55), interval = 'confidence', level = 0.95)


## ----visualizing one prediction, echo = T-----------------------------------------------------------
# store prediction
pred <- predict(fit, newdata = data.frame(Age = 55))

# plot data, add line, add point
plot(prevend)
abline(a = coefs[1], b = coefs[2])
points(x = 55, y = pred, pch = 18, col = 'red')


## ----visualizing intervals, echo = T----------------------------------------------------------------
# generate a grid of 100 points spanning the range of ages
x <- seq_range(prevend$Age, n = 100)

# compute predictions
preds <- predict(fit, newdata = data.frame(Age = x), 
                 interval = 'confidence', level = 0.95)

# add to plot
plot(prevend)
abline(a = coefs[1], b = coefs[2], col = 'blue')
lines(x = x, y = preds[, 2], lty = 2, col = 'blue')
lines(x = x, y = preds[, 3], lty = 2, col = 'blue')


## ----your turn 1, echo = T, result = 'hide', fig.show = 'hide'--------------------------------------
# generate a grid of 100 points spanning the range of ages
x <- seq_range(prevend$Age, n = 100)

# compute predictions
preds <- predict(fit, newdata = data.frame(Age = x), 
                 interval = 'prediction', level = 0.9)

# add to plot
plot(prevend)
abline(a = coefs[1], b = coefs[2], col = 'blue')
lines(x = x, y = preds[, 2], lty = 2, col = 'blue')
lines(x = x, y = preds[, 3], lty = 2, col = 'blue')


## ----practice problem 1, results = 'hide', echo = T-------------------------------------------------
# claw data
head(ex0722)
plot(ex0722$Height, ex0722$Force)

# fit model of closing force (response) against height (explanatory)
fit <- lm(Force ~ Height, data = ex0722)

# check diagnostics
plot(fit$fitted.values, fit$residuals)
abline(h = 0)

hist(fit$residuals)

qqnorm(fit$residuals)
qqline(fit$residuals)

# model summary
summary(fit)

# interval estimate for association
confint(fit, level = 0.95)

# prediction for claw height 12.2
predict(fit, newdata = data.frame(Height = 12.2),
        interval = 'prediction', level = 0.95)


## ----practice problem 2, results = 'hide', echo = T-------------------------------------------------
# galapagos data
head(ex1220)

# plot total species count against area
plot(ex1220$Area, ex1220$Total)

# same, but log-transformed
plot(log(ex1220$Area), log(ex1220$Total))

# fit a model of log-total count against log-area
fit <- lm(log(Total) ~ log(Area), data = ex1220)

# check diagnostics

# check model summary for quality of fit
summary(fit)

# compute and interpret CI for association parameter
confint(fit, level = 0.95)

2^(confint(fit, level = 0.95)[2, ])


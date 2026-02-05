library("ISLR2")
# We will use the Boston dataset
# These exercises are from ISLR Chapter 03 Lab
head(Boston)

BostonModel <- lm(medv ~ lstat, data = Boston)
BostonModel
summary(BostonModel)
confint(BostonModel)

# Predicting using our linear fit
predict(BostonModel, data.frame(lstat = (c(5, 10, 15))),
        interval = "confidence")
# We get a fit, a lower and an upper limit with interval = confidence

predict(BostonModel, data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction")
# Notice that prediction interval is wider than confidence interval, both centered around 25.05

# Plotting
par(mfrow = c(2, 2))
plot(Boston$lstat, Boston$medv)
abline(BostonModel)
plot(BostonModel)
# Setting a 2x2 grid then plotting gives us all 4 plots at once.

# ================================
# MULTIPLE REGRESSION
# ================================
BostonMLR <- lm(medv ~ ., data = Boston)
BostonMLR
summary(BostonMLR)
# We see the indus and age have a high p-value
BostonMLR_wo_age_indus <- lm(medv ~ . - age - indus, data = Boston)
summary(BostonMLR_wo_age_indus)

# Interaction Terms
BostonMLR_intr <- lm(medv ~ lstat * age, data = Boston)
summary(BostonMLR_intr)
# lstat:age has a significant p-value, even though age by itself doesn't.

# Non-linear terms
# ----------------
# Use I(X^2)
# Wrapped in I() because ^ has a special meaning in R if used by itself. I() tells R
# to treat it as a mathematical operator inside the function lm()
# Outside functions, ^ is treated as power even without I()
# Inside functions like lm(), ^2 indicates every 2 way interaction between the terms,
# e.g. lm(y ~ (a + b)^2) gives us (a +b + a:b) where a:b is the 2-way interaction
# I() means Inhibit Interpretation
# lm(y ~ x^2) will be same as y ~ x
# lm(y - I(x^2)) will take x^2 as the predictor, instead of x

Boston_non_linear <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(Boston_non_linear)

# Using anova() to quantify how much the quadratic fit is superior to linear
Boston_linear <- lm(medv ~ lstat, data = Boston)
Boston_quadratic <- lm(medv ~ lstat + I(lstat^2), data = Boston)
anova(Boston_linear, Boston_quadratic)

par(mfrow = c(2,2))
plot(Boston_quadratic) # Notice how the pattern in Residual vs Fitted is less discernible now

# Higher order polynomial without typing each power by hand
# Use poly(predictor, power) function
Boston_poly5 <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(Boston_poly5)
anova(Boston_linear, Boston_poly5)
anova(Boston_quadratic, Boston_poly5)
plot(Boston_poly5)
# Significant till 5th power, let's test even higher ones
Boston_poly6 <- lm(medv ~ poly(lstat, 6), data = Boston)
anova(Boston_poly5, Boston_poly6)
# p-value is 0.21 -> NOT SIGNIFICANT!
# Just to make sure, let's try including more and checking
Boston_poly10 <- lm(medv ~ poly(lstat, 10), data = Boston)
anova(Boston_poly5, Boston_poly10) # p-value 0.15

Boston_poly15 <- lm(medv ~ poly(lstat, 15), data = Boston)
anova(Boston_poly5, Boston_poly15) # p value 0.026
# Let's check between 10 and 15
anova(Boston_poly10, Boston_poly15) # p value 0.03
# Hm.. what's going on here?
# It's a classic case of overfitting!
# Now, with poly 15, our model fits the training data better,
# But it has a high variance.
# It's too flexible.

# Let's try logs
Boston_log <- lm(medv ~ log(rm), data = Boston)
summary(Boston_log)
# Let's compare log(rm) and rm
Boston_rm_linear <- lm(medv ~ rm, data = Boston)
anova(Boston_rm_linear, Boston_log) # Won't work, these models are not nested
AIC(Boston_rm_linear, Boston_log) # Cross Validation using AIC
# We see that the linear model is better (44 units difference!)
# Do I know what AIC does? No. That comes in ISLR Chapter 05, which is still a long way off












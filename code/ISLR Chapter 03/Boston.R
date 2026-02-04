library("ISLR2")
# We will use the Boston dataset
head(Boston)

BostonModel <- lm(medv ~ lstat, data = Boston)
BostonModel
summary(BostonModel)

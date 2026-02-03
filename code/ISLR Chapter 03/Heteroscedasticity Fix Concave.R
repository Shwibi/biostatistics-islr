library(MASS)
data("Cars93")

# Linear model fit
log_model <- lm(log(Price) ~ Horsepower, data=Cars93)
summary(log_model)

# We get an estimate for horsepower to be 0.007
# This means that 1 unit in horsepower increases response by ~0.7%

# Let's try predicting
new_car <- data.frame(Horsepower = 200)
log_pred <- predict(log_model, newdata = new_car) # this gives log value
actual_price_pred <- exp(log_pred)

print(paste("Predicted Price:", round(actual_price_pred, 2)))
print(paste("Log Prediction:", round(log_pred, 2)))

# Visualizing this model
library(ggplot2)

ggplot(Cars93, aes(x = Horsepower, y = Price)) +
  geom_point(color = "deeppink", alpha = 0.5) +
  stat_smooth(method = "glm", # This is the log fitted model
              formula = y ~ x, 
              method.args = list(family = gaussian(link = "log")), 
              color = "darkred",
              se=FALSE) +
  geom_smooth(method="lm", color="black", se=FALSE, alpha=0.4) + # This generates the straight model
  labs(title = "Log-Response Model",
       subtitle = "Model: log(Price) ~ Horsepower")

# NOTE: This was taken from the middle of chapter 03 and not from the Labs.
# There is not much difference between using stat_smooth and geom_smooth; stat_smooth is just the
# convention when we're doing statistics "on the fly"
# Using geom_smooth would give the same result.


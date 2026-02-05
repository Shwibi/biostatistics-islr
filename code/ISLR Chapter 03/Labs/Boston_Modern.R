# ISLR Chapter 03 Labs
# Boston

library(ISLR2)
library(tidymodels)
library(ggplot2)
library(patchwork) # instead of using global par(mfrow)

# Custom functions
source("code/utils/plot_functions.R")
source("code/utils/ka_modern_islr.R")

lm_spec <- linear_reg() %>% 
  set_engine("lm")

lm_fit <- lm_spec %>% 
  fit(medv ~ lstat, data = Boston)

tidy(lm_fit)
# Already, we can see the p-value is not just <2e-16, but rather 5.08e-88
# Which is insane
# For confidence interval, same func can be used

tidy(lm_fit, conf.int = TRUE, conf.level = 0.95)

# Prediction
new_points <- tibble(lstat = c(5, 10, 15))
predict(lm_fit, new_data = new_points, type = "conf_int")
predict(lm_fit, new_data = new_points, type = "pred_int")
# ..this doesn't need much explaining.

lm_fit %>% 
  predict(new_data = new_points) %>% 
  bind_cols(predict(lm_fit, new_data = new_points, type = "conf_int"))

lm_fit %>% 
  augment(new_data = new_points)

# Plotting
p1 <- ggplot(Boston, aes(x = lstat, y = medv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "LSTAT vs MEDV")
print(p1)

boston_diag <- augment(lm_fit$fit, new_data = Boston)

p2 <- ggplot(boston_diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(title = "Residuals vs Fitted")
print(p2)

p3 <- ggplot(boston_diag, aes(sample = .resid)) +
  stat_qq() + 
  stat_qq_line(color = "hotpink") +
  theme_minimal() +
  labs(title = "Normal Q-Q")
print(p3)

p4 <- ggplot(boston_diag, aes(x = .hat, y = .std.resid)) +
  geom_point(aes(size = .cooksd), alpha = 0.5) + # Size points by influence
  geom_smooth(se = FALSE, color = "hotpink") +
  theme_minimal() +
  labs(
    title = "Residuals vs Leverage",
    x = "Leverage",
    y = "Standardized Residuals"
  )

# Grid using pathwork
(p1 + p2) / (p3 + p4)

# We can clearly see the U shape in Residuals vs Fitted
# Let's try polynomial regression

boston_recipe <- recipe(medv ~ lstat, data = Boston) %>% 
  step_poly(lstat, degree = 5)
poly_workflow <- workflow() %>% 
  add_recipe(boston_recipe) %>% 
  add_model(lm_spec)
poly_fit <- fit(poly_workflow, data = Boston)
poly_preds <- augment(poly_fit, new_data = Boston)

poly_diag <- augment(poly_fit, new_data = Boston)

resplot <- residual_plot(poly_diag, ".pred", ".resid") + labs(title = "Residuals vs. Fitted")
print(resplot)

# Just change the poly from 2 to different values, we can easily see the change in residuals vs fitted

# ===========================
# Performance check
# ===========================
library(performance)
fit_linear <- lm_fit$fit
fit_poly <- extract_fit_engine(poly_fit)
compare_performance(fit_linear, fit_poly, metrics = "all")
# poly fit is clearly better!









  
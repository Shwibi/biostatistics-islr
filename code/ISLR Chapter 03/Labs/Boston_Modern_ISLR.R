# This follows https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/03-linear-regression.html
# Which is the modern labs from ISLR authors
# This makes things easier, doesn't it

library(MASS)
library(tidymodels)
library(ISLR)

lm_spec <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

lm_fit <- lm_spec %>% 
  fit(medv ~ lstat, data = Boston)

lm_fit %>% 
  pluck("fit") %>% 
  summary()

tidy(lm_fit) # Get the parameter estimates
glance(lm_fit) # Model statistics

# Predictions
predict(lm_fit, new_data = Boston, type = "conf_int")
bind_cols(
  predict(lm_fit, new_data = Boston),
  Boston) %>% 
  dplyr::select(medv, .pred)
# explicitly mention dplur::select to prevent conflict with MASS::select

# alternative:
lm_fit %>% 
  augment(new_data = Boston) %>% 
  dplyr::select(medv, .pred)

# ========================
# MULTIPLE REGRESSION
# ========================

lm_fit2 <- lm_spec %>% 
  fit(medv ~ lstat + age, data = Boston)

lm_fit2

lm_fit3 <- lm_spec %>% 
  fit(medv ~ ., data = Boston)

tidy(lm_fit3)

?formula # Actually quite useful

# Interaction
lm_fit4 <- lm_spec %>% 
  fit(medv ~ lstat * age, data = Boston)
tidy(lm_fit4)

# Using recipes
rec_spec_interact <- recipe(medv ~ lstat + age, data = Boston) %>% 
  step_interact(~ lstat:age)

lm_wf_interact <- workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(rec_spec_interact)

lm_wf_interact %>% fit(Boston)

rec_spec_pow2 <- recipe(medv ~ lstat, data = Boston) %>% 
  step_mutate(lstat2 = lstat^2)

lm_wf_pow2 <- workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(rec_spec_pow2)

lm_wf_pow2_fitted <- lm_wf_pow2 %>% fit(Boston)

# ==================
# Carseats
# ==================

cs_lm_fit <- lm_spec %>% 
  fit(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
tidy(cs_lm_fit)

# We cannot relty on engines to automatically assign dummy variables, though





























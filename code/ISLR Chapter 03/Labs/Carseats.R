# ISLR Chapter 03
# Labs: 3.6.6
# Using 3.6 from https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/03-linear-regression.html#qualitative-predictors
library("ISLR")
head(Carseats)

# Includes Qualitative Predictors
# Shelveloc -> Bad / Medium / Good
# R generates dummy vars automatically

lm_spec <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

Carseats %>% 
  pull(ShelveLoc) %>% 
  contrasts()

lm_fit <- lm_spec %>% 
  fit(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
tidy(lm_fit)

# We cannot rely on engines to automatically assign dummy variables, though
rec_spec <- recipe(Sales ~ ., data = Carseats) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ Income:Advertising + Price:Age)

lm_wf <- workflow() %>% 
  add_model(lm_spec) %>% 
  add_recipe(rec_spec)

lm_fit <- lm_wf %>% fit(Carseats)
tidy(lm_fit)




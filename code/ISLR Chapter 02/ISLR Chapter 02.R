library(tidyverse)
college <- read_csv("data/College.csv")

# Move first column to turn them into row names
college <- college %>% column_to_rownames(var = "University")
college

college %>% 
  select(where(is.numeric)) %>% 
  select(1:10) %>% 
  pairs()

college %>% 
  ggplot(aes(x=Private, y=Outstate, fill=Private)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3) +
  labs(title="Outstate Tuition: Private vs Public Universities",
       x = "Private University",
       y = "Outstate Tuition ($)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Create the Elite variable using mutate
college <- college %>% 
  mutate(Elite = if_else(Top10perc > 50, "Yes", "No"),
         Elite = as.factor(Elite))

# Check counts
college %>% count(Elite)

# Boxplot
college %>% 
  ggplot(aes(x=Elite, y=Outstate, fill=Elite)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values=c("No" = "#E69F00", "Yes" = "#56B4E9"))

library(tidyverse)

college %>%
  ggplot(aes(x = Outstate, fill = Private)) +
  geom_histogram(bins = 20, color = "white") +
  facet_wrap(~Private) +  # This replaces par(mfrow) logic
  theme_minimal() +
  labs(title = "Distribution of Outstate Tuition",
       x = "Tuition ($)",
       y = "Frequency")


# ============================================

auto <- read_table("data/Auto.data", na="?")
View(auto)

auto_clean <- auto %>% 
  unite("name", name:X10, sep = " ", remove=TRUE) %>% 
  drop_na()

View(auto_clean)

range(auto_clean$horsepower, na.rm = TRUE) # OLD APPROACH


auto_clean %>% 
  summarise(
    min_mpg = min(mpg),
    max_mpg = max(mpg)
  )

summary(auto_clean)
?summary

auto_clean %>% 
  summarise(across(where(is.numeric),
                   list(mean=mean, sd=sd))) %>% 
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "_")

auto_subset <- auto_clean[-c(10:85), ]
auto_subset %>% 
  summarise(across(where(is.numeric),
                   list(mean=mean, sd=sd,
                        min=min, max=max))) %>% 
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "_")

# Auto dataset graphs
library(GGally)

auto_clean %>% 
  select(mpg, displacement, horsepower, weight, acceleration) %>% 
  ggpairs()


ggplot(auto_clean, aes(x = weight, y = mpg)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method =" loess", color = "red")



auto_clean %>% 
  ggplot(aes(x=displacement, y = mpg)) +
  geom_point(alpha=0.5, color = "steelblue") +
  geom_smooth(method="loess", color = "red")

auto_clean %>% 
  ggplot(aes(x=displacement, y=horsepower)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "darkgreen") +
  labs(title = "Engine Displacement vs. Horsepower")

ggplot(auto_clean, aes(x = factor(year), y = mpg)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "MPG Distribution by Model Year", x = "Year (1970-1982)")

# Multiple linear regression
library(broom)

# Fit the model
# We exclude 'name' because it's a unique identifier, not a predictor
model <- lm(mpg ~ . - name, data = auto_clean)

# View the results in a tidy table
tidy(model)
# Modern way to see diagnostic plots
par(mfrow = c(2, 2))
plot(model)

set.seed(42)
x <- 1:10
y <- 2 * x + rnorm(10)

# Outlier
x <- c(x, 25)
y <- c(y, 20)

model <- lm(y ~ x)

raw_res <- resid(model)
stud_res <- rstudent(model) # Externally studentized

results <- data.frame(Raw = raw_res, Studentized = stud_res)
print(results)

# Plotting
par(mfrow = c(1,2))

plot(model, which = 1, main = "Standard Residuals")
     
plot(model, which = 5, main = "Residuals vs Leverage") # Plots internally studentized

# Identifying and Comparing
outlier_indices <- which(abs(stud_res) > 3)

clean_data <- data.frame(x, y)[-outlier_indices, ]

clean_model <- lm(y ~ x, data = clean_data)

cat("Original Slope:", coef(model)[2], "\n")
cat("Cleaned Slope:", coef(clean_model)[2], "\n")
# We see that the cleaned slope is much closer to (2) which was our model slope
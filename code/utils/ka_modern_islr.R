library(ggplot2)
library(patchwork)
library(broom)

four_islr_diagnostics <- function(model_fit, color_theme = "#ff7eb9") {
  
  # 1. Logic to handle both parsnip and engine objects
  # This ensures we always have .cooksd and .hat
  if (inherits(model_fit, "workflow") || inherits(model_fit, "model_fit")) {
    diag_data <- augment(model_fit$fit)
  } else {
    diag_data <- augment(model_fit)
  }
  
  # 2. Define the plots (standardizing on .fitted)
  p1 <- ggplot(diag_data, aes(.fitted, .resid)) + 
    geom_point(alpha = 0.4) + geom_smooth(method="loess", color=color_theme, se=F) +
    labs(title = "Residuals vs Fitted")
  
  p2 <- ggplot(diag_data, aes(sample = .resid)) + 
    stat_qq() + stat_qq_line(color = color_theme) +
    labs(title = "Normal Q-Q")
  
  p3 <- ggplot(diag_data, aes(.fitted, sqrt(abs(.std.resid)))) + 
    geom_point(alpha = 0.4) + geom_smooth(method="loess", color=color_theme, se=F) +
    labs(title = "Scale-Location")
  
  p4 <- ggplot(diag_data, aes(.hat, .std.resid)) + 
    geom_point(aes(size = .cooksd), alpha = 0.4) +
    labs(title = "Residuals vs Leverage")
  
  # 3. Combine and Return
  return((p1 + p2) / (p3 + p4) & theme_minimal())
}
# NOT IN LABS
# Small little distraction
# I'm getting tired of writing the ggplot() functions over and over
# And I feel  like I'm going to use the residual plot a lot
# So we'll make a custom function that returns a ggplot of residuals
library(ggplot2)
quick_plot <- function(data, x_var, y_var, dot_color = "hotpink") {
  plot <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(color = dot_color, alpha = 0.5) +
    theme_minimal()
  return(plot)
}

residual_plot <- function(data, x_var, y_var, line_color = "hotpink") {
  plot <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(alpha = 0.4, color = "slategray") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    # geom_smooth adds the 'trend' line to see if patterns remain
    geom_smooth(method = "loess", color = line_color, se = FALSE) + 
    theme_minimal() +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted Values",
      y = "Residuals"
    )
  return(plot)
}
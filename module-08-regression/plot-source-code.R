# Load necessary libraries
library(ggplot2)
library(dplyr)


################ Creating plot for the sum of squares of residuals ################
# Create synthetic data
df <- data.frame(
  x = 1:8,
  y = c(3, 6, 5, 7, 10, 11, 15, 18)
)

# Compute mean of y
y_mean <- mean(df$y)

# Fit a linear model
model <- lm(y ~ x, data = df)

# Get fitted values
df <- df %>%
  mutate(
    y_pred = predict(model),
    residuals = y - y_pred
  )

# Plot 1: Residuals from the mean
p_sst<- ggplot(df, aes(x, y)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) +
  geom_hline(yintercept = y_mean, color = "black", linewidth = 1) +
  geom_segment(aes(x = x, xend = x, y = y, yend = y_mean), linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(x = "x", y = "Y")

# Plot 2: Residuals from the regression line
p_ssr <- ggplot(df, aes(x, y)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) +
  geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color = "blue", linewidth = 1) +
  geom_hline(yintercept = y_mean, color = "black", linewidth = 1) +
  geom_segment(aes(x = x, xend = x, y = y, yend = y_pred), linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(x = "x", y = "Y")








## working directory
# setwd("D:/Githu-repository/econ148-analytical-stat-packages/econ148-lecture/module-2")
# setwd("c:\\Users\\chris\\Documents\\Github-repository\\econ148-lecture\\module-04-eda")

## load packages
library(tidyverse)
library(glue)
library(patchwork)
library(openintro)
library(ggthemes)
library(latex2exp)

## setting theme
theme_set(theme_bw(base_size = 20)) # Set theme for all ggplots

## loading dataset
wildlife_impacts <- read_csv("data/wildlife_impacts.csv")
days_to_ship <- tibble(
  order = seq(12),
  warehouseA = c(3,3,3,4,4,4,5,5,5,5,5,5),
  warehouseB = c(1,1,1,3,3,4,5,5,5,6,7,10))


## plot 1 wildlife histogram
p_wild_life_hist <- 
  wildlife_impacts |> 
  select(height) |> 
  na.omit() |> 
  ggplot(aes(x = height)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(height, na.rm = TRUE)), color = "red", 
             linewidth = 1, lty = "dashed") +
  geom_vline(aes(xintercept = median(height, na.rm = TRUE)), color = "blue",
             linewidth = 1, lty = "dashed") +
  scale_x_continuous(breaks = seq(0, 20e3, 5e3)) +
  scale_y_continuous(breaks = seq(0, 30e3, 5e3), limits = c(0, 30e3)) +
  # label the mean and median using annotate
  annotate("label", x = mean(wildlife_impacts$height, na.rm = TRUE), y = 27e3, 
           label = "Mean", color = "red") +
  annotate("label", x = median(wildlife_impacts$height, na.rm = TRUE), y = 29e3,
           label = "Median", color = "blue") +
  labs(x = "Height (ft)",
       y = "Count",
       title = "Distribution of height")

ggsave(
  filename = "plot/wildlife_impacts_hist.png",
  plot = p_wild_life_hist,
  width = 10,
  height = 6
)


## days to shape
# Calculate range and SD for each warehouse
annot_data <- days_to_ship |> 
  pivot_longer(-order, names_to = "warehouse", values_to = "days") |> 
  group_by(warehouse) |> 
  summarize(
    range = max(days) - min(days),
    sd = sd(days)
  )

# Annotate dynamically in the plot
p_days_to_ship_1 <- 
  days_to_ship |> 
  pivot_longer(-order, names_to = "warehouse", values_to = "days") |> 
  mutate(s_days = scale(days)) |> 
  ggplot(aes(x = order, y = s_days)) +
  geom_col() +
  facet_wrap(~warehouse) +
  geom_label(
    data = annot_data,
    aes(
      x = 10, y = 2.5, 
      label = paste0("Range: ", round(range, 2), "\nSD: ", round(sd, 2))
    ),
    hjust = 1.1, vjust = 1.1,
    color = "black", fill = "lightblue", size = 4
  ) +
  labs(
    y = "Days to ship (scaled)"
  )

ggsave(
  filename = "plot/days_to_ship.png",
  plot = p_days_to_ship_1,
  width = 12,
  height = 6
)


# anscombe plot
# Function to create a correlation plot
create_corr_plot <- function(data, x, y) {
  corr <- round(cor(data[[x]], data[[y]]), 2)
  
  ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = glue("Correlation: {corr}")) +
    theme(axis.title = element_text(face = "bold"))
}

# Create a correlation plot for each pair of x and y
# Combine the plots for x1-y1 and x2-y2, and x3-y3 and x4-y4
p_corr_anscombe <- 
(create_corr_plot(anscombe, "x1", "y1") + create_corr_plot(anscombe, "x2", "y2")) / 
(create_corr_plot(anscombe, "x3", "y3") + create_corr_plot(anscombe, "x4", "y4"))

## saving plot
ggsave("plot/p_corr_anscombe.png", p_corr_anscombe, width = 10, height = 8)


## summarising nominal data
p_nominal_barplot <- 
  wildlife_impacts |> 
  count(operator, sort = TRUE) |> 
  ggplot(aes(x = fct_reorder(operator, n), y = n)) +
  geom_col(width = 0.6) +
  coord_flip() +
  labs(x = "Operator", y = "Count") +
  theme_minimal()

ggsave(
  filename = "plot/nominal_barplot.jpeg",
  plot = p_nominal_barplot,
  width = 5,
  height = 3
)

## summarizing ordinal
p_ordinal <- 
  wildlife_impacts |> 
  count(incident_month, sort = TRUE) |> 
  ggplot(aes(x = as.factor(incident_month), y = n)) +
  geom_col() +
  labs(x = "Incident Month", y = "Count") 

ggsave(
  filename = "plot/ordinal_barplot.jpeg",
  plot = p_ordinal,
  width = 10,
  height = 6
)


####################
# plots for examining numerical data

## setting the theme
plot_theme <- 
  theme_set(theme_minimal()) +
  theme(plot.margin = margin(20, 20, 20, 20),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray85"),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),
        plot.title = element_text(size = 18, hjust = 0.5, margin = margin(b = 20))
  )

## scatter plot example
### figure 2.1. scatter plots
p_loan <- 
  loan50 |> 
  mutate(total_income = total_income / 1000) |> 
  mutate(loan_amount = loan_amount / 1000) |>
  ggplot(aes(total_income, loan_amount)) +
  geom_point(size = 3, color = "#377eb8") +
  scale_x_continuous(breaks = seq(0, 300, 50), labels = scales::label_currency(suffix = "K"), limits = c(0, 350), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 40, 10), labels = scales::label_currency(suffix = "K"), limits = c(0, 42), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Total Income", 
       y = "Loan Amount",
       title = "Scatter Plot of Total Income vs. Loan Amount"
      ) +
  plot_theme

ggsave(
  filename = "plot/loan_scatter.png",
  plot = p_loan,
  width = 10,
  height = 6
)

## figure 2.2 scatter plot with smooth
p_poverty <- 
  county |> 
  mutate(median_hh_income = median_hh_income / 1000) |>
  ggplot(aes(poverty, median_hh_income)) +
  geom_point(size = 3, color = "#377eb8", alpha = 0.8) +
  geom_point(size = 0.5, color = "gray40") +
  geom_smooth(color = "grey30", lty = "dashed") +
  scale_y_continuous(breaks = seq(0, 130, 20), labels = scales::label_currency(suffix = "K"), limits = c(0, 130), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 50, 10), labels = scales::label_number(suffix = "%"), limits = c(0, 50), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Poverty Rate (%)", 
       y = "Median Household Income",
       title = "Scatter Plot of Poverty Rate vs. Median Household Income"
      ) +
  plot_theme

ggsave(
  filename = "plot/poverty_scatter.png",
  plot = p_poverty,
  width = 10,
  height = 6
)

## figure 2.3 scatter plot population change
p_231 <- 
  county |> 
  mutate(pop2017 = pop2017 / 1e6) |> 
  ggplot(aes(pop2017, pop_change)) +
  geom_point(color = "#377eb8", size = 3, alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10), expand = c(0, 0), labels = scales::label_number(suffix = "m")) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  coord_cartesian(clip = "off") +
  labs(x = "(a) Population before change (m = millions)", 
       y = "Population Change (%)") +
  plot_theme

p_232 <- 
  county |> 
  mutate(pop2017 = pop2017 / 1e6) |> 
  ggplot(aes(pop2017, pop_change)) +
  geom_point(color = "#377eb8", size = 3, alpha = 0.7) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  scale_x_log10() +
  coord_cartesian(clip = "off") +
  labs(x = TeX("(b) $\\log_{10}$ Population before change (m = millions)"), 
       y = "Population Change (%)") +
  plot_theme

p_c_233 <- p_231 + p_232 + plot_layout(ncol = 2)

ggsave(
  filename = "plot/population_change_scatter.png",
  plot = p_c_233,
  width = 12,
  height = 6
)


## dotplot of interest rate
p_dotplot_interest <- 
  loan50 |>
  ggplot(aes(x = interest_rate)) +
  geom_dotplot(
    binwidth = 1, 
    method = "histodot",
    fill = "#5A9BD5", 
    color = "#5A9BD5",
    dotsize = 0.8,
    stackratio = 1.2
  ) +
  geom_point(
    aes(y = -0.05, x = mean(loan50$interest_rate)),
    color = "red",
    size = 4,
    shape = 17,
    fill = "red",
    stroke = 5
  ) +
  geom_hline(yintercept = 0, color = "gray60") +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_x_continuous(
    breaks = seq(5, 30, 5),
    labels = scales::percent_format(scale = 1),
    limits = c(5, 30)
  ) +
  coord_cartesian(ylim = c(-0.05, 1), clip = "off") +
  labs(x = "Interest Rate, Rounded to Nearest Percent") +
  plot_theme

ggsave(
  filename = "plot/interest_rate_dotplot.png",
  plot = p_dotplot_interest,
  width = 10,
  height = 6
)


## histogram
p_interest_hist <- 
  loan50 |> 
  ggplot(aes(x = interest_rate)) +
  geom_histogram(
    bins = 9,
    fill = "#5A9BD5",
    color = "white",
    boundary = 0
  ) +
  scale_x_continuous(
    breaks = seq(0, 30, 5),
    labels = scales::percent_format(scale = 1),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  labs(x = "Interest Rate",
       y = "Frequency"
  ) +
  plot_theme

ggsave(
  filename = "plot/interest_rate_histogram.png",
  plot = p_interest_hist,
  width = 10,
  height = 6
)


# histogram comparison: unimodal, bimodal, multimodal
set.seed(123)

# generate synthetic data
unimodal   <- rnorm(10000, mean = 10, sd = 2)
bimodal    <- c(rnorm(5000, mean = 5, sd = 1),
                rnorm(5000, mean = 8.5, sd = 1))
multimodal <- c(rnorm(3333, mean = 5,  sd = 1),
                rnorm(3333, mean = 8.5, sd = 1),
                rnorm(3334, mean = 12, sd = 1))

# Combine into one data frame
data <- bind_rows(
  data.frame(value = unimodal,   type = "Unimodal"),
  data.frame(value = bimodal,    type = "Bimodal"),
  data.frame(value = multimodal, type = "Multimodal")
) |> 
  tibble() |> 
  mutate(type = factor(type, levels = c("Unimodal", "Bimodal", "Multimodal")))

# Plot histograms side-by-side
p_histogram_modal <- 
  ggplot(data, aes(x = value)) +
  geom_histogram(bins = 35, fill = "steelblue", color = "white", boundary = 0) +
  facet_wrap(~ type, nrow = 1) +
  labs(x = "Value",
       y = "Frequency") +
  plot_theme + 
  theme(strip.text = element_text(size = 16))

ggsave(
  filename = "plot/histogram_modal_comparison.png",
  plot = p_histogram_modal,
  width = 12,
  height = 6
)



## variance and standard deviation
set.seed(42)

# Simulate data with different variances
low_var    <- rnorm(500, mean = 0, sd = 0.5)
medium_var <- rnorm(500, mean = 0, sd = 1)
high_var   <- rnorm(500, mean = 0, sd = 2)

# Combine into one data frame
data <- bind_rows(
  data.frame(value = low_var,    type = "Low Variance"),
  data.frame(value = medium_var, type = "Medium Variance"),
  data.frame(value = high_var,   type = "High Variance")
) |> 
  tibble() |>
  mutate(type = factor(type, levels = c("Low Variance", "Medium Variance", "High Variance")))

# Plot histograms
p_var_sd <- 
  ggplot(data, aes(x = value)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white", boundary = 0) +
  facet_wrap(~ type, ncol = 1) +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  labs(x = "Value",
       y = "Frequency") +
  plot_theme +
  theme(strip.text = element_text(size = 16))

## saving plot
ggsave(
  filename = "plot/variance_standard_deviation.png",
  width = 8,
  height = 10
)

## working directory
setwd("D:/Githu-repository/econ148-analytical-stat-packages/econ148-lecture/module-2")

## load packages
library(tidyverse)
library(glue)
library(patchwork)

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





## working directory
setwd("C:/Users/chris/Documents/Github-repository/econ148-2024/econ148-lecture/module-2")

## load packages
library(tidyverse)

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
  )

ggsave(
  filename = "plot/days_to_ship.png",
  plot = p_days_to_ship_1,
  width = 10,
  height = 4
)


















                                                         
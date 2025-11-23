# explore codes

## load packages
library(openintro)
library(tidyverse)
library(ggthemes)
library(patchwork)
library(latex2exp)

## load OI biostat
openintro::loan50 |> glimpse()

## setting the theme
plot_theme <- 
  theme_set(theme_minimal()) +
  theme(plot.margin = margin(20, 20, 20, 20),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray85"),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, margin = margin(r = 20))
  )

## figure 2.1
loan50 |> 
  mutate(total_income = total_income / 1000) |> 
  mutate(loan_amount = loan_amount / 1000) |>
  ggplot(aes(total_income, loan_amount)) +
  geom_point(size = 3, color = "#377eb8") +
  scale_x_continuous(breaks = seq(0, 300, 50), labels = scales::label_currency(suffix = "K"), limits = c(0, 350), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 40, 10), labels = scales::label_currency(suffix = "K"), limits = c(0, 42), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Total Income", 
       y = "Loan Amount") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray85"),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),
        axis.title.y = element_text(size = 16, margin = margin(r = 20))
)


## figure 2.2
county |> 
  glimpse()

county |> 
  mutate(median_hh_income = median_hh_income / 1000) |>
  ggplot(aes(poverty, median_hh_income)) +
  geom_point(size = 3, color = "#377eb8", alpha = 0.8) +
  geom_point(size = 0.5, color = "gray40") +
  geom_smooth(color = "grey30", lty = "dashed") +
  scale_y_continuous(breaks = seq(0, 130, 20), labels = scales::label_currency(suffix = "K"), limits = c(0, 130), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 40, 10), labels = scales::label_currency(suffix = "K"), limits = c(0, 50), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Poverty Rate (%)", 
       y = "Median Household Income") +
  plot_theme


## figure 2.3
county |> 
  glimpse()

p_231 <- 
  county |> 
  mutate(pop2017 = pop2017 / 1e6) |> 
  ggplot(aes(pop2017, pop_change)) +
  geom_point(color = "#377eb8", size = 3, alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10), expand = c(0, 0), labels = scales::label_number(suffix = "m")) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  coord_cartesian(clip = "off") +
  labs(x = "Population before change (m = millions)", 
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
  labs(x = TeX("$\\log_{10}$ Population before change (m = millions)"), 
       y = "Population Change (%)") +
  plot_theme

p_231 + p_232 + plot_layout(ncol = 2)


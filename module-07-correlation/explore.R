## library
library(tidyverse)
library(openintro)

openintro::t
TGT |> glimpse()



sugar_dta <- 
  tibble(
  y = c(2, 3, 5, 7, 0),
  x = c(1, 3, 12, 20, 0)
) |> 
  mutate(lab_point = paste0("(", x, ", ", y, ")"))

ggplot(data = sugar_dta, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = lab_point), vjust = -1) +
  scale_x_continuous(breaks = seq(0, 20, 5), limits = c(0, 20), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 10, 5), limits = c(0, 10), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_classic()

# set working directory
setwd("C:/Users/chris/Documents/Github-repository/econ149-lecture/module-2-correlation")

library(ggplot2)
library(dplyr)

# Create the data frame
data <- data.frame(
  Person = 1:5,
  Adverts = c(5, 4, 4, 6, 8),
  Packets = c(8, 9, 10, 13, 15)
)

# Calculate means
mean_adverts <- mean(data$Adverts)
mean_packets <- mean(data$Packets)

# Compute differences from the mean
data <- data %>%
  mutate(
    Adverts_diff = Adverts - mean_adverts,
    Packets_diff = Packets - mean_packets
  )

# Create the plot
ggplot(data, aes(x = Person)) +
  geom_point(aes(y = Adverts), color = "blue", size = 3) +
  geom_point(aes(y = Packets), color = "blue", size = 3) +
  geom_hline(yintercept = mean_adverts, color = "blue", linewidth = 1) +
  geom_hline(yintercept = mean_packets, color = "gray40", linewidth = 1) +
  geom_segment(aes(x = Person, xend = Person, y = mean_adverts, yend = Adverts), 
               linetype = "dashed", color = "black") +
  geom_segment(aes(x = Person, xend = Person, y = mean_packets, yend = Packets), 
               linetype = "dashed", color = "black") +
  geom_text(aes(y = Adverts, label = sprintf("%.1f", Adverts_diff)), 
            vjust = ifelse(data$Adverts_diff > 0, -0.5, 1.5), color = "black") +
  geom_text(aes(y = Packets, label = sprintf("%.0f", Packets_diff)), 
            vjust = ifelse(data$Packets_diff > 0, -0.5, 1.5), color = "black") +
  labs(x = "Person", y = "Number of Adverts/Packets") +
  coord_cartesian(clip = "off") +
  annotate("text", x = 5.5, y = mean_adverts + 0.5, label = "Adverts", color = "blue", hjust = 0) +
  annotate("text", x = 5.5, y = mean_packets + 0.5, label = "Packets", color = "gray40", hjust = 0) +
  theme_minimal(base_size = 14) +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

## save plot
ggsave("plot/plot-measuring-relationship-covariance.jpeg",
       width = 10, height = 5, units = "in", dpi = 300)

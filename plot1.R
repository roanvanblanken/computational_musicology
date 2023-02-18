library(tidyverse)
library(spotifyr)
library(ggthemes)

my_faves <- get_playlist_audio_features("", "6pl0C7qbIl5uoY3Tdf82oa")

# Create the scatterplot
ggplot(my_faves, aes(x = liveness, y = energy, color = mode)) +
  geom_point(size = 3) +
  theme_tufte(base_size = 16) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "X-axis label", y = "Y-axis label", title = "Scatterplot Title") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 1),
        panel.background = element_rect(fill = "white"),
        legend.position = "right")

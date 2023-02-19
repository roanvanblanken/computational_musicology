library(tidyverse)
library(ggplot2)
library(spotifyr)
library(ggthemes)

my_faves <- get_playlist_audio_features("", "6pl0C7qbIl5uoY3Tdf82oa")

# Start with modes
my_faves <- my_faves %>%
  mutate(mode = ifelse(mode == 0, "Minor", "Major"))

# Create the scatterplot
my_faves %>%
  ggplot(aes(x = valence, y = energy, size = loudness, color = mode)) +
  geom_point() +
  geom_rug(linewidth = 0.1) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.50, 1), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.50, 1), minor_breaks = NULL) +
  scale_color_brewer(type = "qual", palette = "Paired", name = "Mode") +
  scale_size_continuous(trans = "exp", guide = "none") +
  theme_light() +
  labs(x = "Valence", y = "Energy",
       title = "My Favorite Tracks",
       subtitle = "Plotting valence, energy, and loudness",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken")

# Get playlists
my_faves <- get_playlist_audio_features("", "6pl0C7qbIl5uoY3Tdf82oa")
soft_10s <- get_playlist_audio_features("", "37i9dQZF1DX1uHCeFHcn8X")

# Create the density plot
ggplot() +
  geom_density(aes(x = valence, color = "My Favorites"), data = my_faves, fill = "red", alpha = 0.5) +
  geom_density(aes(x = valence, color = "Soft 10s"), data = soft_10s, fill = "purple", alpha = 0.5) +
  scale_color_manual(values = c("My Favorites" = "red", "Soft 10s" = "purple")) +
  theme_light() +
  labs(x = "Valence", y = "Density",
       title = "Valence Distribution of My Favorite Tracks and Soft 10s",
       subtitle = "Based on Spotify audio features",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken") +
  guides(color = guide_legend(override.aes = list(fill = c("red", "orchid")))) 






library(tidyverse)
library(ggplot2)
library(spotifyr)
library(ggthemes)

# Get playlists
songs_i_like <- get_playlist_audio_features("", "6pl0C7qbIl5uoY3Tdf82oa")
songs_i_dislike <- get_playlist_audio_features("", "4bJQX5w7W4wEnHLmWqUIVY")

# Create the density plot
ggplot() +
  geom_density(aes(x = speechiness, color = "Songs I like"), data = songs_i_like, fill = "lightblue", alpha = 0.5) +
  geom_density(aes(x = speechiness, color = "Songs I dislike"), data = songs_i_dislike, fill = "lightpink", alpha = 0.5) +
  scale_color_manual(values = c("Songs I like" = "lightblue", "Songs I dislike" = "lightpink")) +
  theme_light() +
  labs(x = "Speechiness", y = "Density",
       title = "Speechiness Distribution of My Favorite Tracks and Songs I dislike",
       subtitle = "Based on Spotify audio features",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken") +
  guides(color = guide_legend(override.aes = list(fill = c("lightpink", "lightblue"))))

# Create the scatter plot
ggplot() +
  geom_point(aes(x = instrumentalness, y = energy, color = "Songs I like"), data = songs_i_like, alpha = 0.5) +
  geom_point(aes(x = instrumentalness, y = energy, color = "Songs I dislike"), data = songs_i_dislike, alpha = 0.5) +
  scale_color_manual(values = c("Songs I like" = "green", "Songs I dislike" = "orange")) +
  theme_light() +
  labs(x = "Instrumentalness", y = "Energy",
       title = "Instrumentalness and Energy of My Favorite Tracks and Songs I dislike",
       subtitle = "Based on Spotify audio features",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken")


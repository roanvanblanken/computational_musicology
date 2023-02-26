library(tidyverse)
library(ggplot2)
library(spotifyr)
library(ggthemes)

# Get playlists
songs_i_like <- get_playlist_audio_features("", "6pl0C7qbIl5uoY3Tdf82oa")

songs_i_like <- songs_i_like %>%
  mutate(mode = ifelse(mode == 0, "Minor", "Major"))

songs_i_dislike <- get_playlist_audio_features("", "4bJQX5w7W4wEnHLmWqUIVY")

songs_i_dislike <- songs_i_dislike %>%
  mutate(mode = ifelse(mode == 0, "Minor", "Major"))

# Create a scatterplot
songs_i_like %>%
  ggplot(aes(x = valence, y = energy, size = loudness, color = mode)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.50, 1), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.50, 1), minor_breaks = NULL) +
  scale_color_manual(values = c("darkblue", "lightblue"), name = "Mode", labels = c("Minor", "Major")) +
  scale_size_continuous(trans = "exp", range = c(1, 10), guide = guide_legend(override.aes = list(size = c(1, 3, 5)))) +
  theme_light() +
  labs(x = "Valence", y = "Energy",
       title = "Songs I like",
       subtitle = "Plotting valence, energy, and loudness",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken")

# Create a scatterplot
songs_i_dislike %>%
  ggplot(aes(x = valence, y = energy, size = loudness, color = mode)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.50, 1), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.50, 1), minor_breaks = NULL) +
  scale_color_manual(values = c("darkred", "lightcoral")) +
  scale_size_continuous(trans = "exp", range = c(1, 10), guide = guide_legend(override.aes = list(size = c(1, 3, 5, 7)))) +
  theme_light() +
  labs(x = "Valence", y = "Energy",
       title = "Songs I dislike",
       subtitle = "Plotting valence, energy, loudness, and mode",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken")

ggplot() +
  geom_density(aes(x = valence, fill = "Songs I like"), data = songs_i_like, alpha = 0.5) +
  geom_density(aes(x = valence, fill = "Songs I dislike"), data = songs_i_dislike, alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), name = "") +
  theme_light() +
  labs(x = "Valence", y = "Density",
       title = "Valence Distribution",
       subtitle = "Comparison of Songs I like and Songs I dislike",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)))

ggplot() +
  geom_density(aes(x = speechiness, fill = "Songs I like"), data = songs_i_like, alpha = 0.5) +
  geom_density(aes(x = speechiness, fill = "Songs I dislike"), data = songs_i_dislike, alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), name = "") +
  theme_light() +
  labs(x = "Speechiness", y = "Density",
       title = "Speechiness Distribution",
       subtitle = "Comparison of Songs I like and Songs I dislike",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)))

ggplot() +
  geom_density(aes(x = energy, fill = "Songs I like"), data = songs_i_like, alpha = 0.5) +
  geom_density(aes(x = energy, fill = "Songs I dislike"), data = songs_i_dislike, alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), name = "") +
  theme_light() +
  labs(x = "Energy", y = "Density",
       title = "Energy Distribution",
       subtitle = "Comparison of Songs I like and Songs I dislike",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)))

### Plot 3

# Get playlists
songs_i_like <- get_playlist_audio_features("", "6pl0C7qbIl5uoY3Tdf82oa")
songs_i_dislike <- get_playlist_audio_features("", "4bJQX5w7W4wEnHLmWqUIVY")

# Create the density plot
plot1 <- ggplot() +
  geom_density(aes(x = speechiness, color = "Songs I like"), data = songs_i_like, fill = "lightblue", alpha = 0.5) +
  geom_density(aes(x = speechiness, color = "Songs I dislike"), data = songs_i_dislike, fill = "lightpink", alpha = 0.5) +
  scale_color_manual(values = c("Songs I like" = "lightblue", "Songs I dislike" = "lightpink")) +
  theme_light() +
  labs(x = "Speechiness", y = "Density",
       title = "Speechiness Distribution of My Favorite Tracks and Songs I dislike",
       subtitle = "Based on Spotify audio features",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken") +
  guides(color = guide_legend(override.aes = list(fill = c("lightpink", "lightblue"))))

plotly_object1 <- ggplotly(plot1)

plot1

### Plot 4

# Create the scatter plot
plot2 <- ggplot() +
  geom_point(aes(x = instrumentalness, y = energy, color = "Songs I like"), data = songs_i_like, alpha = 0.5) +
  geom_point(aes(x = instrumentalness, y = energy, color = "Songs I dislike"), data = songs_i_dislike, alpha = 0.5) +
  scale_color_manual(values = c("Songs I like" = "green", "Songs I dislike" = "orange")) +
  theme_light() +
  labs(x = "Instrumentalness", y = "Energy",
       title = "Instrumentalness and Energy of My Favorite Tracks and Songs I dislike",
       subtitle = "Based on Spotify audio features",
       caption = "Data source: Spotify API\nAuthor: Roan van Blanken")

plotly_object2 <- ggplotly(plot2)

plot2


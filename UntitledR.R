library(tidyverse)
library(spotifyr)

i_love_rap <- get_playlist_audio_features("", "0TNXzGZM9uRpATDNRlwFCa")
i_love <- get_playlist_audio_features("", "6VFb76emoLgG0AnTyAA5yB")
havent_i_given_enough <- get_playlist_audio_features("", "36js5VTgRtBlfbM61RkKYR")

i_love_rap <- i_love_rap %>%
  mutate(mode = ifelse(mode == 0, "Minor", "Major")) %>%
  mutate(playlist = 'i love rap')

i_love <- i_love %>%
  mutate(mode = ifelse(mode == 0, "Minor", "Major")) %>%
  mutate(playlist = 'i love ♡')

havent_i_given_enough <- havent_i_given_enough %>%
  mutate(mode = ifelse(mode == 0, "Minor", "Major")) %>%
  mutate(playlist = 'havent i given enough, given enough?')

# Combine playlists
combined_playlist <- rbind(i_love_rap, i_love, havent_i_given_enough)


combined_playlist %>%      
  ggplot(aes(x = valence, y = energy, size = loudness, color = playlist, shape = mode)) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),    # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    minor_breaks = NULL
  ) +
  scale_color_brewer(         # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Paired"        # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp"             # Use an exp transformation to emphasise loud.
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    color = "Playlist",
    shape = "Mode"
  )

combined_playlist %>%      
  ggplot(aes(x = liveness, y = danceability, size = loudness, color = playlist, shape = mode)) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),    # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    minor_breaks = NULL
  ) +
  scale_color_brewer(         # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Paired"        # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp"             # Use an exp transformation to emphasise loud.
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Danceability",
    y = "Liveness",
    color = "Playlist",
    shape = "Mode"
  )

combined_playlist %>%      
  ggplot(aes(x = instrumentalness, y = speechiness, color = playlist, shape = mode)) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),    # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    minor_breaks = NULL
  ) +
  scale_color_brewer(         # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Paired"        # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp"             # Use an exp transformation to emphasise loud.
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Instrumentalness",
    y = "Speechiness",
    color = "Playlist",
    shape = "Mode"
  )

combined_playlist %>%      
  ggplot(aes(x = mode, fill = playlist)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  labs(
    x = "Mode",
    y = "Count",
    fill = "Playlist"
  ) +
  theme_light()

combined_playlist %>%      
  ggplot(aes(x = mode, y = speechiness, fill = playlist)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", color = "black", size = 0.5) +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  labs(
    x = "Mode",
    y = "Speechiness (Mean)",
    fill = "Playlist"
  ) +
  theme_light()







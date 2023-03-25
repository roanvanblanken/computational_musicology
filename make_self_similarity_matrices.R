library(spotifyr)
library(ggplot2)
library(tidyverse)
library(compmus)
library(plotly)

shutupanddance_timbre <-
  get_tidy_audio_analysis("0kzw2tRyuL9rzipi5ntlIy") |> # Change URI.
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  compmus_self_similarity(timbre, "cosine")

saveRDS(shutupanddance_timbre, file="data/Self-similarity Matrix (shutupanddance_timbre).Rda")

# Define a custom color palette
blues_purples <- colorRampPalette(c("#2c7bb6", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"))(100)
blue_red <- colorRampPalette(c("#00008B", "#ADD8E6", "#E6FFFF", "#FFE4E1", "#FF6347", "#8B0000"))(100)

ssm1 <- state_of_unrest_chroma |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  ggtitle("Self-similarity Matrix (Chroma) \nLamb of God, Kreator: 'State of Unrest'") +
  coord_fixed() +
  scale_fill_gradientn(colors = blue_red) +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_color_gradientn(colors = blue_red)

ggplotly(ssm1, tooltip = "none", source = "none")

ssm2 <- state_of_unrest_timbre |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  ggtitle("Self-similarity Matrix (Timbre) \nLamb of God, Kreator: 'State of Unrest'") +
  coord_fixed() +
  scale_fill_gradientn(colors = blue_red) +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_color_gradientn(colors = blue_red)

ggplotly(ssm2, tooltip = "none", source = "none")

ssm3 <- shutupanddance_chroma |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  ggtitle("Self-similarity Matrix (Chroma) \nWALK THE MOON: 'Shut up and Dance'") +
  coord_fixed() +
  scale_fill_gradientn(colors = blue_red) +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_color_gradientn(colors = blue_red)

ggplotly(ssm3, tooltip = "none", source = "none")

ssm4 <- shutupanddance_timbre |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  ggtitle("Self-similarity Matrix (Timbre) \nWALK THE MOON: 'Shut up and Dance'") +
  coord_fixed() +
  scale_fill_gradientn(colors = blue_red) +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_color_gradientn(colors = blue_red)

ggplotly(ssm4, tooltip = "none", source = "none")

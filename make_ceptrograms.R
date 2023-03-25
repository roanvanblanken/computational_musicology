library(spotifyr)
library(ggplot2)
library(tidyverse)
library(compmus)

state_of_unrest <-
  get_tidy_audio_analysis("3u4djE2yAEkKMWJEUOOJyT") |> # Change URI.
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      # in all three
  unnest(bars) |>                                      # of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"           # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  compmus_gather_timbre()

saveRDS(state_of_unrest, file="data/Ceptrogram (state_of_unrest).Rda")

# Define a custom color palette
blues_purples <- colorRampPalette(c("#2c7bb6", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"))(100)

ceptrogram1 <- state_of_unrest |>
  ggplot(aes(x = start + duration / 2, width = duration, y = basis, fill = value)) +
  geom_tile() +
  ggtitle("Ceptrogram - Lamb of God, Kreator: 'State of Unrest'") +
  scale_fill_gradientn(colors = blues_purples) +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_color_gradientn(colors = blues_purples)

ggplotly(ceptrogram1, tooltip = "none", source = "none")

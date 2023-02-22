library(spotifyr)
library(compmus)
library(tidyverse)

## The Tallis Scholars
tallis <-
  get_tidy_audio_analysis("2J3Mmybwue0jyQ0UVMYurH") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

## La Chapelle Royale
chapelle <-
  get_tidy_audio_analysis("4ccw2IcnFt1Jv9LqQCOYDi") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

## The Cambridge Singers
cambridge <-
  get_tidy_audio_analysis("54cAT1TCFaZbLOB2i1y61h") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

## Oxford Camerata
oxford <-
  get_tidy_audio_analysis("5QyUsMY40MQ1VebZXSaonU") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

## Chanticleer
chanticleer <-
  get_tidy_audio_analysis("1bocG1N8LM7MSgj9T1n3XH") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

## The Hilliard Ensemble
hilliard <-
  get_tidy_audio_analysis("2rXEyq50luqaFNC9DkcU6k") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

## The Gabrieli Consort
gabrieli <-
  get_tidy_audio_analysis("4NnJ4Jes8a8mQUfXhwuITx") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

compmus_long_distance(
  tallis |> mutate(pitches = map(pitches, compmus_normalise, "chebyshev")),
  chapelle |> mutate(pitches = map(pitches, compmus_normalise, "chebyshev")),
  feature = pitches,
  method = "euclidean"
) |>
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
  coord_equal() +
  labs(x = "The Tallis Scholars", y = "La Chapelle Royale") +
  theme_minimal() +
  scale_fill_viridis_c(guide = NULL)

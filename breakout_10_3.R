library(spotifyr)
library(purrr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(compmus)

sil <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "6pl0C7qbIl5uoY3Tdf82oa"
  ) |>
  add_audio_analysis()
sid <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "4bJQX5w7W4wEnHLmWqUIVY"
  ) |>
  add_audio_analysis()
corpus <-
  sil |>
  mutate(Playlist = "Songs I like") |>
  bind_rows(sid |> mutate(Playlist = "Songs I dislike"))

corpus2 <- corpus |>
  mutate(
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, loudness, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) |>
  unnest(sections)

saveRDS(corpus2, file="corpus.Rda")

corpus2 |>
  ggplot(
  aes(
    x = tempo,
    y = tempo_section_sd,
    colour = Playlist,
    alpha = loudness
  )
) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Playlist",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  )


corpus |>
  mutate(
    timbre =
      map(
        segments,
        compmus_summarise,
        timbre,
        method = "mean"
      )
  ) |>
  select(Playlist, timbre) |>
  compmus_gather_timbre() |>
  ggplot(aes(x = basis, y = value, fill = Playlist)) +
  geom_violin() +
  scale_fill_viridis_d() +
  labs(x = "Spotify Timbre Coefficients", y = "", fill = "Playlist")

cdata <- corpus %>%
  mutate(
    timbre =
      map(
        segments,
        compmus_summarise,
        timbre,
        method = "mean"
      )
  ) %>%
  select(Playlist, timbre, track.name) %>%
  compmus_gather_timbre()

cdata %>%
  ggplot(aes(x = factor(basis), y = value, fill = Playlist)) +
  geom_violin() +
  scale_fill_viridis_d() +
  geom_text(data = . %>% filter(Playlist == "Songs I like" & basis == 'c02') %>% filter(value == min(value)), aes(label = track.name), hjust = -0.1, vjust = -0.95, color = "darkgoldenrod", size = 4, fontface = "bold") +
  geom_text(data = . %>% filter(Playlist == "Songs I like" & basis == 'c02') %>% filter(value == max(value)), aes(label = track.name), hjust = -0.15, vjust = 0., color = "darkgoldenrod", size = 4, fontface = "bold") +
  geom_text(data = . %>% filter(Playlist == "Songs I dislike" & basis == 'c02') %>% filter(value == min(value)), aes(label = track.name), hjust = 0, vjust = 0.4, color = "darkviolet", size = 4, fontface = "bold") +
  geom_text(data = . %>% filter(Playlist == "Songs I dislike" & basis == 'c02') %>% filter(value == max(value)), aes(label = track.name), hjust = 0, vjust = 0.5, color = "darkviolet", size = 4, fontface = "bold") +
  labs(x = "Spotify Timbre Coefficients", y = "", fill = "Playlist")

saveRDS(cdata, file="timbre_data.Rda")

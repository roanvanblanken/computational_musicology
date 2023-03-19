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

library(tidyr)
library(dplyr)
library(stringr)

cdata <- corpus %>%
  mutate(
    timbre = map(
      segments,
      compmus_summarise,
      timbre,
      method = "mean"
    )
  ) %>%
  select(Playlist, timbre, track.name, track.artists) %>%
  compmus_gather_timbre()

timbre_data <- cdata %>%
  unnest(track.artists) %>%
  mutate(track.artists = ifelse(name == "name", NA_character_, name)) %>%
  group_by(track.name) %>%
  summarise(artists = str_c(unique(na.omit(track.artists)), collapse = ", ")) %>%
  right_join(cdata, by = "track.name", multiple = "all") %>%
  select(-track.artists)


timbre_data %>%
  ggplot(aes(x = factor(basis), y = value, fill = Playlist)) +
  geom_violin() +
  scale_fill_viridis_d() +
  geom_text(data = . %>% filter(Playlist == "Songs I like" & basis == 'c02') %>% filter(value == min(value)), aes(label = track.name), hjust = -0.1, vjust = -0.95, color = "darkgoldenrod", size = 4, fontface = "bold") +
  geom_text(data = . %>% filter(Playlist == "Songs I like" & basis == 'c02') %>% filter(value == max(value)), aes(label = track.name), hjust = -0.15, vjust = 0., color = "darkgoldenrod", size = 4, fontface = "bold") +
  geom_text(data = . %>% filter(Playlist == "Songs I dislike" & basis == 'c02') %>% filter(value == min(value)), aes(label = track.name), hjust = 0, vjust = 0.4, color = "darkviolet", size = 4, fontface = "bold") +
  geom_text(data = . %>% filter(Playlist == "Songs I dislike" & basis == 'c02') %>% filter(value == max(value)), aes(label = track.name), hjust = 0, vjust = 0.5, color = "darkviolet", size = 4, fontface = "bold") +
  labs(x = "Spotify Timbre Coefficients", y = "", fill = "Playlist")

saveRDS(timbre_data, file="timbre_data.Rda")

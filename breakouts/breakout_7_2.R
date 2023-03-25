library(tidyverse)
library(spotifyr)

juditha <- get_track_audio_features(c("2M5b9YLAgFroqWzeaZf86e", "3DBKc4ioGnMQLlbGQcFDIO"))
alla <- get_album_tracks("7oI0E3DdTbD85rhMg19GSU")
gilberto <- get_artist_audio_features("gilberto gil")
ecm <- get_playlist_audio_features("", "1zN4nK6oHMo2dei2WWPtSL")

ecm |>
  summarise(
    mean_speechiness = mean(speechiness),
    mean_acousticness = mean(acousticness),
    mean_liveness = mean(liveness),
    sd_speechiness = sd(speechiness),
    sd_acousticness = sd(acousticness),
    sd_liveness = sd(liveness),
    median_speechiness = median(speechiness),
    median_acousticness = median(acousticness),
    median_liveness = median(liveness),
    mad_speechiness = mad(speechiness),
    mad_acousticness = mad(acousticness),
    mad_liveness = mad(liveness)
  )

disney <- get_playlist_audio_features("", "37i9dQZF1DX8C9xQcOrE6T?si=brcY2gRMTEeLA73yteEw_A&nd=1")

disney |>
  summarise(
    mean_speechiness = mean(speechiness),
    mean_acousticness = mean(acousticness),
    mean_liveness = mean(liveness),
    sd_speechiness = sd(speechiness),
    sd_acousticness = sd(acousticness),
    sd_liveness = sd(liveness),
    median_speechiness = median(speechiness),
    median_acousticness = median(acousticness),
    median_liveness = median(liveness),
    mad_speechiness = mad(speechiness),
    mad_acousticness = mad(acousticness),
    mad_liveness = mad(liveness)
  )

disney_filtered <- filter(disney, track.name %in% c("Let It Go - From \"Frozen\"/Soundtrack Version", "We Don't Talk About Bruno"))


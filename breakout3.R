library(tidyverse)
library(spotifyr)

grammy <- get_playlist_audio_features("", "4kQovkgBZTd8h2HCM3fF31")
edison <- get_playlist_audio_features("", "37i9dQZF1DX8mnKbIkppDf")

awards <-
  bind_rows(
    grammy |> mutate(category = "Grammys"),
    edison |> mutate(category = "Edisons")
  )

grammy |> ggplot(aes(x = energy)) + geom_histogram(binwidth = 0.1)

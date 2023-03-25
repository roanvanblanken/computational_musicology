library(spotifyr)
library(ggplot2)
library(tidyverse)
library(compmus)

wilay <- get_tidy_audio_analysis("4ebcE2SmkG7nplvzFAWRu7")

wilay_normal <- wilay |>
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE)

wilay_cyclic <- wilay |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE)

saveRDS(wilay_normal, file="data/Tempogram (wilay_normal).Rda")
saveRDS(wilay_cyclic, file="data/Tempogram (wilay_cyclic).Rda")

wilay_normal |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  ggtitle("Normal") +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

wilay_cyclic |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  ggtitle("Cyclic") +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

starstruck <- get_tidy_audio_analysis("2WJVFqVQ3ivhAoAQWzEzeL")

starstruck_normal <- starstruck |>
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE)

starstruck_cyclic <- starstruck |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE)

saveRDS(starstruck_normal, file="data/Tempogram (starstruck_normal).Rda")
saveRDS(starstruck_cyclic, file="data/Tempogram (starstruck_cyclic).Rda")

starstruck_normal |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  ggtitle("Normal") +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

starstruck_cyclic |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  ggtitle("Cyclic") +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

africa <- get_tidy_audio_analysis("2374M0fQpWi3dLnB54qaLX")

africa_normal <- africa |>
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE)

africa_cyclic <- africa |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE)

saveRDS(africa_normal, file="data/Tempogram (africa_normal).Rda")
saveRDS(africa_cyclic, file="data/Tempogram (africa_cyclic).Rda")

africa_normal |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  ggtitle("Normal") +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

africa_cyclic |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  ggtitle("Cyclic") +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

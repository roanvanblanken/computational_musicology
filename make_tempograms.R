library(spotifyr)
library(ggplot2)
library(tidyverse)
library(compmus)



high_hopes <- get_tidy_audio_analysis("1rqqCSm0Qe4I9rUvWncaom")

high_hopes_normal <- high_hopes |>
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE)

high_hopes_cyclic <- high_hopes |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE)

saveRDS(high_hopes_normal, file="data/Tempogram (high_hopes_normal).Rda")
saveRDS(high_hopes_cyclic, file="data/Tempogram (high_hopes_cyclic).Rda")

high_hopes_normal |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  ggtitle("Normal") +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

high_hopes_cyclic |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  ggtitle("Cyclic") +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

fascination <- get_tidy_audio_analysis("5AKQ1JHezaXDmN5SyMSpEr")

fascination_normal <- fascination |>
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE)

fascination_cyclic <- fascination |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE)

saveRDS(fascination_normal, file="data/Tempogram (fascination_normal).Rda")
saveRDS(fascination_cyclic, file="data/Tempogram (fascination_cyclic).Rda")

fascination_normal |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  ggtitle("Normal") +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

fascination_cyclic |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  ggtitle("Cyclic") +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

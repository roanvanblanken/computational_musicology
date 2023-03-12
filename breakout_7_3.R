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

awards |>
  ggplot(aes(x = energy)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~category)

awards |>
  ggplot(aes(x = category, y = energy)) +
  geom_boxplot()

awards |>                    # Start with awards.
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  ) |>
  ggplot(                     # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = loudness,
      colour = mode
    )
  ) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
  geom_text(                  # Add text labels from above.
    aes(
      x = valence,
      y = energy,
      label = label
    ),
    data = 
      tibble(
        label = c("Altijd wel iemand", "ENERGY"),
        category = c("Edisons", "Grammys"),
        valence = c(0.090, 0.123),
        energy = c(0.101, 0.967)
      ),
    colour = "black",         # Override colour (not mode here).
    size = 3,                 # Override size (not loudness here).
    hjust = "left",           # Align left side of label with the point.
    vjust = "bottom",         # Align bottom of label with the point.
    nudge_x = -0.05,          # Nudge the label slightly left.
    nudge_y = 0.02            # Nudge the label slightly up.
  ) +
  facet_wrap(~ category) +    # Separate charts per playlist.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(        # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "Paired"        # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp",            # Use an exp transformation to emphasise loud.
    guide = "none"            # Remove the legend for size.
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    colour = "Mode"
  )


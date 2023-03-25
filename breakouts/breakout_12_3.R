library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(compmus)

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
}  

pop <- get_playlist_audio_features("spotify", "37i9dQZF1DWWEcRhUVtL8n")
party <- get_playlist_audio_features("spotify", "37i9dQZF1DWTujiC7wfofZ")
workout <- get_playlist_audio_features("spotify", "37i9dQZF1DWZq91oLsHZvy")

indie <-
  bind_rows(
    pop |> mutate(playlist = "Indie Pop") |> slice_head(n = 20),
    party |> mutate(playlist = "Indie Party") |> slice_head(n = 20),
    workout |> mutate(playlist = "Indie Running") |> slice_head(n = 20)
  ) |> 
  add_audio_analysis()

indie_features <-
  indie |>  # For your portfolio, change this to the name of your corpus.
  mutate(
    playlist = factor(playlist),
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(
        segments,
        compmus_summarise, pitches,
        method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean",
      )
  ) |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  mutate_at(vars(pitches, timbre), map, bind_rows) |>
  unnest(cols = c(pitches, timbre))

indie_recipe <-
  recipe(
    playlist ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      duration +
      C + `C#|Db` + D + `D#|Eb` +
      E + `F` + `F#|Gb` + G +
      `G#|Ab` + A + `A#|Bb` + B +
      c01 + c02 + c03 + c04 + c05 + c06 +
      c07 + c08 + c09 + c10 + c11 + c12,
    data = indie_features           # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

indie_cv <- indie_features |> vfold_cv(5)

knn_model <-
  nearest_neighbor(neighbors = 1) |>
  set_mode("classification") |> 
  set_engine("kknn")

indie_knn <- 
  workflow() |> 
  add_recipe(indie_recipe) |> 
  add_model(knn_model) |> 
  fit_resamples(indie_cv, control = control_resamples(save_pred = TRUE))

indie_knn |> get_conf_mat()

indie_knn |> get_conf_mat() |> autoplot(type = "mosaic")

indie_knn |> get_conf_mat() |> autoplot(type = "heatmap")

indie_knn |> get_pr()

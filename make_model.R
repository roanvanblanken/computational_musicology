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

# Get playlists
songs_i_like <- get_playlist_audio_features("", "6pl0C7qbIl5uoY3Tdf82oa")

songs_i_like <- songs_i_like %>%
  mutate(mode = ifelse(mode == 0, "Minor", "Major"))

songs_i_dislike <- get_playlist_audio_features("", "4bJQX5w7W4wEnHLmWqUIVY")

songs_i_dislike <- songs_i_dislike %>%
  mutate(mode = ifelse(mode == 0, "Minor", "Major"))

new_music_friday_sil <- get_playlist_audio_features("", "5w3YQiVAfcfCIhQ1wnWael") %>%
  mutate(mode = ifelse(mode == 0, "Minor", "Major"))

new_music_friday_sid <- get_playlist_audio_features("", "307LwouWuqsCBAwq9FYmYU") %>%
  mutate(mode = ifelse(mode == 0, "Minor", "Major"))

new_music_friday <-
  bind_rows(
    songs_i_like |> mutate(playlist = "Songs I like"),
    songs_i_dislike |> mutate(playlist = "Song I dislike")) |> 
  add_audio_analysis() 

saveRDS(new_music_friday, file="data/Model (new_music_friday).Rda")

new_music_friday_features <-
  model |>  # For your portfolio, change this to the name of your corpus.
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

saveRDS(new_music_friday_features, file="data/Model (new_music_friday_features).Rda")

model <-
  bind_rows(
    songs_i_like |> mutate(playlist = "Songs I like"),
    songs_i_dislike |> mutate(playlist = "Song I dislike")) |> 
  add_audio_analysis()

model_features <-
  model |>  # For your portfolio, change this to the name of your corpus.
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

saveRDS(model_features, file="data/Model (model_features).Rda")

corpus_recipe <-
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
      duration,
    data = model_features           # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

corpus_cv <- model_features |> vfold_cv(5)

knn_model <-
  nearest_neighbor(neighbors = 1) |>
  set_mode("classification") |> 
  set_engine("kknn")

corpus_knn <- 
  workflow() |> 
  add_recipe(corpus_recipe) |> 
  add_model(knn_model) |> 
  fit_resamples(corpus_cv, control = control_resamples(save_pred = TRUE))

corpus_knn |> get_conf_mat()

corpus_knn |> get_conf_mat() |> autoplot(type = "mosaic")

corpus_knn |> get_conf_mat() |> autoplot(type = "heatmap")

corpus_knn |> get_pr()

# Create a decision tree model
decision_tree_model <-
  decision_tree(tree_depth = 3, min_n = 25) |> 
  set_mode("classification") |> 
  set_engine("rpart")

# Fit the decision tree model with cross-validation
corpus_decision_tree <- 
  workflow() |> 
  add_recipe(corpus_recipe) |> 
  add_model(decision_tree_model) |> 
  fit_resamples(corpus_cv, control = control_resamples(save_pred = TRUE))

# Get confusion matrix
corpus_decision_tree |> get_conf_mat()

# Plot confusion matrix as mosaic plot
corpus_decision_tree |> get_conf_mat() |> autoplot(type = "mosaic")

# Plot confusion matrix as heatmap
corpus_decision_tree |> get_conf_mat() |> autoplot(type = "heatmap")

# Get precision and recall
corpus_decision_tree |> get_pr()

# Fit the decision tree model without cross-validation
corpus_decision_tree_no_cv <- 
  workflow() |> 
  add_recipe(corpus_recipe) |> 
  add_model(decision_tree_model) |> 
  fit(data = model_features)

# Extract the rpart object
rpart_obj <- corpus_decision_tree_no_cv %>% pull_workflow_fit() %>% pluck("fit")

# Plot the decision tree
rpart.plot(rpart_obj, type = 1, extra = 101, cex = 0.8, box.palette = "auto", tweak = 1.2, roundint = FALSE)

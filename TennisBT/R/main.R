library(tidyr)
library(readr)
library(dplyr)
library(BradleyTerry2)
library(tidyverse)

TennisTidyr <- function(startyear, endyear, testset, numgames) {
  # Adding errors for if years will not match the dataset
  if (!(startyear > 2012 & startyear < 2023)) {
    stop("Error: Start year must be between 2013 and 2022")
  }
  if (!(endyear > 2012 & endyear < 2023)) {
    stop("Error: End year must be between 2013 and 2022")
  }
  if (!(testset > 2012 & testset < 2024)) {
    stop("Error: Test set must be between 2013 and 2023")
  }
  if (startyear > endyear) {
    stop("Error: Start year must be less than end year")
  }
  if (endyear > testset) {
    stop("Error: End year must be less than test set")
  }

  datasets <- list("2013" = wta_matches_2013, "2014" = wta_matches_2014,
                   "2015" = wta_matches_2015, "2016" = wta_matches_2016,
                   "2017" = wta_matches_2017, "2018" = wta_matches_2018,
                   "2019" = wta_matches_2019, "2020" = wta_matches_2020,
                   "2021" = wta_matches_2021, "2022" = wta_matches_2022,
                   "2023" = wta_matches_2023)

  #Creating blank dataset for training set, then appending each year
  train <- tibble()
  num_names <- c()

  for (i in startyear:endyear) {
    data <- as.data.frame(datasets[i-2012])
    colnames(data) <- colnames(wta_matches_2013)
    num_names <- append(num_names, c(as.matrix(data[, "winner_name"]),
                                     as.matrix(data[, "loser_name"])))
    train <- rbind(train, data)
  }
  #List of all unique names in that time period
  names <- unique(num_names)
  #Number of games played per player
  n_names <- length(names)
  num_games <- rep(0, n_names)
  for (i in 1:n_names){
    num_games[i] <- length(which(num_names == names[i]))
  }
  #Players who played in the test set (2023)
  test_set <- as.data.frame(datasets[testset-2012])
  colnames(test_set) <- colnames(wta_matches_2013)
  test_names <- c(as.matrix(test_set[, "winner_name"]),
                  as.matrix(test_set[, "loser_name"]))

  #List of 'significant' players
  main_names <- names[num_games > numgames & names %in% test_names]
  #Only selecting columns we currently care about
  train <- train[, c("winner_id", "winner_name", "winner_hand", "winner_ht", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon",
                     "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced", "loser_id", "loser_name", "loser_hand", "loser_ht",
                     "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved",
                     "l_bpFaced", "surface","winner_rank_points","loser_rank_points","tourney_date")]
  test <- test_set[,c("winner_id", "winner_name", "winner_hand", "winner_ht", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon",
                      "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced", "loser_id", "loser_name", "loser_hand", "loser_ht",
                      "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved",
                      "l_bpFaced", "surface","winner_rank_points","loser_rank_points","tourney_date")]

  test

  #Only selecting players we care about
  train <- train[train$winner_name %in% main_names, ]
  train <- train[train$loser_name %in% main_names, ]
  test <- test[test$winner_name %in% main_names, ]
  test <- test[test$loser_name %in% main_names, ]
  return(list(train=train, test=test, main_names=main_names))
}


time_weighting <- function(t, weight) {
  return(min(weight, weight^t))
}


get_recency_weights <- function(train_set, recency_weighting) {
  tryCatch(
    {
      train_set$tourney_date
    },
    error = function(cond) {
      message("train_set does not contain a 'tourney_date' column.")
      return(NA)
    }
  )

  tourney_year = sapply(train_set$tourney_date, function(x) {as.numeric(substr(x, 1, 4))})
  time <- 2023-tourney_year
  train_set$time <- time
  weights <- sapply(train_set$time, function(x) time_weighting(x, recency_weighting))
  return(weights)
}


predict <- function(player1, player2, df_coeff) {
  lambda1 <- df_coeff[player1,1]
  lambda2 <- df_coeff[player2,1]
  if (is.na(lambda1)) {
    cat(player1, " ", player2, "\n")
    cat(lambda1, " ", lambda2, "\n")
  }
  pred <- exp(lambda1) / (exp(lambda1) + exp(lambda2))
  return(c(pred>0.5, pred))
}


predict_matches <- function(test_set, names, model) {
  df_coeff <- as.data.frame(BTabilities(model))

  # make predictions
  draws <- test_set[,c("winner_name","loser_name")]
  draws <- draws[draws$winner_name %in% names & draws$loser_name %in% names, ]
  draws$pred <- NA

  for (i in 1:nrow(draws)) {
    player1 <- draws$winner_name[i]
    player2 <- draws$loser_name[i]
    pred <- predict(player1, player2, df_coeff)
    draws$pred[i] <- pred[2]
  }

  return(draws)
}


standard_error <- function(x) sd(x) / sqrt(length(x))


score_predictions <- function(predictions) {
  # accuracy
  predictions$correct <- predictions$pred > 0.5
  num <-nrow(filter(predictions, predictions$correct))
  accuracy <- num / nrow(predictions)

  # avg probability
  winner_prob <- predictions[predictions$pred > 0.5,]$pred
  avg_probability <- mean(winner_prob)
  avg_prob_se <- standard_error(winner_prob)

  #avg log probability
  avg_log_probability <- mean(log(winner_prob))
  avg_log_prob_se <- standard_error(log(winner_prob))

  return(list(accuracy = accuracy, avg_probability = avg_probability,
              avg_prob_se = avg_prob_se,
              avg_log_probability = avg_log_probability,
              abg_log_prob_se = avg_log_prob_se))
}

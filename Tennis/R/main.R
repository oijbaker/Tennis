library(tidyr)
library(readr)
library(BradleyTerry2)

# Base functions for library

getWinLossBinomial <- function(startyear, endyear, testset, numgames, filename) {
    # Get win loss data in binomial format
    # startyear: first year to include
    # endyear: last year to include
    # numgames: minimum number of games played by each player

    data <- TennisTidyr(startyear, endyear, testset, numgames, filename)
    train <- data$train
    test <- data$test
    main_names <- data$main_names

    N <- length(main_names)

    winmat <- matrix(data = 0, ncol = N, nrow = N,
                     dimnames = list(main_names, main_names))

    M <- dim(train)[1]

    for (i in 1:M){
      winner <- train$winner_name[i]
      loser <- train$loser_name[i]
      winmat[winner, loser] <- winmat[winner, loser] + 1
    }

    # Get win loss data in binomial format
    wl_bin <- countsToBinomial(winmat)

    return(wl_bin)
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

countsToBinomial <- function(xtab){

  # Throw an error if xtab is not square
  if (nrow(xtab) != ncol(xtab)) stop("xtab must be square")

  players <- rownames(xtab)
  comb <- t(combn(nrow(xtab), 2))
  won <- xtab[comb]
  lost <- t(xtab)[comb]
  res <- !(won == 0 & lost == 0)
  player1 <- factor(players[comb[,1]], levels = players)[res]
  player2 <- factor(players[comb[,2]], levels = players)[res]
  data.frame(player1, player2, win1 = won[res], win2 = lost[res])
}

TennisTidyr <- function(startyear, endyear, testset, numgames, filename) {
  # Adding errors for if years will not match the dataset
  if (!(startyear > 1967 & startyear < 2023)) {
    stop("Error: Start year must be between 1968 and 2022")
  }
  if (!(endyear > 1967 & endyear < 2023)) {
    stop("Error: End year must be between 1968 and 2022")
  }
  if (!(testset > 1967 & testset < 2024)) {
    stop("Error: Test set must be between 1968 and 2023")
  }
  if (startyear > endyear) {
    stop("Error: Start year must be less than end year")
  }
  if (endyear > testset) {
    stop("Error: End year must be less than test set")
  }
  #Creating blank dataset for training set, then appending each year
  train <- tibble()
  num_names <- c()

  for (i in startyear:endyear){
    data <- read_csv(paste(filename, as.character(i), ".csv", sep = ""),
                     show_col_types = FALSE)
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
  test_set <- read_csv(paste(filename, as.character(testset), ".csv", sep = ""),
                       show_col_types = FALSE)
  test_names <- c(as.matrix(test_set[, "winner_name"]),
                  as.matrix(test_set[, "loser_name"]))

  #List of 'significant' players
  main_names <- names[num_games > numgames & names %in% test_names]
  #Only selecting columns we currently care about
  train <- train[, c('surface', 'winner_name', 'winner_hand', 'winner_ht',
                     'winner_age', 'loser_name', 'loser_hand', 'loser_ht',
                     'loser_age')]
  test <- test_set[,c('surface', 'winner_name', 'winner_hand', 'winner_ht',
                      'winner_age', 'loser_name', 'loser_hand', 'loser_ht',
                      'loser_age')]

  #Only selecting players we care about
  train <- train[train$winner_name %in% main_names, ]
  train <- train[train$loser_name %in% main_names, ]
  test <- test[test$winner_name %in% main_names, ]
  test <- test[test$loser_name %in% main_names, ]
  return(list(train=train, test=test, main_names=main_names))
}

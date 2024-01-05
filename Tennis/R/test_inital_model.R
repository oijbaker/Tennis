library(tidyr)
library(dplyr)
library(ggplot2)
library(BradleyTerry2)
library(readr)
library(here)

source(here::here("Tennis/R/main.R"))

train_matrices <- list()

start <- 2013
end <- 2023

# Read in data
for (start_year in start:(end-1)) {
    wl_matrix = getWinLossBinomial(start_year, end-1, end,
                                   20, here("data/wta_matches_"))
    train_matrices[[start_year]] = wl_matrix

    if (start_year == start) {
      players <- as.vector(unique(wl_matrix$player1))
    }
}

pred_df <- read_csv(here("data/wta_matches_2023.csv"))

# For each year
for (start_year in 2013:2022) {
    # Get the training matrix
    train_matrix = train_matrices[[start_year]]
    players <- as.vector(unique(c(train_matrix$player1,train_matrix$player2)))

    # Fit the model
    model = BTm(cbind(win1, win2), player1, player2, data = train_matrix)

    s <- summary(model)
    names <- sort(players)
    ref_player <- players[1]

    df_coeff <- as.data.frame(s$coefficients)
    rownames(df_coeff) <- lapply(rownames(df_coeff),
                                 function(x) substr(x, 3, nchar(x)))
    df_coeff[ref_player,] <- rep(0, 2)

    # make predictions
    draws <- pred_df[,c("winner_name","loser_name")]
    draws <- draws[draws$winner_name %in% players & draws$loser_name %in% players, ]

    for (i in 1:nrow(draws)) {
        player1 <- draws$winner_name[i]
        player2 <- draws$loser_name[i]
        pred <- predict(player1, player2, df_coeff)
        draws$pred[i] <- pred[2]
    }

    draws$correct <- draws$pred > 0.5
    print(sum(draws$correct) / nrow(draws))
}

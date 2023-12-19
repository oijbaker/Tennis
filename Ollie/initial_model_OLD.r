library(BradleyTerry2)

matches <- read.csv("data/matches_no_22.csv")

model <- BTm(cbind(wins1,
                   wins2),
             as.factor(player1), as.factor(player2),
             data = matches)

# open the 2023 file and make predictions
matches_2023 <- read.csv("data/atp_matches_2023.csv")
matches_2022 <- read.csv("data/atp_matches_2022.csv")
pred_df <- rbind(matches_2023, matches_2022)

# make training data
players <- unique(c(matches$player1, matches$player2))
matches_2023 <- matches_2023[matches_2023$winner_name %in% players & matches_2023$loser_name %in% players, ]
matches_2023 <- matches_2023[, c("winner_name", "loser_name")]

s <- summary(model)
names <- sort(players)

# make all coefficients positive by setting the worst player
# as the reference category
if (min(s$coefficients) < 0) {
  # redo the model with refcat set to the name of the worst player
  worst_player <- names[which.min(s$coefficients[,1])+1]
  model <- BTm(cbind(wins1, wins2),
               as.factor(player1), as.factor(player2), data = matches,
               refcat = worst_player)
}

s <- summary(model)
df_coeff <- as.data.frame(s$coefficients)
rownames(df_coeff) <- lapply(rownames(df_coeff),
                             function(x) substr(x, 3, nchar(x)))

# if worst_player is NULL
if (is.null(worst_player)) {
  worst_player <- names[1]
}

df_coeff[worst_player,] <- rep(0, 2)

predict <- function(player1, player2) {
  lambda1 <- df_coeff[player1,1]
  lambda2 <- df_coeff[player2,1]

  pred <- exp(lambda1) / (exp(lambda1) + exp(lambda2))
  return(c(pred>0.5, pred))
}

# make predictions
draws <- pred_df[,c("winner_name","loser_name")]
draws <- draws[draws$winner_name %in% players & draws$loser_name %in% players, ]

for (i in 1:nrow(draws)) {
  player1 <- draws$winner_name[i]
  player2 <- draws$loser_name[i]
  pred <- predict(player1, player2)
  draws$pred[i] <- pred[2]
}

# find the accuracy
draws$correct <- draws$pred > 0.5
sum(draws$correct) / nrow(draws)
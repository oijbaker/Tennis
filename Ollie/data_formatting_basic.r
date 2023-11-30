# Load the utils package
library(utils)
library(BradleyTerry2)
library(ggplot2)
library(tidyr)

# Define the file path
file_paths <- c("data/atp_matches_2022.csv")#,
#                 "data/atp_matches_2021.csv", "data/atp_matches_2020.csv",
#                 "data/atp_matches_2019.csv")

df <- read.csv(file_paths[1])

n_players <- c(0)
zero_fraction <- c(1)

# for (i in 2:(length(file_paths)-1)) {
#   df <- rbind(df, read.csv(file_paths[i]))
# }

# make a dataframe with the data we need
df2 <- df[,c("winner_name", "loser_name")]


winners = df2$winner_name
losers = df2$loser_name
players = intersect(winners, losers)

# make a list of players with 0
player_freqs <- setNames(rep(0, length(players)), players)
for (j in 1:length(players)) {
  for (k in 1:length(winners)) {
    if (winners[k] == players[j]) {
      player_freqs[j] <- player_freqs[j] + 1
    } else if (losers[k] == players[j]) {
      player_freqs[j] <- player_freqs[j] + 1
    }
  }
}

# filter the players to only have players with more than 5 matches
players <- names(player_freqs[player_freqs >= 20])
players

# Filter df2
df2 <- df2[df2$winner_name %in% players & df2$loser_name %in% players, ]

winners = df2$winner_name
losers = df2$loser_name

# make a dataframe of 0s with the column and row names as the player names
zero_matrix <- matrix(0, nrow = length(players), ncol = length(players))

# Convert the matrix to a dataframe
df3 <- as.data.frame(zero_matrix)

# Set the row and column names
rownames(df3) <- players
colnames(df3) <- players

for (j in 1:nrow(df2)) {
  winner <- df2$winner_name[j]
  loser <- df2$loser_name[j]
  df3[winner, loser] <- df3[winner, loser] + 1
}

# find the number of zeros in df3
n_players <- c(n_players, length(players))
zero_fraction <- c(zero_fraction, sum(df3 == 0) / (length(players)**2))

count_data <- data.frame(matrix(nrow=0, ncol=4))
colnames(count_data) <- c("players", "win1", "win2")

for (i in 1:ncol(df3)) {
  for (j in i:nrow(df3)) {
    if (df3[i,j] != 0 || df3[j,i] != 0) {
      count_data <- rbind(count_data, c(players[i], players[j], df3[i,j], df3[j,i]))
    }
  }
}


# NEED TO CHECK SET DIFFERENCE THEN SWITCH RELEVANT PLAYERS ROUND


colnames(count_data) <- c("player1", "player2", "wins1", "wins2")

# switch pairs so that each column has the same players in

matches <- data.frame(count_data)

# find last occurence of the first player1
last <- max(which(matches$player1 == matches$player1[1]))

# swap player1 with player 2 and wins1 with wins2 at index last
matches[last, c("player1", "player2", "wins1", "wins2")] <- matches[last, c("player2", "player1", "wins2", "wins1")]

missing_players = setdiff(matches$player2, matches$player1)
print(missing_players)

# repeat for the second player1
last <- max(which(matches$player1 == matches$player1[last+1]))

# swap player1 with player 2 and wins1 with wins2 at index last
matches[last, c("player1", "player2", "wins1", "wins2")] <- matches[last, c("player2", "player1", "wins2", "wins1")]

# swap the first occurrence of each player in missing players as above
for (i in 1:length(missing_players)) {
  last <- min(which(matches$player2 == missing_players[i]))
  matches[last, c("player1", "player2", "wins1", "wins2")] <- matches[last, c("player2", "player1", "wins2", "wins1")]
}

write.csv(matches, "data/matches3.csv", row.names = FALSE)

library(BradleyTerry2)

matches <- read.csv("data/matches.csv")

setdiff(matches$player1, matches$player2)
setdiff(matches$player2, matches$player1)

model <- BTm(cbind(wins1,
                   wins2),
             as.factor(player1), as.factor(player2), data=matches)
model


library(BradleyTerry2)

matches <- read.csv("data/matches.csv")
matches <- data.frame(matches[1:100,])
typeof(matches$wins1)
model <- BTm(cbind(wins1,
                   wins2),
             player1, player2, data=matches)
model

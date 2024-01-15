library(TennisBT)
library(readr)
library(plyr)
library(tidyr)
library(dplyr)
library(BradleyTerry2)

dat <- TennisTidyr(2013, 2022, 2023, 20)
dat_csv <- dat$train
data_test <- dat$test
players <- dat$main_names

# Extracting necessary columns and removing NAs
data <- data.frame(dat_csv[,c("winner_id", "winner_name", "winner_hand", "winner_ht", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon",
                              "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced", "loser_id", "loser_name", "loser_hand", "loser_ht",
                              "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved",
                              "l_bpFaced", "surface")])
data <- na.omit(data)
attach(data)

# Extract all elements from winner_name and loser_name columns
all_players <- data.frame(Player=c(winner_name,loser_name))
# Count number of rows each unique player has in 'all_players' (i.e. each player's number of total wins & losses)
count <- count(all_players, Player, name="Total_matches")
# Only keep players who have played 20+ matches
players <- filter(count, Total_matches >= 20)

# Check that all the players in 'players' have won at least one match
which( !(players$Player%in%winner_name) )

# Make a dataframe showing the hand of each player
hand <- data.frame(Player=c(winner_name,loser_name), Hand=c(winner_hand,loser_hand))
hand <- distinct(hand) # eliminates duplicate rows
# Eliminate players whose hand is unknown
hand <- filter(hand, Hand != "U")
# Similarly for height
height <- data.frame(Player=c(winner_name,loser_name), Height=c(winner_ht,loser_ht))
height <- distinct(height)

# Make 'predictors' dataframe
predictors <- players %>% inner_join(hand) %>% inner_join(height)
predictors = predictors[,-2]
# Convert characters to factors
predictors$Player <- factor(predictors$Player)
predictors$Hand <- factor(predictors$Hand)

# Create indices that allow us to extract matches where both the winner and loser must be in 'predictors$Player' and their hand is known
indices_w <- winner_name %in% predictors$Player
indices_l <- loser_name %in% predictors$Player
indices_wh <- (winner_hand != "U")
indices_lh <- (loser_hand != "U")
indices <- indices_w == TRUE & indices_l == TRUE & indices_wh == TRUE & indices_lh == TRUE

# Extract 'winner_name' and 'loser_name', and keeping only the matches marked TRUE by indices
matches <- data.frame( Winner=factor(winner_name[indices]), Loser=factor(loser_name[indices]) )
players <- unique( c(matches[,1], matches[,2]) )

##### Make new variables
# Extract Surface variable for the matches we're considering
Surface <- surface[indices]

# Creating 3 dataframes for matches played on clay, grass, hard court
ind_clay <- Surface == "Clay"
ind_grass <- Surface == "Grass"
ind_hard <- Surface =="Hard"
matches_clay <- matches[ind_clay,]
matches_grass <- matches[ind_grass,]
matches_hard <- matches[ind_hard,]
# Initial values for the variables we're aiming to find
prev_wins_clay <- matrix(0, nrow=dim(matches_clay)[1], 2)
prev_wins_grass <-matrix(0, nrow=dim(matches_grass)[1], 2)
prev_wins_hard <- matrix(0, nrow=dim(matches_hard)[1], 2)

func_clay <- func_surface(matches_clay, prev_wins_clay)
func_grass <- func_surface(matches_grass, prev_wins_grass)
func_hard <- func_surface(matches_hard, prev_wins_hard)

# Split 'matches' dataframe
winners0 <- data.frame(Player=matches[,1])
losers0 <- data.frame(Player=matches[,2])
# Add these columns to the 'winners0' and 'losers0' dataframes
# Hard court
winners0$prev.wins.hard = rep(0,dim(winners0)[1])
winners0$prev.wins.hard[which(ind_hard==TRUE)] <- func_hard[,1]
losers0$prev.wins.hard = rep(0,dim(losers0)[1])
losers0$prev.wins.hard[which(ind_hard==TRUE)] <- func_hard[,2]
# Clay
winners0$prev.wins.clay = rep(0,dim(winners0)[1])
winners0$prev.wins.clay[which(ind_clay==TRUE)] <- func_clay[,1]
losers0$prev.wins.clay = rep(0,dim(losers0)[1])
losers0$prev.wins.clay[which(ind_clay==TRUE)] <- func_clay[,2]
# Grass
winners0$prev.wins.grass = rep(0,dim(winners0)[1])
winners0$prev.wins.grass[which(ind_grass==TRUE)] <- func_grass[,1]
losers0$prev.wins.grass = rep(0,dim(losers0)[1])
losers0$prev.wins.grass[which(ind_grass==TRUE)] <- func_grass[,2]

# Surface variable:
winners0$Surface <- winners0$prev.wins.clay + winners0$prev.wins.grass + winners0$prev.wins.hard
losers0$Surface <- losers0$prev.wins.clay + losers0$prev.wins.grass + losers0$prev.wins.hard
winners0 <- winners0[,-(2:4)]
losers0 <- losers0[,-(2:4)]

variables <- func(matches, data, indices)

# Add in columns for new variables
winners <- data.frame(winners0, variables$w_output)
losers <- data.frame(losers0, variables$l_output)
# Convert the 'Player' column to rows
library(tibble)
predictors = column_to_rownames(predictors, var="Player")

# Combine everything into a single list
data_for_model <- list(winners=winners, losers=losers, predictors=predictors)

attach(data_test)

indices_test <- winner_name %in% players == TRUE & loser_name %in% players
matches_test <- data.frame( Winner=factor(winner_name[indices_test]), Loser=factor(loser_name[indices_test]) )
players_test <- unique( c(matches_test[,1], matches_test[,2]) )
# Make sure Winner and Loser factors have same number of levels as # players
levels(matches_test$Winner) <- levels(players_test)
levels(matches_test$Loser) <- levels(players_test)

##### Make new variables
# Extract Surface variable for the matches we're considering
Surface <- surface[indices_test]

# Creating 3 dataframes for matches played on clay, grass, hard court
ind_clay <- Surface == "Clay"
ind_grass <- Surface == "Grass"
ind_hard <- Surface =="Hard"
matches_clay <- matches_test[ind_clay,]
matches_grass <- matches_test[ind_grass,]
matches_hard <- matches_test[ind_hard,]

# Initial values for the variables we're aiming to find
prev_wins_clay <- matrix(0, nrow=dim(matches_clay)[1], 2)
prev_wins_grass <-matrix(0, nrow=dim(matches_grass)[1], 2)
prev_wins_hard <- matrix(0, nrow=dim(matches_hard)[1], 2)

# Apply func_surface
func_clay <- func_surface(matches_clay, prev_wins_clay)
func_grass <- func_surface(matches_grass, prev_wins_grass)
func_hard <- func_surface(matches_hard, prev_wins_hard)

# Split 'matches' dataframe
winners0 <- data.frame(Player=matches_test[,1])
losers0 <- data.frame(Player=matches_test[,2])

# Add these columns to the 'winners0' and 'losers0' dataframes
# Hard court
winners0$prev.wins.hard = rep(0,dim(winners0)[1])
winners0$prev.wins.hard[which(ind_hard==TRUE)] <- func_hard[,1]
losers0$prev.wins.hard = rep(0,dim(losers0)[1])
losers0$prev.wins.hard[which(ind_hard==TRUE)] <- func_hard[,2]
# Clay
winners0$prev.wins.clay = rep(0,dim(winners0)[1])
winners0$prev.wins.clay[which(ind_clay==TRUE)] <- func_clay[,1]
losers0$prev.wins.clay = rep(0,dim(losers0)[1])
losers0$prev.wins.clay[which(ind_clay==TRUE)] <- func_clay[,2]
# Grass
winners0$prev.wins.grass = rep(0,dim(winners0)[1])
winners0$prev.wins.grass[which(ind_grass==TRUE)] <- func_grass[,1]
losers0$prev.wins.grass = rep(0,dim(losers0)[1])
losers0$prev.wins.grass[which(ind_grass==TRUE)] <- func_grass[,2]

# Surface variable:
winners0$Surface <- winners0$prev.wins.clay + winners0$prev.wins.grass + winners0$prev.wins.hard
losers0$Surface <- losers0$prev.wins.clay + losers0$prev.wins.grass + losers0$prev.wins.hard
winners0 <- winners0[,-(2:4)]
losers0 <- losers0[,-(2:4)]

# Apply func
variables <- func(matches_test, data_test, indices_test)

# Add in columns
winners_test <- data.frame(winners0, variables$w_output)
losers_test <- data.frame(losers0, variables$l_output)
# Combine everything into a single list
data_for_model_test <- list(winners=winners_test, losers=losers_test, predictors=predictors[players_test,] )


##### Without covariates
data_for_model0 <- list(matches=matches, predictors=predictors)
data_for_model_test0 <- list(matches=matches_test, predictors=predictors[players_test,] )
model0 <- BTm(rep(1,dim(matches)[1]), player1=Winner, player2=Loser, data=data_for_model0)


##### With covariates
model_full <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
              formula = ~  Hand[Player] + Height[Player] + Surface + Ace + Df + Svpt +
                FirstIn + FirstWon + SecondWon + SvGms + BpSaved + BpFaced + (1|Player),
              id="Player", data=data_for_model)
# Removing SvGms
model1 <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
                    formula = ~  Hand[Player] + Height[Player] + Surface + Ace + Df + Svpt +
                      FirstIn + FirstWon + SecondWon + BpSaved + BpFaced + (1|Player),
                    id="Player", data=data_for_model)
# Removing BpFaced
model2 <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
              formula = ~  Hand[Player] + Height[Player] + Surface + Ace + Df + Svpt +
                FirstIn + FirstWon + SecondWon + BpSaved + (1|Player),
              id="Player", data=data_for_model)
# Removing FirstIn
model3 <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
              formula = ~  Hand[Player] + Height[Player] + Surface + Ace + Df + Svpt +
                FirstWon + SecondWon + BpSaved + (1|Player),
              id="Player", data=data_for_model)
# Removing BpSaved
model4 <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
              formula = ~  Hand[Player] + Height[Player] + Surface + Ace + Df + Svpt +
                FirstWon + SecondWon + (1|Player),
              id="Player", data=data_for_model)
# Removing Hand (although all variables are already signif at <0.1)
model5 <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
              formula = ~  Height[Player] + Surface + Ace + Df + Svpt +
                FirstWon + SecondWon + (1|Player),
              id="Player", data=data_for_model)
# Removing SecondWon
model6 <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
              formula = ~  Height[Player] + Surface + Ace + Df + Svpt +
                FirstWon + (1|Player),
              id="Player", data=data_for_model)
# Removing Height
model7 <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
              formula = ~  Surface + Ace + Df + Svpt +
                FirstWon + (1|Player),
              id="Player", data=data_for_model)
# Removing Df
model8 <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
              formula = ~  Surface + Ace + Svpt +
                FirstWon + (1|Player),
              id="Player", data=data_for_model)
# Removing Ace
model9 <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
              formula = ~  Surface + Svpt +
                FirstWon + (1|Player),
              id="Player", data=data_for_model)


summary(model0)
summary(model_full)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)
summary(model9)

pred0 <- predict(model0, newdata=data_for_model_test0, type="response", se.fit=TRUE)
pred_full <- predict(model_full, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred1 <- predict(model1, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred2 <- predict(model2, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred3 <- predict(model3, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred4 <- predict(model4, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred5 <- predict(model5, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred6 <- predict(model6, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred7 <- predict(model7, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred8 <- predict(model8, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred9 <- predict(model9, newdata=data_for_model_test, type="response", se.fit=TRUE)

# Remove NA predictions

pred0$fit <- na.omit(pred0$fit)
pred1$fit <- na.omit(pred1$fit)
pred2$fit <- na.omit(pred2$fit)
pred3$fit <- na.omit(pred3$fit)
pred4$fit <- na.omit(pred4$fit)
pred5$fit <- na.omit(pred5$fit)
pred6$fit <- na.omit(pred6$fit)
pred7$fit <- na.omit(pred7$fit)
pred8$fit <- na.omit(pred8$fit)
pred9$fit <- na.omit(pred9$fit)

mean(pred0$fit)
length(which(pred0$fit>0.5))
length(which(pred0$fit>0.5)) / length(pred0$fit)

mean(pred_full$fit)
length(which(pred_full$fit>0.5))
length(which(pred_full$fit>0.5)) / length(pred_full$fit)

mean(pred1$fit)
length(which(pred1$fit>0.5))
length(which(pred1$fit>0.5)) / length(pred1$fit)

mean(pred2$fit)
length(which(pred2$fit>0.5))
length(which(pred2$fit>0.5)) / length(pred2$fit)

mean(pred3$fit)
length(which(pred3$fit>0.5))
length(which(pred3$fit>0.5)) / length(pred3$fit)

mean(pred4$fit)
length(which(pred4$fit>0.5))
length(which(pred4$fit>0.5)) / length(pred4$fit)

mean(pred5$fit)
length(which(pred5$fit>0.5))
length(which(pred5$fit>0.5)) / length(pred5$fit)

mean(pred6$fit) 
length(which(pred6$fit>0.5))
length(which(pred6$fit>0.5)) / length(pred6$fit)

mean(pred7$fit) 
length(which(pred7$fit>0.5))
length(which(pred7$fit>0.5)) / length(pred7$fit)

mean(pred8$fit)
length(which(pred8$fit>0.5))
length(which(pred8$fit>0.5)) / length(pred8$fit)

mean(pred9$fit)
length(which(pred9$fit>0.5))
length(which(pred9$fit>0.5)) / length(pred9$fit)

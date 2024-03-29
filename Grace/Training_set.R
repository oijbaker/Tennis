library(readr)
library(plyr)
library(tidyr)
library(dplyr)
library(BradleyTerry2)
library(TennisBT)

# myfiles <- list.files(path = "/home/cr23868/Documents/Tennis/R_code/tennis_wta-master", pattern = ".csv", full.names=TRUE)
# dat_csv <- ldply(myfiles, read_csv)

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

func_surface <- function(MATCHES, PREV_WINS) {
  
  players <- unique( c(MATCHES[,1], MATCHES[,2]) )
  scores <- data.frame(Player=players, Score=rep(0, length(players))) # 'scores' keeps track of their # previous wins on a surface
  PREV_WINS <- matrix( 0, dim(MATCHES)[1], 2)
  
  # Printing to see if it works
  #View(matches_clay)
  #View(matches_grass)
  #View(matches_hard)
  #View(prev_wins_clay)
  #View(prev_wins_grass)
  #View(prev_wins_hard)
  #View(players)
  #View(scores)
  #View(PREV_WINS)
  
  for (n in 2:dim(PREV_WINS)[1]) {
    prev_winner <- which(scores$Player==MATCHES[n-1,1]) # gives row number of previous match winner in 'scores' df
    scores[prev_winner, 2] = scores[prev_winner, 2] + 1 # updates previous match winner's score
    
    curr_player1 <- which(scores$Player==MATCHES[n,1]) # gives row number of 1st player in upcoming match
    curr_player2 <- which(scores$Player==MATCHES[n,2]) # same for 2nd player
    PREV_WINS[n,] = c(scores[curr_player1, 2], scores[curr_player2, 2]) # gives current scores of players in upcomin match (unchanged if neither player were in previous match)
  }
  
  return(PREV_WINS)
}

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

## Make other variables
func <- function(matches, data, indices) {
  
  players <- unique( c(matches[,1], matches[,2]) )
  # 'scores' keeps track of each player's numbers so far.
  scores <- data.frame(Player=players,
                       Ace=rep(0, length(players)), Df=rep(0, length(players)),
                       Svpt=rep(0, length(players)), FirstIn=rep(0, length(players)),
                       FirstWon=rep(0, length(players)), SecondWon=rep(0, length(players)),
                       SvGms=rep(0, length(players)), BpSaved=rep(0, length(players)),
                       BpFaced=rep(0, length(players)) )
  
  # Extract relevant columns from 'data'
  w_numbers <- data[indices, 5:13]
  l_numbers <- data[indices, 18:26]
  
  # 'w_output' and 'l_output' are the variables we want
  w_output <- matrix( 0, dim(matches)[1], 9 ) # because there are 9 variables
  w_output <- as.data.frame(w_output)
  colnames(w_output) <- colnames(scores[-1])
  l_output <- w_output
  
  # Print to check dimensions are correct
  #View(w_numbers)
  #View(l_numbers)
  #View(players)
  #View(scores)
  #View(w_output)
  #View(l_output)
  
  
  for (n in 2:dim(matches)[1]) {
    prev_winner <- which(scores$Player==matches[n-1,1]) # gives row number of player who won the previous match
    scores[prev_winner, 2:10] = w_numbers[n-1,] # replaces previous match winner's score with the numbers obtained from previous match data
    # Same for losers
    prev_loser <- which(scores$Player==matches[n-1,2])
    scores[prev_loser, 2:10] = l_numbers[n-1,]
    
    curr_player1 <- which(scores$Player==matches[n,1]) # gives row number of 1st player in upcoming match
    curr_player2 <- which(scores$Player==matches[n,2]) # same for 2nd player
    w_output[n,] <- scores[curr_player1, 2:10] # get current scores of 1st player in upcoming match
    l_output[n,] <- scores[curr_player2, 2:10] # same for 2nd player
  }
  
  return( list(w_output=w_output, l_output=l_output) )
}

variables <- func(matches, data, indices)

# Add in columns for new variables
winners <- data.frame(winners0, variables$w_output)
losers <- data.frame(losers0, variables$l_output)
# Convert the 'Player' column to rows
library(tibble)
predictors = column_to_rownames(predictors, var="Player")

# Combine everything into a single list
data_for_model <- list(winners=winners, losers=losers, predictors=predictors)
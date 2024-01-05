# DO NOT RUN THIS CODE PLEASE PLEASE PLEASE DO NOT RUN THIS CODE

library(tidyr)
library(dplyr)

countsToBinomial<-function(xtab){
  ##assumes square
  players <- rownames(xtab)
  comb <- t(combn(nrow(xtab), 2))
  won <- xtab[comb]
  lost <- t(xtab)[comb]
  res <- !(won == 0 & lost == 0)
  player1 <- factor(players[comb[,1]], levels = players)[res]
  player2 <- factor(players[comb[,2]], levels = players)[res]
  return(data.frame(player1, player2, win1 = won[res], win2 = lost[res]))
}

# Define the file path
file_paths <- c("data/wta_matches_2021.csv", "data/wta_matches_2020.csv",
                "data/wta_matches_2019.csv", "data/wta_matches_2018.csv",
                "data/wta_matches_2017.csv", "data/wta_matches_2016.csv",
                "data/wta_matches_2015.csv", "data/wta_matches_2014.csv",
                "data/wta_matches_2013.csv", "data/wta_matches_2022.csv",
                "data/wta_matches_2023.csv")

df <- read.csv(file_paths[1])

for (i in 2:(length(file_paths)-1)) {
  df1 <- rbind(df, read.csv(file_paths[i]))
}

################## Extract relevant cols
df1 <- df1[,c("winner_name","loser_name","surface")]

################## Filter out matches with no surface
df1 <- df1 %>% filter(!is.na(surface))

################## Get matches on Grass surface
df1_grass <- df1 %>% filter(surface == "Grass")

################## Get matches on Hard surface
df1_hard <- df1 %>% filter(surface == "Hard")
df1_hard

################## Get matches on Clay surface
df1_clay <- df1 %>% filter(surface == "Clay")

# Note we assume carpet surface is irrelevant as there are only 27 entries

# run the code below for each surface
surface_dfs = list(grass=df1_grass, hard=df1_hard, clay=df1_clay)
surface_names = c("grass", "hard", "clay")

i = 1
for (s in surface_dfs) {
  
  # Remove players in s that appear in fewer than 5 matches
  players <- unique(c(s$winner_name, s$loser_name))
  player_freqs <- setNames(rep(0, length(players)), players)
  for (j in 1:length(players)) {
    for (k in 1:length(s$winner_name)) {
      if (s$winner_name[k] == players[j]) {
        player_freqs[j] <- player_freqs[j] + 1
      } else if (s$loser_name[k] == players[j]) {
        player_freqs[j] <- player_freqs[j] + 1
      }
    }
  }
  # filter the players to only have players with more than 5 matches
  players <- names(player_freqs[player_freqs >= 5])
  s <- s[s$winner_name %in% players & s$loser_name %in% players, ]
  
  
  ################## Get frequencies
  tab1_getfreq <- s %>%
    group_by(winner_name, loser_name)%>%
    summarise(n=n())
  
  
  ################## Make df wide
  attach(tab1_getfreq) # Prevents bugs in the next line
  
  tab1_contingency <- tab1_getfreq %>% pivot_wider(names_from = loser_name, values_from = n)
  
  ################## Shift row names to the left
  library(tidyverse)
  tab1_contingency <- tab1_contingency %>% remove_rownames %>% column_to_rownames(var="winner_name")
  
  ################## Make tab1_contingency symmetric by adding the missing winners/losers
  rows <- as.vector(rownames(tab1_contingency))
  cols <- as.vector(colnames(tab1_contingency))
  
  missing_winners <- setdiff(cols,rows)
  missing_losers <- setdiff(rows,cols)
  
  ################## Append missing winners/losers to tab1_contingency
  # First, bind rows...
  tab2_extrarows <- data.frame(matrix(NA, nrow=length(missing_winners), ncol=length(colnames(tab1_contingency))))
  rownames(tab2_extrarows) <- missing_winners
  colnames(tab2_extrarows) <- colnames(tab1_contingency)
  tab_concat <- rbind(tab1_contingency, tab2_extrarows)
  
  # ...then cols
  tab2_extracols <- data.frame(matrix(NA, nrow=length(rownames(tab_concat)), ncol=length(missing_losers)))
  rownames(tab2_extracols) <- rownames(tab_concat)
  colnames(tab2_extracols) <- missing_losers
  tab_concat <- cbind(tab_concat, tab2_extracols)
  
  
  ################## Sort players' names in alphabetical order
  final_tab <- tab_concat[sort(rownames(tab_concat)), ]
  final_tab <- final_tab[,sort(colnames(tab_concat))]
  ################## Convert NAs to 0s
  final_tab <- final_tab %>% mutate_all(funs(replace_na(.,0)))
  
  tennis1 <- countsToBinomial(final_tab)
  
  # # NEED TO CHECK SET DIFFERENCE THEN SWITCH RELEVANT PLAYERS ROUND
  
  
  # colnames(tennis_1) <- c("player1", "player2", "wins1", "wins2")
  
  # # switch pairs so that each column has the same players in
  
  # matches <- data.frame(tennis_1)
  
  # # find last occurence of the first player1
  # last <- max(which(matches$player1 == matches$player1[1]))
  
  # # swap player1 with player 2 and wins1 with wins2 at index last
  # matches[last, c("player1", "player2", "wins1", "wins2")] <- matches[last, c("player2", "player1", "wins2", "wins1")]
  
  # missing_players = setdiff(matches$player2, matches$player1)
  # print(missing_players)
  
  # # repeat for the second player1
  # last <- max(which(matches$player1 == matches$player1[last+1]))
  
  # # swap player1 with player 2 and wins1 with wins2 at index last
  # matches[last, c("player1", "player2", "wins1", "wins2")] <- matches[last, c("player2", "player1", "wins2", "wins1")]
  
  # # swap the first occurrence of each player in missing players as above
  # for (i in 1:length(missing_players)) {
  #   last <- min(which(matches$player2 == missing_players[i]))
  #   matches[last, c("player1", "player2", "wins1", "wins2")] <- matches[last, c("player2", "player1", "wins2", "wins1")]
  # }
  
  p1 = factor(tennis1$player1, levels = players)[rep(TRUE, nrow(tennis1))]
  p2 = factor(tennis1$player2, levels = players)[rep(TRUE, nrow(tennis1))]
  tennis1 <- tennis1 %>% mutate(player1 = p1, player2 = p2)
  
  # write to CSV names "matches_surface_name.csv"
  write.csv(tennis1, paste0("data/matches_", surface_names[i], ".csv"), row.names=FALSE)
  i <- i+1
}

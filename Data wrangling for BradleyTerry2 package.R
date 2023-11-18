library(tidyr)
library(dplyr)

################## Import 2023 ATP file manually

################## Extract relevant cols
df1 <- data.frame(atp_matches_qual_chall_2023[,c("winner_name","loser_name")])
View(df1)

tab1 <- table(df1)
View(tab1)

################## Get frequencies
tab1_getfreq <- df1 %>%
  group_by(winner_name, loser_name)%>%
  summarise(n=n())

print(tab1_getfreq, n=20) # Just to see what it looks like

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

View(final_tab)

################## Reshape table using BradleyTerry2 package
library(BradleyTerry2)

################## Take a subset of final_tab and fit the model
subset <- final_tab[1:100,1:100]
tennis1 <- countsToBinomial(subset)

btmodel1 <- BTm(cbind(win1,win2), player1, player2, data = tennis1)
btmodel1
summary(btmodel1)

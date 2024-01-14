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
# Removing FirstIn
model2 <- BTm(rep(1,dim(matches)[1]), player1=winners, player2=losers,
                    formula = ~  Hand[Player] + Height[Player] + Surface + Ace + Df + Svpt +
                      FirstWon + SecondWon + BpSaved + BpFaced + (1|Player),
                    id="Player", data=data_for_model)
# Removing BpFaced
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



summary(model0)
summary(model_full)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)

pred0 <- predict(model0, newdata=data_for_model_test0, type="response", se.fit=TRUE)
pred_full <- predict(model_full, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred1 <- predict(model1, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred2 <- predict(model2, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred3 <- predict(model3, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred4 <- predict(model4, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred5 <- predict(model5, newdata=data_for_model_test, type="response", se.fit=TRUE)
pred6 <- predict(model6, newdata=data_for_model_test, type="response", se.fit=TRUE)

mean(pred0$fit) # 0.5917155
length(which(pred0$fit>0.5)) # 1285
length(which(pred0$fit>0.5)) / length(pred0$fit) # 0.6576254

mean(pred_full$fit) # 0.5809942
length(which(pred_full$fit>0.5)) # 1336
length(which(pred_full$fit>0.5)) / length(pred_full$fit) # 0.6837257

mean(pred1$fit) # 0.5809969
length(which(pred1$fit>0.5)) # 1336
length(which(pred1$fit>0.5)) / length(pred1$fit) # 0.6837257

mean(pred2$fit) # 0.5809995
length(which(pred2$fit>0.5)) # 1338
length(which(pred2$fit>0.5)) / length(pred2$fit) # 0.6847492

mean(pred3$fit) # 0.5810482
length(which(pred3$fit>0.5)) # 1338
length(which(pred3$fit>0.5)) / length(pred3$fit) # 0.6847492

mean(pred4$fit) # 0.5811076
length(which(pred4$fit>0.5)) # 1333
length(which(pred4$fit>0.5)) / length(pred4$fit) # 0.6821904

mean(pred5$fit) # 0.5813334
length(which(pred5$fit>0.5)) # 1329
length(which(pred5$fit>0.5)) / length(pred5$fit) # 0.6801433

mean(pred6$fit) # 0.579767
length(which(pred6$fit>0.5)) # 1316
length(which(pred6$fit>0.5)) / length(pred6$fit) # 0.6734903
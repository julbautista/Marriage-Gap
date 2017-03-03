source("Marriage_Gap_Data.R") #data manipulation
library(cowplot)

#Create Stan Model
stanc("logistic.stan")$status
fit1 <- stan("logistic.stan",
             data = c("N","marital","state_id","party", 
                      "state_id_pred", "mar_pred"),
             iter = 3000, chains = 4)
beep()

#create objects for plotting
fitted <- extract(fit1)
obama_per <- elect$vote_Obama_pct/100
mar_gap <- marriage %>% 
  group_by(state) %>% 
  summarise(gap = mean(marital[party == 1]) - mean(marital[party == 0]), vote = mean(party))
obama_vote <- mar_gap$vote
gap <- mar_gap$gap

#pulling from stan
gap_pred <- colMeans(fitted$gap_pred)
vote_pred <- colMeans(fitted$vote_pred)
pool_pred <- colMeans(fitted$pool_pred)
gap_pool_pred <- mean(fitted$gap_pool_pred)

#vote_pred_sd <- apply(fitted$vote_pred, 2, sd)
#vote_pred_sd <- rep(sd(vote_pred),48)
#invlogit(apply(fitted$alpha,2,sd) + apply(fitted$beta,2,sd))

# gap_pred <- invlogit(colMeans(fitted$alpha) + 
#                      colMeans(fitted$beta)) - 
#             invlogit(colMeans(fitted$alpha))
# vote_pred <- colMeans(fitted$vote_pred)

#vote prediction plots
plotter<- rbind(
  data.frame(vote = obama_vote, Legend = rep("Data", 48),obama_per),
  data.frame(vote = vote_pred, Legend = rep("Model", 48),obama_per))

pp<-ggplot(plotter, aes(vote, obama_per, colour = Legend)) + jbplot + jbscale +
  geom_abline(slope = 1, intercept = 0, alpha = 0.4) + geom_point() +
  labs(x = "Obama Vote Shares", y = "Predicted Vote Shares", title = "Vote Prediction", colour = "")

#balance plot for vote prediction
diff_vote <- obama_per - vote_pred

b_p<-ggplot(NULL, aes(c(1:48), diff_vote)) + ylim(-1,1) + jbplot +
  geom_hline(yintercept = 0, alpha = 0.6) + geom_point(colour = jbpal$blue) +
  labs(x = "State Indexes", y = "Difference between Predicted and Actual Votes", title = "Vote Prediction Balance")

plot_grid(pp,b_p, ncol = 2)

#plot marriage gap
plotter2<- rbind(
  data.frame(gap = gap, Legend = rep("Data", 48),obama_per),
  data.frame(gap = gap_pred, Legend = rep("Model", 48),obama_per))

ggplot(plotter2, aes(gap, obama_per, colour = Legend)) + jbplot + jbscale +
  geom_point() +
  labs(colour = "", x = "Marriage Gap", y = "Obama Vote % 2008", title = "Marriage Gap")

#posterior predictive checks
ggplot(marriage, aes(pred)) + geom_histogram()
ggplot(marriage, aes(pred, state)) + geom_histogram(aes(marriage$pred))


marriage$pred <- colMeans(fitted$indv_vote)

par(mfrow = c(8,6),
    mar = c(1, 1, 1, 1),
    mgp = c(1.5, 0.2, 0),
    cex = 0.8)
for(i in 1:48){
  hist(marriage$pred[marriage$id == i], freq = FALSE, 
       breaks = 60, xlab = NULL, ylab = NULL, yaxt = 'n',
       xaxt = 'n', main = state[i], cex.main = 0.8)
  # axis(1, at = c(0.25, 0.5, 0.75),
  #      cex.axis = 0.5)
  abline(v = mean(marriage$party[marriage$marital == 1
                                 & marriage$id == i]),
         col = "dark green")
  abline(v = mean(marriage$party[marriage$marital == 0
                                 & marriage$id == i]),
         col = "blue")
}

#complete pool vote prediction plots
plotter3<- rbind(
  data.frame(vote = obama_vote, Legend = rep("Data", 48),obama_per),
  data.frame(vote = pool_pred, Legend = rep("Model", 48),obama_per))

com_pp<-ggplot(plotter3, aes(vote, obama_per, colour = Legend)) + jbplot + jbscale +
  geom_abline(slope = 1, intercept = 0, alpha = 0.4) + geom_point() + 
  labs(x = "Obama Vote Shares", y = "Predicted Vote Shares", title = "Complete Pooling Vote Prediction", colour = "")  

#balance plot for complete pooling vote prediction
diff_vote <- obama_per - pool_pred

com_b_p<-ggplot(NULL, aes(c(1:48), diff_vote)) + jbplot + jbscale + 
  geom_hline(yintercept = 0, alpha = 0.6) + geom_point(colour = jbpal$blue) +  
  labs(x = "State Indexes", y = "Difference between Predicted and Actual Votes", title = "Complete Pooling Vote Prediction Balance") +
  ylim(-1,1)

plot_grid(com_pp,com_b_p, ncol = 2)

#plot complete pooling marriage gap
plotter4<- rbind(
  data.frame(gap = gap, Legend = rep("Data", 48),obama_per),
  data.frame(gap = gap_pool_pred, Legend = rep("Model", 48),obama_per))

ggplot(plotter4, aes(gap, obama_per, colour = Legend)) + geom_point() + jbplot + jbscale +
  labs(colour = "", x = "Marriage Gap", y = "Obama Vote % 2008", title = "Complete Pooling Marriage Gap")

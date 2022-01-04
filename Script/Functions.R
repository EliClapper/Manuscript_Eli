metrics <- function(obs, pred){
  RMSE <- sqrt(mean((obs-pred)^2))
  MAD <- mean(abs(obs-pred))
  R2 <- 1-((sum((obs-pred)^2))/(sum((obs - mean(obs))^2)))
  return(c(RMSE = RMSE, MAD = MAD, R2 = R2))
}


LPCR <- function(train, test, npc){
  
  # obtain predictor matrices
  train_pred <- as.matrix(train[!(colnames(train) %in% c('Rating','ind'))]) 
  test_pred <- as.matrix(test[!(colnames(test) %in% c('Rating'))])
  
  # run pcr on test dat
  train_pca <- prcomp(train_pred)
  train_pca_scores <- as.data.frame(cbind(Rating = train$Rating, PC = train_pca$x[,1:npc]))
  
  #run imposed matrix on test data
  test_pca_scores <- predict(train_pca, newdata = test_pred)[,1:npc] 
  test_pca_scores <- as.data.frame(cbind(PC = test_pca_scores)) #obtain dataframe with PC scores for the testing data
  
  #fit lm with PC's and evaluate on test data
  fit.lpcr <- lm(Rating ~ ., data = train_pca_scores)
  pred <- predict(fit.lpcr, test_pca_scores)
  
  return(metrics(test$Rating, pred))
}

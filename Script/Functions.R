#obtains metrics RMSE, MAD and R2 for an algorithm
metrics <- function(obs, pred){
  RMSE <- sqrt(mean((obs-pred)^2)) #Root Mean Square Error
  MAD <- mean(abs(obs-pred))       #Mean Absolute Deviation
  R2 <- 1-((sum((obs-pred)^2))/(sum((obs - mean(obs))^2)))  #R-sqaured
  return(c(RMSE = RMSE, MAD = MAD, R2 = R2))
}


#Linear Principle Component Analysis on current dataset
LPCR <- function(train, test, npc, relevance = F){
  
  # obtain predictor matrices
  train_pred <- as.matrix(train[!(colnames(train) %in% c('Rating','ind'))]) 
  test_pred <- as.matrix(test[!(colnames(test) %in% c('Rating'))])
  
  # run pcr on train data
  train_pca <- prcomp(train_pred)
  train_pca_scores <- as.data.frame(cbind(Rating = train$Rating, PC = train_pca$x[,1:npc]))
  
  # impose matrix rotation on test data
  test_pca_scores <- predict(train_pca, newdata = test_pred)[,1:npc] 
  test_pca_scores <- as.data.frame(cbind(PC = test_pca_scores))
  
  # fit lm with PC's and evaluate on test data
  fit.lpcr <- lm(Rating ~ ., data = train_pca_scores)
  pred <- predict(fit.lpcr, test_pca_scores)
  
  #if we do not need relevance, just return metrics
  if(!relevance){
    return(metrics(test$Rating, pred))
  }
  
  #obtain relevance
  CIs <- confint(fit.lpcr)
  unimp <- rownames(CIs[CIs[,1] < 0 & CIs[,2] >0, ]) #unimportant predictors
  imp <- rownames(CIs)[!rownames(CIs) %in% unimp][-1] #important predictors
  
  PC_selected <- train_pca$rotation[,imp]
  imp_feat <- apply(PC_selected, 1, function(x){
    imp <- !sum(abs(x) < 0.2) == length(imp)
  })
  
  imp.pcr <- names(which(imp_feat))

  return(list(metrics = metrics(test$Rating, pred), imp.pcr = imp.pcr))
}

#function to obtain list with metrics over folds and RMSE values
evaluation <- function(metrics){
  
  # obtain mean and variances of metrics over folds
  eval <- lapply(metrics, function(x){ 
    apply(x, 2, function(y){
      c(m = mean(y), variance = var(y))})
  })
  
  # obtain RMSE values vector
  RMSE <- sapply(eval, function(x){ 
    x[1,1]})
  
  # obtain condition with lowest RMSE
  lowestRMSE <- paste0(names(which(RMSE == min(RMSE))), ": ", min(RMSE)) 
  
  
  return(list(evaluation = eval, RMSE = RMSE, lowest_RMSE = lowestRMSE))
}


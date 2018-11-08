XGBtest <- function(modelList, dat_test){
  library("xgboost")
  
  predArr <- array(NA, c(dim(dat_test)[1], 4, 3))
  
  for (i in 1:12){
    fit_train <- modelList[[i]]
    ### calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    #featMat <- dat_test[, , c2]
    
    featMat <- dat_test[, 2:25, c2]
    center<-dat_test[,1,c2]
    ### make predictions
    predArr[, c1, c2] <- predict(fit_train$fit, newdata=featMat, n.trees=fit_train$iter, type="response")+center
  }
  return(predArr)
}


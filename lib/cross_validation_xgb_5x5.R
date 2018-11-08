########################
### Cross Validation ###
########################

### Author: Chengliang Tang
### Project 3

XGBcv.function <- function(X.train, y.train, par, K){ 
  
  n <- dim(y.train)[1]
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i, ,]
    train.label <- y.train[s != i, ,]
    test.data <- X.train[s == i, ,]
    test.label <- y.train[s == i, ,]
    
    fit <- XGBTr(train.data, train.label, par)
    pred <- XGBtest0(fit, test.data)  
    cv.error[i] <- mean((pred - test.label)^2)  
    
  }			
  return(c(mean(cv.error),sd(cv.error)))
}


############run test##############

#model_values <- seq(3, 11, 2)
#model_labels = paste("XGB with depth =", model_values)
  
#feat_train <- features$feature

#label_train <- features$label
  
#x.train = feat_train
#y.train = label_train
  
#err_cv <- array(dim=c(length(model_values), 2))
  
#for(k in 1:length(model_values)){
#   cat("k=", k, "\n")
#   err_cv[k,] <- XGBcv.function(feat_train, label_train, model_values[k], K = 3)
 # }

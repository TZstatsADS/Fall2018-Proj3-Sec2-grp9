stopImplicitCluster()
library(xgboost)
library(plyr)
require(snow)
require(foreach)
library(doParallel)
cl <- makeCluster(detectCores(logical = F))
registerDoParallel(cl)

### train
XGBTr<- function(dat_train, label_train, par=NULL){
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 5
    et = 0.3
    csamp = 0.3
  } else {
    depth <- par$depth
    et = par$et
    csamp = par$csamp
  }
  k1=rep(1:4,3)
  k2=rep(1:3,each=4)
  modelList<-foreach(i=1:12,.packages="xgboost") %dopar%{ 
    
    featMat <- dat_train[, , k2[i]]
    labMat <- label_train[, k1[i], k2[i]]
    # bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, 
    #                      eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
    # fit_xgb = xgboost(data = featMat, label = labMat,
    #                   max.depth = depth, eta = et, nrounds = 10, objective = "reg:linear", 
    #                   colsample_bytree = csamp, subsample = 0.5, num_parallel_tree = 3)
    fit_xgb = xgboost(data = featMat, label = labMat,
                      max.depth = 1, eta = 0.1, nrounds = 5, objective = "reg:linear")
    
    modelList[[i]] <- list(fit=fit_xgb)
  }
  return(modelList)
}

#####################Run###########################
ts_train <- system.time(modelList <- XGBTr(features$feature,features$label))
ts_train

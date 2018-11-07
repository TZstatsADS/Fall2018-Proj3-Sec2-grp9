#########################################################
### Train a classification model with training features ###
#########################################################

### Author: Chengliang Tang

library(dplyr)
library(foreach)
library(doParallel)
library(snow)
cl <- makeCluster(detectCores(logical = F))
registerDoParallel(cl)
train<- function(dat_train, label_train, par=NULL, shrinkage=NULL){
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 1
  } else {
    depth <- par$depth
  }
  if(is.null(shrinkage)){
    sk<- 0.01
  } else {
    sk <- shrinkage$sk_value
  }
  
  k1=rep(1:4,3)
  k2=rep(1:3,each=4)
  modelList<-foreach(i=1:12,.packages="gbm") %dopar%{ 
    
    featMat <- dat_train[, , k2[i]]
    labMat <- label_train[, k1[i], k2[i]] 
    fit_gbm=gbm.fit(x=featMat, y=labMat,
                    n.trees=200,
                    distribution="gaussian",
                    interaction.depth=depth, 
                    bag.fraction = 0.5,
                    shrinkage = sk,
                    verbose=FALSE)
    list(fit=fit_gbm,iter=gbm.perf(fit_gbm, method="OOB", plot.it = FALSE))
  }
  return(modelList)
}

stopImplicitCluster() 
cl <- makeCluster(detectCores(logical = F))
registerDoParallel(cl)


# feats=dat_train$feature
# labs=dat_train$label
# n=1.5e6

# ### data prep
# feats1=feats[,-1]%>%unlist()%>%matrix(nrow=n)
# labs1=labs[,-1]%>%unlist()%>%matrix(nrow=n)
# 
# feats2=array(feats1,c(dim(feats)[1],8,3))
# labs2=array(labs1,c(dim(labs)[1],4,3))

# debug input:
# load("../output/feature_train.RData")
# dat_train = dat_train_all$feature
# label_train = dat_train_all$label
# depth = 1
# i = 1

### train
XGBTr<- function(dat_train, label_train, par=NULL){
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 1
  } else {
    depth <- par$depth
  }
  k1=rep(1:4,3)
  k2=rep(1:3,each=4)
  modelList<-foreach(i=1:12,.packages="xgboost") %dopar%{ 
    
    featMat <- dat_train[, , k2[i]]
    labMat <- label_train[, k1[i], k2[i]]
    # bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, 
    #                      eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
    fit_xgb = xgboost(data = featMat, label = labMat,
                      max.depth = depth, eta = 0.8, nthread = 2, nrounds = 3, objective = "reg:linear")
    
    modelList[[i]] <- list(fit=fit_xgb)
  }
  return(modelList)
}

### run
# system.time(
#   train(feats2[1:100000,,],labs2[1:100000,,])->results
# )


#stopImplicitCluster()


###TA's

# train_o <- function(dat_train, label_train, par=NULL){
#   
#   ### Train a Gradient Boosting Model (GBM) using processed features from training images
#   
#   ### Input: 
#   ###  -  features from LR images 
#   ###  -  responses from HR images
#   ### Output: a list for trained models
#   
#   ### load libraries
#   library("gbm")
#   
#   ### creat model list
#   modelList <- list()
#   
#   ### Train with gradient boosting model
#   if(is.null(par)){
#     depth <- 3
#   } else {
#     depth <- par$depth
#   }
#   
#   ### the dimension of response array is * x 4 x 3, which requires 12 classifiers
#   ### this part can be parallelized
#   for (i in 1:12){
#     ## calculate column and channel
#     c1 <- (i-1) %% 4 + 1 ####123412341234
#     c2 <- (i-c1) %/% 4 + 1 ####111122223333
#     featMat <- dat_train[, , c2] ### all feat in this channel
#     labMat <- label_train[, c1, c2] #### all data in this column and chanel
#     fit_gbm <- gbm.fit(x=featMat, y=labMat,
#                        n.trees=200,
#                        distribution="gaussian",
#                        interaction.depth=depth, 
#                        bag.fraction = 0.5,
#                        verbose=FALSE)
#     best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
#     modelList[[i]] <- list(fit=fit_gbm, iter=best_iter)
#   }
#   
#   return(modelList)
# }

# system.time(
#   train_o(feats2[1:100000,,],labs2[1:100000,,])->results_o
# )

# predict(results_o[[1]]$fit,newdata=imgLR[,,1],n.trees=200, type="response")->predk1
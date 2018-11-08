#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3


#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3
#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3
set.seed(357)
library(dplyr)
zero_pad <- function(array3)
{
  dim1 <- dim(array3)[1]
  dim2 <- dim(array3)[2]
  new_array <- array(0,c(dim1+2, dim2+2, 3))
  for (i in 1:3)
  {
    new_array[2:(dim1+1), 2:(dim2+1),i] <- array3[,,i]
  }
  return(new_array)
}

findNeighb <- function(ind,mat)
{
  s=ind[1];t=ind[2]
  neigh1 <- mat[s-1, t-1] - mat[s, t]
  neigh2 <- mat[s-1, t] - mat[s, t]
  neigh3 <- mat[s-1, t+1] - mat[s, t]
  neigh4 <- mat[s, t-1] - mat[s,t]
  neigh5 <- mat[s, t+1] - mat[s , t]
  neigh6 <- mat[s+1, t-1] - mat[s , t]
  neigh7 <- mat[s+1, t] - mat[s , t]
  neigh8 <- mat[s+1, t+1] - mat[s , t]
  return(c(neigh1, neigh2, neigh3, neigh4, neigh5, neigh6, neigh7, neigh8))}


label <- function(ind, matHR,matLR){
  s=ind[1];t=ind[2]
  lab=c(matHR[2*s-1, 2*t-1], matHR[2*s-1, 2*t], matHR[2*s, 2*t-1], matHR[2*s, 2*t])-matLR[s,t]
  return(lab)
}





feature <- function(LR_dir, HR_dir, n_points=1500){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  #n_files=1
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgLR<-as.array(imgLR)
    imgHR<-as.array(imgHR) 
    ### step 1. sample n_points from imgLR
    x <- sample(1:(dim(imgLR)[1]), n_points, replace = TRUE)
    y <- sample(1:(dim(imgLR)[2]), n_points, replace = TRUE)
    inds <- cbind(x, y)
    
    imgLR_zero <- zero_pad(imgLR)
    ###
    
    
    for(k in 1:3){
      featMat[((i-1)*1500+1):(i*1500),,k] <-  apply(inds+1,1, findNeighb,imgLR_zero[,,k] )%>%t()
      labMat[((i-1)*1500+1):(i*1500),,k] <- apply(inds, 1, label, imgHR[,,k],imgLR[,,k])%>%t()
      
    }
    ### step 2. for each sampled point in imgLR,
    
    
    
    ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    
    ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
    
    ### step 3. repeat above for three channels
    
  }
  return(list(feature = featMat, label = labMat))
}





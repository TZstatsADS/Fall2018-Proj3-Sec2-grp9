########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3
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
reallocate_single <- function(index, row_rep, col_rep)
{
  row1=row_rep[index[1]]
  row=2*(row1-1) + rep(1:2, each=2)[index[2]]
  col1=col_rep[index[1]]
  col=2*(col1-1)+rep(1:2,2)[index[2]]
  return(c(row, col))
}

reallocate <- function(preArray, lh, lw){
  row_rep <- rep(1:lh, each=lw)
  col_rep <- rep(1:lw, lh)
  inds = expand.grid(1:4, 1:(lh*lw))[,2:1]%>% as.matrix()
  
  imgArray <- array(NA, c(2*lh,2*lw, 3))
  for (i in 1:(lh*lw*4)){
    a <- inds[i,1]
    b <- inds[i,2]
    index1 <- reallocate_single(inds[i,], row_rep , col_rep)
    a1 <- index1[1]
    b1 <- index1[2]
    imgArray[a1,b1,] <- preArray[a,b,]
  }
  return(imgArray)
}





superResolution <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    x <- dim(imgLR)[1]
    y <- dim(imgLR)[2]
    imgLR_zero <- zero_pad(imgLR)
    
    
    
    inds <- expand.grid(2:(dim(imgLR_zero)[1]-1), 2:(dim(imgLR_zero)[2]-1)) %>% 
      as.matrix()
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    for (k in 1:3) {
      featMat[,,k] <- apply(inds, 1, findNeighb,imgLR_zero[,,k] )%>%t()
      
    }
    
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    ### step 3. recover high-resolution from predMat and save in HR_dir
    imageArr <- array(NA, c(dim(predMat)[1],4,3))
    imageArr_central  <- as.numeric(imgLR)
    for (i in 1:3 ){
      imageArr[,,i] <- predMat[,,i] +imageArr_central[((i-1)*dim(predMat)[1]+1):(dim(predMat)[1]*i)]
    }
    
    recover <-  Image(transpose(reallocate(imageArr, dim(imgLR)[2], dim(imgLR)[1])), colormode = Color)
    
    writeImage(recover,pathHR)
  }
  
}



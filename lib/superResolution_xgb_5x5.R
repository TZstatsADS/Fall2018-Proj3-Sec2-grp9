library(dplyr)
library(foreach)

zero_pad2 <- function(array3)
{
  dim1 <- dim(array3)[1]
  dim2 <- dim(array3)[2]
  new_array <- array(0,c(dim1+4, dim2+4, 3))
  for (i in 1:3)
  {
    new_array[3:(dim1+2), 3:(dim2+2),i] <- array3[,,i]
  }
  return(new_array)
}


findCNeighb <- function(ind,mat)
{
  s=ind[1];t=ind[2]
  center=mat[s,t]
  neigh1 <- mat[s-1, t-1] - mat[s, t]
  neigh2 <- mat[s-1, t] - mat[s, t]
  neigh3 <- mat[s-1, t+1] - mat[s, t]
  neigh4 <- mat[s, t-1] - mat[s,t]
  neigh5 <- mat[s, t+1] - mat[s , t]
  neigh6 <- mat[s+1, t-1] - mat[s , t]
  neigh7 <- mat[s+1, t] - mat[s , t]
  neigh8 <- mat[s+1, t+1] - mat[s , t]
  return(c(center,neigh1, neigh2, neigh3, neigh4, neigh5, neigh6, neigh7, neigh8))}


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




XGBsuperResolution <- function(LR_dir, 
                               SR_dir, modelList){
  library("EBImage")
# n_files <- length(imgs)
  n_files <- length(list.files(LR_dir))
  for(i in 1:n_files){
#    t1 = proc.time()
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pathHR <- paste0(HR_dir, "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 25, 3))
    x <- dim(imgLR)[1]
    y <- dim(imgLR)[2]
    imgLR <- zero_pad2(imgLR)
    inds <- expand.grid(3:(dim(imgLR)[2]-2), 3:(dim(imgLR)[1]-2))[,2:1] %>% 
      as.matrix()
    for (k in 1:3) {
      featMat[,,k] <- apply(inds, 1, findCNeighb5x5,imgLR[,,k] )%>%t()
    }
#    t2 = proc.time()
#    tm_feature = tm_feature + t2-t1
    predMat <- XGBtest(modelList, featMat)
    #prearray <- array(predMat, c(dim(imgLR)[1]*2,dim(imgLR)[2]*2,3))
    recover <- reallocate(predMat, x, y)
    img <- Image(recover, colormode=Color)
    writeImage(img,pathSR)
    }
}


# ##############run##################
# LR_dir <- 'C:/Users/wang1/Documents/GR 5243/Fall2018-Proj3-Sec2-grp9 xgb/data/test_set/LR/'
# HR_dir <- 'C:/Users/wang1/Documents/GR 5243/Fall2018-Proj3-Sec2-grp9 xgb/data/test_set/HR/'
# 
# st_super <- system.time(XGBsuperResolution(LR_dir, HR_dir, modelList))
# st_super


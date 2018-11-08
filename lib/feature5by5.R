require(dplyr)

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


findNeighb5x5 <- function(ind,mat)
{
  s=ind[1];t=ind[2]
  c=mat[s,t]
  u=mat[(s-2):(s-1),(t-2):(t+2)]%>%t()%>%as.numeric()
  l=mat[s,c((t-2),(t-1),(t+1),(t+2))]%>%as.numeric()
  d=mat[(s+1):(s+2),(t-2):(t+2)]%>%t()%>%as.numeric()
  
  return(c(u,l,d)-c)}


findCNeighb5x5 <- function(ind,mat)
{
  s=ind[1];t=ind[2]
  c=mat[s,t]
  u=mat[(s-2):(s-1),(t-2):(t+2)]%>%t()%>%as.numeric()-c
  l=mat[s,c((t-2),(t-1),(t+1),(t+2))]%>%as.numeric()-c
  d=mat[(s+1):(s+2),(t-2):(t+2)]%>%t()%>%as.numeric()-c
  return(c(c,u,l,d))}


label <- function(ind, matHR,matLR){
  s=ind[1];t=ind[2]
  lab=c(matHR[2*s-1, 2*t-1], matHR[2*s-1, 2*t], matHR[2*s, 2*t-1], matHR[2*s, 2*t])-matLR[s,t]
  return(lab)
}


feature5x5 <- function(LR_dir, HR_dir, n_points=1000){
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  # n_files=1
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 24, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    ### step 1. sample n_points from imgLR
    x <- sample(1:(dim(imgLR)[1]), n_points, replace = TRUE)
    y <- sample(1:(dim(imgLR)[2]), n_points, replace = TRUE)
    inds <- cbind(x, y)
    
    imgLR <- zero_pad2(imgLR)
    imgHR<-as.array(imgHR)
    
    for(k in 1:3){
      featMat[((i-1)*n_points+1):(i*n_points),,k] <-  apply(inds+2, 1, findNeighb5x5,imgLR[,,k] )%>%t()
      labMat[((i-1)*n_points+1):(i*n_points),,k] <- apply(inds, 1, label, imgHR[,,k],imgLR[,,k])%>%t()
      }
    
  }
  return(list(feature5x5 = featMat, label = labMat))
}



###############Run######################

# LR_dir = 'C:/Users/wang1/Documents/GR 5243/Fall2018-Proj3-Sec2-grp9/data/train_set/LR/'
# HR_dir = 'C:/Users/wang1/Documents/GR 5243/Fall2018-Proj3-Sec2-grp9/data/train_set/HR/'
# st_feat <- system.time(features <- feature5x5(LR_dir, HR_dir, n_points=1000))
# st_feat

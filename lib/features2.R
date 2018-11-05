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

label <- function(ind, mat){
  s=ind[1];t=ind[2]
  lab=c(mat[2*s-1, 2*t-1], mat[2*s-1, 2*t], mat[2*s, 2*t-1], mat[2*s, 2*t])
  
  return(lab)
}





feature2 <- function(LR_dir, HR_dir, n_points=1000, imgs){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ###        + a string vector of images associated with the same label
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  
  ####################Temporarily For Label Classified#######################
  # n_files = 500
  
  ### store feature and responses
  n_files <- length(imgs)
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", imgs[i]))
    imgHR <- readImage(paste0(HR_dir,  "img_", imgs[i]))

    imgLR <- zero_pad(imgLR)
    imgHR <- zero_pad(imgHR)
    x <- sample(2:(dim(imgLR)[1]-1), n_points, replace = TRUE)
    y <- sample(2:(dim(imgLR)[2]-1), n_points, replace = TRUE)
    inds <- cbind(x, y)

    for(k in 1:3){
      featMat[((i-1)*1000+1):(i*1000),,k] <-  apply(inds, 1, findNeighb,imgLR[,,k] )
      labMat[((i-1)*1000+1):(i*1000),,k] <- apply(inds, 1, label, imgHR[,,k])
    }
  }
  return(list(feature = featMat, label = labMat))
}


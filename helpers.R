# import libraries
library(EBImage)
library(tiff)
library(ijtiff)
library(plotly)
library(dygraphs)
library(xts)


# change contrast 
scaling <- function(img, vl, vh){
  new_img <- pmax(pmin((img - vl)/(vh - vl), 1), 0)
}


# return the mean intensity value of ROI
seg <- function(img, xmin, xmax, ymin, ymax, nFrame){
  cropImg <- list()
  for(i in c(1:nFrame)){
    cropImg[[i]] <- img[ymin:ymax, xmin:xmax, ,i]
  }
  
  return(lapply(cropImg, mean))
  
}












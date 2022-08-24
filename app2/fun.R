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


# plot the cross correlation 
ccf_ly <- function(series1= NULL, series2= NULL){
  series <- c(series1, series2)
  p_ccf <-ccf(series1, series2, lag = 60, plot=F)
  df_ccf <- with(p_ccf, data.frame(lag, acf))
  offset <- which.max(df_ccf$acf) - ceiling(nrow(df_ccf)/2) 
  ccf_plotly <- plotly::plot_ly(df_ccf, x = ~lag, y = ~acf, type = "scatter", showlegend = FALSE, mode = 'lines', name = "LAG, ACF") %>%
    plotly::layout( xaxis = list( title = 'LAG', showgrid = FALSE), yaxis = list( title = 'CCF', showgrid = FALSE)) %>%
    layout(title = list(text = sprintf("Offset = %s frame\nS1 leads <> S2 leads", offset), x= 0.5, xref = 'paper')) %>%
    plotly::add_lines(x = 0, line = list(color = 'black', dash = 'dot'),
                      name = "center", showlegend = TRUE)  %>%
    plotly::add_lines(x = offset, line = list(color = 'orange', dash = 'dot'),
                      name = "peak synchrony", showlegend = TRUE)
  ccf_plotly
  return(ccf_plotly)
}


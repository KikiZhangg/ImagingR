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
ccf_ly <- function(series1 = NULL, series2 = NULL){
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


# power spectrum

power_spectrum <- function(series1 , series2, series3){
  series1 <- series1 - mean(series1)
  series2 <- series2 -mean(series2)
  series3<- series3 - mean(series3)
  n <- length(series1)
  fs <- 1/60
  f <- (seq_len(n)-1)*fs/n
  dft_amp1 <- abs(fft(series1, inverse = FALSE))
  dft_amp2 <- abs(fft(series2, inverse = FALSE))
  dft_amp3 <- abs(fft(series3, inverse = FALSE))
  power1 <- (dft_amp1^2)/n
  power2 <- (dft_amp2^2)/n
  power3 <- (dft_amp3^2)/n
  power <- data.frame(channel = rep(c("Neuronal", "CBV", "CBF"), each = n),
                      frequency = rep(f[1:n/2+1], 3),
                      power = c(power1[1:n/2+1], power2[1:n/2+1], power3[1:n/2+1]))
  
  freq_plot <- ggplot(data = power, mapping = aes(x = frequency, y = power, color = channel))+
    geom_line()+
    labs(x = "Frequency (Hz)", y = "Power",
         title = "Power Spectrum")

  return(ggplotly(freq_plot))
}




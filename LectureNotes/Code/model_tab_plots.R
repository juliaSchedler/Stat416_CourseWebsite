models_tabs<- function(x_t, ma_order, one_plot = T, output_data = F){
  if(one_plot){par(mfrow = c(2,3))}
  png("image.png")
  tsplot(x_t)
  dev.off()
  
  img <- readPNG("image.png")
  p1<- ggplot(iris, aes(Species, Sepal.Length)) + xlab("") + ylab("")+
    background_image(img)

  ## number 2
  ma_xt <- stats::filter(x_t, filter = rep(1/ma_order, ma_order), method = "convolution")
  png("image.png")
  tsplot(x_t)
  lines(ma_xt, col = "blueviolet", lwd = 2)
  dev.off()
  
  img <- readPNG("image.png")
  p2<- ggplot(iris, aes(Species, Sepal.Length)) + xlab("") + ylab("")+
    background_image(img)
  
  # number 3
  detrended_ma <- x_t - ma_xt
  png("image.png")
  tsplot(detrended_ma)
  dev.off()
  
  img <- readPNG("image.png")
  p3<- ggplot(iris, aes(Species, Sepal.Length)) + xlab("") + ylab("")+
    background_image(img)
  
  # Number 4
  time_index <- 1:length(x_t)
  lm_xt <- lm(x_t~time_index)
  png("image.png")
  tsplot(x_t)
  reg_xt <- lm_xt$fitted.values
  lines(reg_xt, col = "turquoise", lwd = 2)
  dev.off()
  
  img <- readPNG("image.png")
  p4<- ggplot(iris, aes(Species, Sepal.Length)) + xlab("") + ylab("")+
    background_image(img)
  
  # number 5
  detrended_reg <- x_t - reg_xt
  png("image.png")
  tsplot(detrended_reg)
  dev.off()
  
  img <- readPNG("image.png")
  p5<- ggplot(iris, aes(Species, Sepal.Length)) + xlab("") + ylab("")+
    background_image(img)
  
  # Number 6
  differenced <- c(diff(x_t), NA)
  png("image.png")
  tsplot(differenced)
  dev.off()
  
  img <- readPNG("image.png")
  p6<- ggplot(iris, aes(Species, Sepal.Length)) + xlab("") + ylab("")+
    background_image(img)
  
  return(p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(nrow = 2, ncol = 3))
  if(one_plot){par(mfrow = c(1,1))}
  
  if(output_data){
    return(data.frame(x_t, ma_xt, detrended_ma, 
                      reg_xt, detrended_reg, differenced))
  }
}
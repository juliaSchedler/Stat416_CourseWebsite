models<- function(x_t, ma_order, one_plot = T, output_data = F){
  if(one_plot){par(mfrow = c(2,3))}
  # number 1
  tsplot(x_t)
  
  ## number 2
  ma_xt <- stats::filter(x_t, filter = rep(1/ma_order, ma_order), method = "convolution")
  tsplot(x_t)
  lines(ma_xt, col = "blueviolet", lwd = 2)
  
  # number 3
  detrended_ma <- x_t - ma_xt
  tsplot(detrended_ma)
  
  # Number 4
  time_index <- 1:length(x_t)
  lm_xt <- lm(x_t~time_index)
  tsplot(x_t)
  reg_xt <- lm_xt$fitted.values
  lines(reg_xt, col = "turquoise", lwd = 2)
  
  # number 5
  detrended_reg <- x_t - reg_xt
  tsplot(detrended_reg)

  # Number 6
  differenced <- c(diff(x_t), NA)
  tsplot(differenced)
  dev.off()

  if(one_plot){par(mfrow = c(1,1))}
  
  if(output_data){
    return(data.frame(x_t, ma_xt, detrended_ma, 
                    reg_xt, detrended_reg, differenced))
  }
}
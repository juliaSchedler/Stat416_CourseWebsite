process_ts <- function(raw_ts, ptitle, output = F, plots = T){
  
  par(mfrow = c(2,3))
  x_t <- raw_ts
  all_ts <- data.frame(x_t, Time = time(x_t))
  # number 1
  if(plots){
    tsplot(all_ts$x_t, type = "b", ylab = paste(ptitle), main = "Observed Time Series", pch = 16)}
  
  ## number 2
  all_ts$ma_x_t <- stats::filter(x_t, filter =rep(1/5,5), sides = 2)
  if(plots){
    tsplot(all_ts$x_t, type = "b", ylab =paste(ptitle), main = "Moving Average Smoother (5 point)", pch = 16 )
    lines(all_ts$ma_x_t, col = "magenta", lwd = 2)}
  
  # number 3
  all_ts$detrended_ma <- all_ts$x_t - all_ts$ma_x_t
  if(plots){
    tsplot(all_ts$detrended_ma, type = "b",ylab =paste(ptitle), main = "Detrended - Moving Average Smoother (5 point)", pch = 16 )}
  
  # Number 4
  lm_x_t <- lm(x_t~Time, data = all_ts)
  all_ts$reg_x_t <- lm_x_t$fitted.values
  if(plots){
    tsplot(all_ts$x_t, type = "b",ylab = paste(ptitle), main = "Linear Trend", pch = 16)
    lines(all_ts$reg_x_t, col = "turquoise", lwd = 2)
  }
  # number 5
  
  all_ts$detrended_reg <- all_ts$x_t - all_ts$reg_x_t
  if(plots){
    tsplot(all_ts$detrended_reg, type = "b",ylab = paste(ptitle), main = "Detrended - Linear Trend", pch = 16)}
  
  # Number 6
  all_ts$differenced <- c(diff(all_ts$x_t), NA)
  if(plots){
    tsplot(all_ts$differenced, type = "b",ylab = paste(ptitle), main = "Differenced", pch = 16 )}
  
  par(mfrow = c(1,1))
  
  if(output){return(all_ts)}
}
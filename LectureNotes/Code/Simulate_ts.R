set.seed(0016)
N <- 100
png("image.png")
## Simulate white noise.
y1 <- rnorm(N)

## Simulate an MA(3) 
w <- rnorm(N+50)
y2 <- stats::filter(w, filter = rep(1/3,3), 
                    method = "convolution", sides = 2)[2:101]
## Simulate an MA(5)
w <- rnorm(N+50)
y3 <- stats::filter(w, filter = rep(1/5,5), 
                     method = "convolution", sides = 2)[10:109]

## Simulate trend stationary
w <- rnorm(N)
time_index <- 1:N
y4 <- 2.718 + 1.96*time_index + w

## real data 
#devtools::install_github("Fish-Forecast/FishForecast")
#library(FishForecast)
data(Nile, package = "datasets")
y5 <- as.vector(Nile)

time_series <- data.frame(y1, y2, y3, y4, y5)
write.csv(time_series, file = "../Data/time_series.csv", row.names = F)

# simulate a cos plus noise
w <- rnorm(N, sd = 1)
y_5 <- cos((2*pi/12)*1:N) + w
tsplot(y_5)
acf(y_5)

## fit ma_5 and plot
ma_5_y_5<-stats::filter(y_5, filter = rep(1/5,5), method = "convolution")
tsplot(y_5)
lines(ma_5_y_5, col = "red")

## detrend using ma_5
detrend_ma_y5 <- y_5 - ma_5_y_5
tsplot(detrend_ma_y5)

## acf
acf(detrend_ma_y5, na.action = na.pass)
pacf(detrend_ma_y5, na.action = na.pass)

## Simulate arima model
w<- rnorm(N*2)
y_6 <- stats::filter(w, filter = c(1.5, -0.75), method = "recursive")
tsplot(y_6)
acf(y_6)

## fit ma_5 and plot
ma_5_y_6<-stats::filter(y_6, filter = rep(1/5,5), method = "convolution")
tsplot(y_6)
lines(ma_5_y_6)

## detrend using ma_5
detrend_ma_y6 <- y_6 - ma_5_y_6
tsplot(detrend_ma_y6)

## acf
acf(detrend_ma_y6, na.action = na.pass)

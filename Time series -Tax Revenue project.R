## USE FORECAST AND ZOO LIBRARIES.

library(forecast)
library(zoo)
#-------------------------------------------------------------------------------
# Set working directory for locating files.
setwd("/Users/vinmathi/Documents/Ban 673")

# Create data frame.
Tax.data <- read.csv("Financial Report.csv")
# See the first 6 records of the file.
tail(Tax.data)
Tax.data$Revenue
#-------------------------------------------------------------------------------
## CREATE TIME SERIES DATA SET.

Tax.ts <- ts(Tax.data$Revenue, 
                  start = c(2009, 1), end = c(2022, 4), freq = 4)
head(Tax.ts)
tail(Tax.ts)
#-------------------------------------------------------------------------------
# Use plot() to plot time series data  
plot(Tax.ts, 
     xlab = "Time", ylab = "Taxes (in millions)", 
     ylim = c(250000, 550000), xaxt = 'n',
     main = "Tax")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(2009, 2023, 1), labels = format(seq(2009, 2023, 1)))
#-------------------------------------------------------------------------------
#Predictability test
# Use Arima() function to fit AR(1) model 

Tax.ar1<- Arima(Tax.ts, order = c(1,0,0))
summary(Tax.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.9781
s.e. <- 0.0244
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}
#-------------------------------------------------------------------------------
# Create first differenced Shipment data using lag1.
diff.Tax.ts <- diff(Tax.ts, lag = 1)
diff.Tax.ts
#-------------------------------------------------------------------------------
# Use Acf() function to identify autocorrealtion for first differenced 
# shipment data , and plot autocorrelation for different lags 

Acf(diff.Tax.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced Tax data")
#-------------------------------------------------------------------------------
# Use stl() function to plot times series components of the original data. 

Tax.stl <- stl(Tax.ts, s.window = "periodic")
autoplot(Tax.stl, main = "Tax Time Series Components")
#-------------------------------------------------------------------------------
# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags.
autocor <- Acf(Tax.ts, lag.max = 8, 
               main = "Autocorrelation for Tax")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)
#-------------------------------------------------------------------------------
## CREATE DATA PARTITION.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 15
nTrain <- length(Tax.ts) - nValid
train.ts <- window(Tax.ts, start = c(2009, 1), end = c(2009, nTrain))
valid.ts <- window(Tax.ts, start = c(2009, nTrain + 1), 
                   end = c(2009, nTrain + nValid))
train.ts
valid.ts
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Models that we tried for training and validation period
#-------------------------------------------------------------------------------
#MODEL 1 - Trailing MA 
#-------------------------------------------------------------------------------
## CREATE TRAILING MA FOR VARIOUS WINDOWS (NUMBER OF PERIODS).

# Create trailing MA with window widths (number of periods) 
# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_8 <- rollmean(train.ts, k = 8, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

# Use head() function to show training MA 

head(ma.trailing_12)
tail(ma.trailing_12)

# Create forecast for the validation data for the window widths 

ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_4.pred
ma.trail_8.pred <- forecast(ma.trailing_8, h = nValid, level = 0)
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred

# Plot ts data, linear trend, and predictions for validation period.
plot(ma.trail_8.pred$mean, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 520000),
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "Model with Trailing MA", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)) )
lines(ma.trail_8.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 550000))
lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Training")
text(2022, 450000, "Validation")

arrows(2009, 440000, 2018.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 440000, 2022.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_8.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)
#-------------------------------------------------------------------------------
## Two level forecast - MODEL 2
#LINEAR TREND AND SEASONALITY + Trailing MA for residuals

# Fit a regression model with linear trend and seasonality for
# training partition. 
train.lin.season <- tslm(train.ts ~ trend + season)
summary(train.lin.season)

# Create regression forecast with trend and seasonality for 
# validation period.
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Identify and display regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
trend.seas.res <- trend.seas$residuals
trend.seas.res

# Apply trailing MA for residuals with window width k = 4
# for training partition.
ma.trail.res <- rollmean(trend.seas.res, k = 4, align = "right")
ma.trail.res

# Regression residuals in validation period.
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

# Create a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(valid.df) <- c("Tax", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)
#-------------------------------------------------------------------------------
## MODEL 3 - FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: 

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)

round(accuracy(train.lin.season.pred$mean, valid.ts),3)
#-------------------------------------------------------------------------------
## MODEL 4 - FIT REGRESSION MODEL WITH LINEAR TREND 
train.lin <- tslm(train.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)

# Plot ts data, linear trend, and predictions for validation period.
plot(train.lin.pred$mean, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 520000),
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "Regression Model with Linear Trend", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 550000))
lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Training")
text(2022, 450000, "Validation")

arrows(2009, 440000, 2018.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 440000, 2022.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use accuracy() function to identify common accuracy measures with rounded
# values to 3 decimals.
round(accuracy(train.lin.pred$mean, valid.ts), 3)

#-------------------------------------------------------------------------------
## MODEL 5 - HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATIC
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 520000),
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)) )
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 550000))
lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Training")
text(2022, 450000, "Validation")

arrows(2009, 440000, 2018.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 440000, 2022.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#accuracy
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

#-------------------------------------------------------------------------------
# MODEL 6 - Model with trend and seasonality and AR model for residuals

Acf(train.lin.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Tax Training Residuals")
Acf(valid.ts - train.lin.season.pred$mean, lag.max = 8, 
    main = "Autocorrelation for Tax Validation Residuals")

## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- round(data.frame(train.ts, train.lin.season$fitted, 
                             train.lin.season$residuals, res.ar1$fitted, res.ar1$residuals), 3)
names(train.df) <- c("Tax", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 

Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Tax Training Residuals of Residuals")

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.
valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Tax", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(valid.two.level.pred, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 520000),
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "Two-Level Forecast: Regression with Trend
     and Seasonlity + AR(1) for Residuals", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)) )
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 550000))
lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Training")
text(2022, 450000, "Validation")

arrows(2009, 440000, 2018.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 440000, 2022.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) two-level model (linear trend and seasonal model + AR(1) model for residuals),

round(accuracy(valid.two.level.pred, valid.ts), 3)
#-------------------------------------------------------------------------------

## MODEL 7 - FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND: 

# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

# Plot ts data, regression with quadratic trend and forecast for validation period.
plot(train.quad.pred$mean, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 520000),
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "Model with Quadratic trend", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)) )
lines(train.quad.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 550000))
lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Training")
text(2022, 450000, "Validation")

arrows(2009, 440000, 2018.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 440000, 2022.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for regression models with linear trend and quadratic (polynomial) trend.

round(accuracy(train.quad.pred$mean, valid.ts), 3)

#-------------------------------------------------------------------------------
## MODEL 8 - FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: 

# Use tslm() function to create quadratic trend and seasonal model.
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.quad.season.pred$mean, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 520000),
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "Regression Model with Quadratic Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)) )
lines(train.quad.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 550000))
lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Training")
text(2022, 450000, "Validation")

arrows(2009, 440000, 2018.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 440000, 2022.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#accuracy
round(accuracy(train.quad.season.pred$mean, valid.ts),3)
#-------------------------------------------------------------------------------
## MODEL 9 - FIT REGRESSION MODEL WITH EXPONENTIAL TREND

train.expo <- tslm(train.ts ~ trend, lambda = 0)

# See summary of exponential trend model and associated parameters.
summary(train.expo)

# Apply forecast() function to make forecast using exponential  
# trend for validation period.  
train.expo.pred <- forecast(train.expo, h = nValid, level = 0)

#Plot
plot(train.expo.pred$mean,
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 520000),
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "Regression Models Exponential Trends", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)) )
lines(train.expo.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lty = 1)
lines(train.ts, col = "black", lty = 1)
lines(c(2019, 2019), c(0, 550000))
lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Training")
text(2022, 450000, "Validation")

arrows(2009, 440000, 2018.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 440000, 2022.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#accuracy
round(accuracy(train.expo.pred$mean, valid.ts), 3)
#-------------------------------------------------------------------------------
## MODEL 10 - FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.

train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 8, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot .
plot(train.auto.arima.pred$mean, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 520000),
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "Auto ARIMA model", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)) )

lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)

lines(c(2019, 2019), c(0, 550000))
lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Training")
text(2022, 450000, "Validation")

arrows(2009, 440000, 2018.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 440000, 2022.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#accuracy 
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

#-------------------------------------------------------------------------------
## MODEL 11 - FIT ARIMA (1,1,1)(1,1,1) MODEL.

# Use Arima() function to fit ARIMA(1,1,1)(1,1,1) model for 
# trend and seasonality.

train.arima.seas <- Arima(train.ts, order = c(1,1,1), 
                          seasonal = c(1,1,1)) 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

# Use Acf() function to create autocorrelation chart of ARIMA(1,1,1)(1,1,1) 
# model residuals.
Acf(train.arima.seas$residuals, lag.max = 8, 
    main = "Autocorrelations of ARIMA(1,1,1)(1,1,1) Model Residuals")

# Plot .
plot(train.arima.seas.pred$mean, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 520000),
     bty = "l", xlim = c(2009, 2024), xaxt = "n",
     main = "Auto ARIMA model", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2009, 2024, 1), labels = format(seq(2009, 2024, 1)) )

lines(train.arima.seas.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)

lines(c(2019, 2019), c(0, 550000))
lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Training")
text(2022, 450000, "Validation")

arrows(2009, 440000, 2018.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 440000, 2022.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(train.arima.seas.pred$mean, valid.ts), 3)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
## ENTIRE DATASET 
#-------------------------------------------------------------------------------
#MODEL 1 - QUADRATIC TREND

# Use tslm() function to create quadratic (polynomial) trend model.
tot.quad <- tslm(Tax.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(tot.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
tot.quad.pred <- forecast(tot.quad, h = 8, level = 0)

#plot
plot(Tax.ts, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 550000),
     bty = "l", xlim = c(2009, 2025), xaxt = "n",
     main = "Quadratic trend Model for Entire Data Set", 
     lty = 2, lwd = 2, col = "black")
axis(1, at = seq(2009, 2025, 1), labels = format(seq(2009, 2025, 1)) )
lines(tot.quad$fitted, col = "blue", lwd = 2)
lines(tot.quad.pred$mean, col = "blue", lty = 5, lwd = 2)

# plot on the chart vertical lines and horizontal arrows

lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Historical")
text(2024, 450000, "Future")

arrows(2009, 440000, 2022.5, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 440000, 2024.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#-------------------------------------------------------------------------------
#MODEL 2 - QUADRATIC TREND + SEASONALITY

# Use tslm() function to create quadratic trend and seasonal model.
tot.quad.season <- tslm(Tax.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(tot.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
tot.quad.season.pred <- forecast(tot.quad.season, h = 8, level = 0)

#plot
plot(Tax.ts, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 550000),
     bty = "l", xlim = c(2009, 2025), xaxt = "n",
     main = "Quadratic trend and seasonality Model for Entire Data Set", 
     lty = 2, lwd = 2, col = "black")
axis(1, at = seq(2009, 2025, 1), labels = format(seq(2009, 2025, 1)) )
lines(tot.quad.season$fitted, col = "blue", lwd = 2)
lines(tot.quad.season.pred$mean, col = "blue", lty = 5, lwd = 2)

# plot on the chart vertical lines and horizontal arrows

lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Historical")
text(2024, 450000, "Future")

arrows(2009, 440000, 2022.5, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 440000, 2024.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#-------------------------------------------------------------------------------
#MODEL 3 - EXPONENTAIL  TREND 

# Use tslm() function to create linear trend model.
tot.expo<- tslm(Tax.ts ~ trend, lambda = 0)

# See summary of linear trend equation and associated parameters.
summary(tot.expo)

# Apply forecast() function to make predictions for ts with 

tot.expo.pred <- forecast(tot.expo, h = 8, level = 0)

#Plot 
plot(Tax.ts, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 550000),
     bty = "l", xlim = c(2009, 2025), xaxt = "n",
     main = "Exponentail trend for Entire Data Set", 
     lty = 2, lwd = 2, col = "black")
axis(1, at = seq(2009, 2025, 1), labels = format(seq(2009, 2025, 1)) )
lines(tot.expo$fitted, col = "blue", lwd = 2)
lines(tot.expo.pred$mean, col = "blue", lty = 5, lwd = 2)

# plot on the chart vertical lines and horizontal arrows

lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Historical")
text(2024, 450000, "Future")

arrows(2009, 440000, 2022.5, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 440000, 2024.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#-------------------------------------------------------------------------------
#MODEL 4 - ARIMA MODEL
# Use arima() function to fit seasonal ARIMA(1,1,1)(1,1,1) model 
# for entire data set.

arima.seas <- Arima(Tax.ts, order = c(1,1,1), 
                    seasonal = c(1,1,1)) 
summary(arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 8 periods. 
arima.seas.pred <- forecast(arima.seas, h = 8, level = 0)
arima.seas.pred

# Use Acf() function to create autocorrelation chart of seasonal ARIMA 
# model residuals.
Acf(arima.seas$residuals, lag.max = 8, 
    main = "Autocorrelations of Seasonal ARIMA (1,1,1)(1,1,1) Model Residuals")

# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 8 future periods.
plot(Tax.ts, 
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 550000),
     bty = "l", xlim = c(2009, 2025), xaxt = "n",
     main = "Seasonal ARIMA(1,1,1)(1,1,1)[4] Model for Entire Data Set", 
lty = 2, lwd = 2, col = "black")
axis(1, at = seq(2009, 2025, 1), labels = format(seq(2009, 2025, 1)) )
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "blue", lty = 5, lwd = 2)

# plot on the chart vertical lines and horizontal arrows

lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Historical")
text(2024, 450000, "Future")

arrows(2009, 440000, 2022.5, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 440000, 2024.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#-------------------------------------------------------------------------------
#AUTO ARIMA - Model 5 
#-------------------------------------------------------------------------------
# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(Tax.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 

auto.arima.pred <- forecast(auto.arima, h = 8, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 8, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")
#------
# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 8 future periods.
plot(Tax.ts,
     xlab = "Time", ylab = "Tax in millions",ylim = c(250000, 550000),
     bty = "l", xlim = c(2009, 2025), xaxt = "n",
     main = "Auto ARIMA Model for Entire Dataset", 
     lty = 2, lwd = 2, col = "black")
axis(1, at = seq(2009, 2025, 1), labels = format(seq(2009, 2025, 1)) )
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)


lines(c(2022.8, 2022.8), c(0, 550000))
text(2015, 450000, "Historical")
text(2024, 450000, "Future")

arrows(2009, 440000, 2022.5, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 440000, 2024.9, 440000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#-------------------------------------------------------------------------------
#Final accuracy comparison
#-------------------------------------------------------------------------------
round(accuracy(tot.quad.pred$fitted, Tax.ts), 3)
round(accuracy(tot.quad.season.pred$fitted, Tax.ts), 3)
round(accuracy(tot.expo.pred$fitted, Tax.ts), 3)
round(accuracy(arima.seas.pred$fitted, Tax.ts), 3)
round(accuracy(auto.arima.pred$fitted, Tax.ts), 3)
round(accuracy((snaive(Tax.ts))$fitted, Tax.ts), 3)
round(accuracy((naive(Tax.ts))$fitted, Tax.ts), 3)

#-------------------------------------------------------------------------------


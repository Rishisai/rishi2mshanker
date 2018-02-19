
#1.) Plotting air time series

st <- Sept11Travel
st1=ts(st$`Air RPM (000s)`, 
   start = c(1990, 1), end = c(2001, 8), freq = 12)
install.packages("forecast")


air.ts <- ts(st$`Air RPM (000s)`, 
                   start = c(1990, 1 ,1), end = c(2001, 8 ,10), freq = 12)
plot(air.ts, xlab = "Time", ylab = "Air miles", ylim = c(29672427, 69003617))

library(dplyr)

air.lm <- forecast::tslm(air.ts ~ trend + I(trend^2))

plot(air.ts, xlab = "Time", ylab = "Air miles", ylim = c(29672427, 69003617))
# overlay the fitted values of the linear model

#trend lines for air miles

lines(air.lm$fitted, lwd = 1)




#2.) Linear regression model with trend is the method adequate for plotting a series as shown in the figure. Since we need seasonally adjusted graph , the other options have seasonality included in them.


#3.)The outcome variables are time and noise. The predictor is previous data which is variable as per time.

#4.)

air.ts <- ts(st$`Air RPM (000s)`, 
             start = c(1990, 1 ,1), end = c(1991, 8 ,10), freq = 12)
plot(air.ts, xlab = "Time", ylab = "Air miles", ylim = c(29672427, 69003617))
library(seasonal)
t <- seas(st1)
plot(t)
final(t)
#The predicted value is starts ar 39 Mil. which is ~4 Million more than the actual value.

#5.)

library(forecast)

air.ts.season<- tslm(air.ts ~ trend+season)
summary(air.ts.season)
Acf(air.ts.season$residuals, lag.max = 12, main="")
#Let us also display partial autocorrelation
Pacf(air.ts.season$residuals, lag.max = 12, main="")


#This was a steady time series till 9/11. The disruption in acf indicated the significant change in air miles driven. due to the event

#6.)

# plot forecasts and actuals in the training and validation sets






nValid <- 32
nTrain <- length(ridership.ts) - nValid
# partition the data
train.ts <- window(air.ts1, start = c(1990, 1), end = c(2001,8))
valid.ts <- window(air.ts1, start = c(1991, nTrain + 1),
                   end = c(1991, nTrain + nValid))


naive.pred <- forecast::naive(train.ts, h = nValid)
snaive.pred <- forecast::snaive(valid.ts, h = nValid)



air.ts1 <- ts(st$`Air RPM (000s)`, 
             start = c(1990, 1), end = c(2004, 4 ), freq = 12)

plot(air.ts1,  ylab = "Ridership", xlab = "Time", bty = "l", 
     xaxt = "n", main = "") 
axis(1, at = seq(1990, 2001, 1), labels = format(seq(1990, 2001, 1)))
lines(naive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(snaive.pred$mean, lwd = 2, col = "red", lty = 1)
lines(valid.ts, col = "grey20", lty = 3)
lines(c(1990 - 3, 2001 - 3), c(0, 3500)) 
lines(c(1990, 2001), c(0, 3500))
text(1990, 1995, "Training")
text(1995, 2001, "Validation")
text(2001, 2004, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)






#rail...................




nValid <- 32
nTrain <- length(ridership.ts) - nValid
# partition the data
train.ts <- window(rail.ts1, start = c(1990, 1), end = c(2001,8))
valid.ts <- window(rail.ts1, start = c(1991, nTrain + 1),
                   end = c(1991, nTrain + nValid))

naive.pred <- forecast::naive(train.ts, h = nValid)
snaive.pred <- forecast::snaive(valid.ts, h = nValid)



rail.ts1 <- ts(st$`Rail PM`, 
              start = c(1990, 1), end = c(2004, 4 ), freq = 12)

plot(rail.ts1,  ylab = "Ridership", xlab = "Time", bty = "l", 
     xaxt = "n", main = "") 
axis(1, at = seq(1990, 2001, 1), labels = format(seq(1990, 2001, 1)))
lines(naive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(snaive.pred$mean, lwd = 2, col = "red", lty = 1)
lines(valid.ts, col = "grey20", lty = 3)
lines(c(1990 - 3, 2001 - 3), c(0, 3500)) 
lines(c(1990, 2001), c(0, 3500))
text(1990, 1995, "Training")
text(1995, 2001, "Validation")
text(2001, 2004, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)




#vmt................


nValid <- 32
nTrain <- length(ridership.ts) - nValid
# partition the data
train.ts <- window(vmt.ts1, start = c(1990, 1), end = c(2001,8))
valid.ts <- window(vmt.ts1, start = c(1991, nTrain + 1),
                   end = c(1991, nTrain + nValid))

naive.pred <- forecast::naive(train.ts, h = nValid)
snaive.pred <- forecast::snaive(valid.ts, h = nValid)



vmt.ts1 <- ts(st$`VMT (billions)`, 
               start = c(1990, 1), end = c(2004, 4 ), freq = 12)

plot(vmt.ts1,  ylab = "Ridership", xlab = "Time", bty = "l", 
     xaxt = "n", main = "") 
axis(1, at = seq(1990, 2001, 1), labels = format(seq(1990, 2001, 1)))
lines(naive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(snaive.pred$mean, lwd = 2, col = "red", lty = 1)
lines(valid.ts, col = "grey20", lty = 3)

text(1990, 1995, "Training")
text(1995, 2001, "Validation")
text(2001, 2004, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)



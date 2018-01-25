

#The study is descriptive since we are only analyzing the existing data and , the project is not performing
#any kind of future predictions of the data . The current data is only about the date beofre 2001 september 11


summary(Sept11Travel)
sapply()
class(Sept11Travel)

st <- Sept11Travel

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




range(st$`Rail PM`)


rail.lm <- forecast::tslm(rail.ts ~ trend + I(trend^2))

rail.ts <- ts(st$`Rail PM`, 
             start = c(1990, 1 ,1), end = c(2001, 8 ,10), freq = 12)
plot(rail.ts, xlab = "Time", ylab = "Rail miles", ylim = c(326874247, 664013874))

#trend lines for rail miles

lines(rail.lm$fitted, lwd = 1)


range(st$`VMT (billions)`)

vmt.lm <- forecast::tslm(vmt.ts ~ trend + I(trend^2))

vmt.ts <- ts(st$`VMT (billions)`, 
              start = c(1990, 1 ,1), end = c(2001, 8 ,10), freq = 12)
plot(vmt.ts, xlab = "Time", ylab = "VMT", ylim = c(153.2554,261.3000))

#trend lines

lines(vmt.lm$fitted, lwd = 1)


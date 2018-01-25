summary(Sept11Travel)
sapply()
hist(Sept11Travel$`VMT (billions)`)
hist(Sept11Travel$`Air RPM (000s)`)
class(Sept11Travel)
data(Sept11Travel)
st <- Sept11Travel

data(st)
range(st$`Air RPM (000s)`)
class(st)
start(st)
class(st)
data(st)
data(Sept11Travel)
plot(st)
st1 <- complete.cases(st)
?na.rm
?rm
plot(st$`Air RPM (000s)`)
plot(st$`Rail PM`)
plot(st$`VMT (billions)`)
install.packages("forecast")


air.ts <- ts(st$`Air RPM (000s)`, 
                   start = c(1990, 1 ,1), end = c(2001, 8 ,10), freq = 12)
plot(air.ts, xlab = "Time", ylab = "Air miles", ylim = c(29672427, 69003617))

library(dplyr)

air.lm <- forecast::tslm(air.ts ~ trend + I(trend^2))

plot(air.ts, xlab = "Time", ylab = "Air miles", ylim = c(29672427, 69003617))
# overlay the fitted values of the linear model
lines(air.lm$fitted, lwd = 1)




range(st$`Rail PM`)


rail.lm <- forecast::tslm(rail.ts ~ trend + I(trend^2))

rail.ts <- ts(st$`Rail PM`, 
             start = c(1990, 1 ,1), end = c(2001, 9 ,10), freq = 12)
plot(rail.ts, xlab = "Time", ylab = "Rail miles", ylim = c(326874247, 664013874))

lines(rail.lm$fitted, lwd = 1)


range(st$`VMT (billions)`)

vmt.lm <- forecast::tslm(vmt.ts ~ trend + I(trend^2))

vmt.ts <- ts(st$`VMT (billions)`, 
              start = c(1990, 1 ,1), end = c(2001, 9 ,10), freq = 12)
plot(vmt.ts, xlab = "Time", ylab = "VMT", ylim = c(153.2554,261.3000))

lines(vmt.lm$fitted, lwd = 1)








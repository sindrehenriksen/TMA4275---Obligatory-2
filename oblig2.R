rm(list=ls())

## ---- preliminary
library(survival)
data <- read.table("data.dat")
attach(data)

smoothSEcurve <- function(yy, xx) {
  # use after a call to "plot"
  # fit a lowess curve and 95% confidence interval curve
  # make list of x values
  xx.list <- min(xx) + ((0:100)/100)*(max(xx) - min(xx))
  # Then fit loess function through the points (xx, yy)
  # at the listed values
  yy.xx <- predict(loess(yy ~ xx), se=T,
                   newdata=data.frame(xx=xx.list))
  lines(yy.xx$fit ~ xx.list, lwd=2)
  lines(yy.xx$fit -
          qt(0.975, yy.xx$df)*yy.xx$se.fit ~ xx.list, lty=2)
  lines(yy.xx$fit +
          qt(0.975, yy.xx$df)*yy.xx$se.fit ~ xx.list, lty=2)
}

## ---- 1
coxreg <- coxph(Surv(y, delta)~1)
mres <- residuals(coxreg, type="martingale")

plot(x1, mres)
smoothSEcurve(mres, x1)
plot(log(x1), mres)
smoothSEcurve(mres, log(x1))

plot(x2, mres)
smoothSEcurve(x2, mres)

## ---- 2
coxreg2 <- coxph(Surv(y,delta)~x2)
sres2 <- residuals(coxreg2,type="schoenfeld")
failure_times <- sort(y[delta==1])
failure_times <- failure_times[failure_times != 0]
plot(failure_times,sres2)
smoothSEcurve(sres2,failure_times)

Rhat_0 <- survfit(Surv(y) ~ x2, subset={x2==0})
n_0 = length(Rhat_0$surv)
Rhathat_0 <- (tail(Rhat_0$surv, n_0-1) + head(Rhat_0$surv, n_0-1)) / 2
time_0 <- tail(Rhat_0$time, n_0-1)
logminlogR_0 <- log(-log(Rhathat_0))

Rhat_1 <- survfit(Surv(y) ~ x2, subset={x2==1})
n_1 = length(Rhat_1$surv)
Rhathat_1 <- (tail(Rhat_1$surv, n_1-1) + head(Rhat_1$surv, n_1-1)) / 2
time_1 <- tail(Rhat_1$time, n_1-1)
logminlogR_1 <- log(-log(Rhathat_1))

plot(time_0, logminlogR_0, type="l",
     xlab="time",
     ylab="logminlogR")
lines(time_1, logminlogR_1, col=2)
legend("bottomright", legend=c("x2 = 0", "x2 = 1"), col=c(1, 2), pch=1)

plot(log(time_0), logminlogR_0, 
     xlab="time",
     ylab="logminlogR")
points(log(time_1), logminlogR_1, col=2)
legend("bottomright", legend=c("x2 = 0", "x2 = 1"), col=c(1, 2), pch=1)

## ---- 3
new_data <- data
new_data$x1 = log(new_data$x1)
coxreg_ex <- coxph(Surv(y, delta)~x1 + x2, data=new_data)
summary(coxreg_ex)

## ---- 4
sres_ex <- residuals(coxreg_ex,type="schoenfeld")
plot(failure_times,sres_ex[,1])
smoothSEcurve(sres_ex[,1], failure_times)
plot(failure_times,sres_ex[,2])
smoothSEcurve(sres_ex[,2],failure_times)

cox.zph(coxreg_ex)

## ---- 5
par(mfrow=c(2,2))
R0 = survfit(coxreg_ex)
plot(R0, 
     xlab='y',
     ylab=expression('R'[0]))

plot(survfit(coxreg_ex, newdata=data.frame(x1=c(0.2), x2)),
     xlab='y',
     ylab='R')
plot(survfit(coxreg_ex, newdata=data.frame(x1=c(1), x2)),
    xlab='y',
    ylab='R')
# plot(c(0,R0$time), c(1, R0$surv^exp(coxreg_ex$coefficients[['x1']] + coxreg_ex$coefficients[['x2']])), type='s')
plot(survfit(coxreg_ex, newdata=data.frame(x1=c(5), x2)),
     xlab='y',
     ylab='R')
par(mfrow=c(1,1))

## ---- 6

## ---- 7

## ---- 8

## ---- 9


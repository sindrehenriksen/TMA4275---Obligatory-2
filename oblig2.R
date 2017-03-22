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
smoothSEcurve(log(x1), mres)
plot(x2, mres)
smoothSEcurve(x2, mres)

## ---- 2
coxreg2 <- coxph(Surv(y,delta)~x2)
sres <- residuals(coxreg2,type="schoenfeld")
failure_times <- sort(y[delta==1])
failure_times <- failure_times[failure_times != 0]
plot(failure_times,sres)
smoothSEcurve(sres,failure_times)

Rhat_0 <- survfit(Surv(y) ~ x2, subset={x2==0})
n_0 = length(Rhat_0$surv)
Rhathat_0 <- (tail(Rhat$surv, n_0-1)
time_0 <- Rhat_0$time
# surv_0 <- Rhat_0$surv
logminlog_0 <- log(-log(Rhat_0$surv))

Rhat_1 <- survfit(Surv(y) ~ x2, subset={x2==1})
time_1 <- Rhat_1$time
# surv_1 <- Rhat_1$surv
logminlog_1 <- log(-log(Rhat_1$surv))

plot(time_0, logminlog_0)
points(time_1, logminlog_1, col=2)
plot(log(time_0), logminlog_0)
points(log(time_1), logminlog_1, col=2)

## ---- 3


## ---- 4

## ---- 5

## ---- 6

## ---- 7

## ---- 8

## ---- 9


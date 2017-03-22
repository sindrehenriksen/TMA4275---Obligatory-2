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

# Rhat <- survfit(coxreg)
# Zhat <- -log(R$surv)
# M <- delta - Zhat

## ---- 2

## ---- 3

## ---- 4

## ---- 5

## ---- 6

## ---- 7

## ---- 8

## ---- 9


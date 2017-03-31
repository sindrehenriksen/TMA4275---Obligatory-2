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
     xlab="log(time)",
     ylab="logminlogR")
points(log(time_1), logminlogR_1, col=2)
legend("bottomright", legend=c("x2 = 0", "x2 = 1"), col=c(1, 2), pch=1)

## ---- 3
coxreg_ex <- coxph(Surv(y, delta)~log(x1) + x2)
summary(coxreg_ex)

## ---- 4
sres_ex <- residuals(coxreg_ex,type="schoenfeld")
plot(failure_times,sres_ex[,1])
smoothSEcurve(sres_ex[,1], failure_times)
plot(failure_times,sres_ex[,2])
smoothSEcurve(sres_ex[,2],failure_times)

cox.zph(coxreg_ex)

## ---- 5
R0 = survfit(coxreg_ex)

par(mfrow=c(2,2))
plot(R0, 
     xlab='y',
     ylab=expression('R'[0]))

plot(survfit(coxreg_ex, newdata=data.frame(x1=0.2, x2)),
     xlab='y',
     ylab='R')
plot(survfit(coxreg_ex, newdata=data.frame(x1=1, x2)),
    xlab='y',
    ylab='R')
plot(survfit(coxreg_ex, newdata=data.frame(x1=5, x2)),
     xlab='y',
     ylab='R')
par(mfrow=c(1,1))

## ---- 6
plot(log(-log(R0$surv)),log(R0$time),xlab='t*',ylab=expression('R*'[0]))
smoothSEcurve(log(R0$time),log(-log(R0$surv)))

## ---- 7
weib_reg <- survreg(Surv(y, delta)~log(x1) + x2, dist='weibull')
summary(coxreg_ex)
summary(weib_reg)
n <- length(weib_reg$y[,1])
beta_w1 <- weib_reg$coefficients[2]
beta_w2 <- weib_reg$coefficients[3]
scale <- weib_reg$scale
grad_g1 <- matrix(data = c(-1/scale, beta_w1/scale), nrow = 1)
cov_b1_lnscale <- matrix(data = c(0,0,0,0), nrow=2)
cov_b1_lnscale[2,1] <- vcov(weib_reg)[2,4]
cov_b1_lnscale[1,2] <- vcov(weib_reg)[2,4]
cov_b1_lnscale[1,1] <- vcov(weib_reg)[2,2]
cov_b1_lnscale[2,2] <- vcov(weib_reg)[4,4]
var_beta_c1 <- grad_g1%*%cov_b1_lnscale%*%t(grad_g1)
se_beta_c1 <- sqrt(var_beta_c1)

grad_g2 <- matrix(data = c(-1/scale, beta_w2/scale), nrow = 1)
cov_b2_lnscale <- matrix(data = c(0,0,0,0), nrow=2)
cov_b2_lnscale[2,1] <- vcov(weib_reg)[3,4]
cov_b2_lnscale[1,2] <- vcov(weib_reg)[3,4]
cov_b2_lnscale[1,1] <- vcov(weib_reg)[3,3]
cov_b2_lnscale[2,2] <- vcov(weib_reg)[4,4]
var_beta_c2 <- grad_g2%*%cov_b2_lnscale%*%t(grad_g2)
se_beta_c2 <- sqrt(var_beta_c2)

se_beta_c1
se_beta_c2

## ---- 8

## ---- 9


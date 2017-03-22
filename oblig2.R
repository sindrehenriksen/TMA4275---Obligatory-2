library(survival)
data <- read.table("data.dat")
attach(data)

## ---- 1
coxreg <- coxph(Surv(y, delta)~1)
mres <- residuals(coxreg, type="martingale")
plot(x1, mres)
plot(x2, mres)
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


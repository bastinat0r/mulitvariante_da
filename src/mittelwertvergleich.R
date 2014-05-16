library(car)
library(psych)
library(descr)
source("src/gmedian.R")

#mittelwertvergleich:
attach(data)

vg  <- tapply(qd2, qd1_4, FUN="gmedian")
plot(vg, type = "b")
vg  <- tapply(qd4_4, qd1_4, FUN="gmedian")
plot(vg, type = "b", ylim = c(0, 5))

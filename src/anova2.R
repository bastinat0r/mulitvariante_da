library(descr)
library(foreign)
library(car)

setwd("~/projects/uni/multivariante_da")
ab2010 <- read.spss('data/ZA4610_A10.SAV')

source("src/gmedian.R")

freq(ab2010$V162)

temp <- levels(ab2010$V162)

ab2010$V162_2 <- recode(as.numeric(ab2010$V162), recodes = 'c(2,3,5,6,7,8) = NA')
ab2010$V162_2 <- ordered(ab2010$V162_2, labels = temp[c(1,4)])

ab2010$V162 <- recode(as.numeric(ab2010$V162), recodes = "c(7,8) = NA")
ab2010$V162 <- ordered(ab2010$V162, labels = temp[1:6])

freq(ab2010$V162)
freq(ab2010$V162_2)


freq(ab2010$V7)



attach(ab2010)

freq(V83)
is.ordered(V162)
gmedian(V7)

tapply(V7, V162, FUN = 'gmedian')

wilcox.test(as.numeric(V7) ~ V162_2) 

kruskal.test(as.numeric(V7) ~ V162)


tapply(as.numeric(V7),V162, FUN = 'mean')

t.test(as.numeric(V7) ~ V162_2)

summary(aov(as.numeric(V7) ~ V162))



tapply(as.numeric(V7),V65, FUN = 'mean')

summary(aov(as.numeric(V7) ~ V162 * V65))


detach(ab2010)

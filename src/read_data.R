setwd("/home/sebastian/projects/uni/multivariante_da")

library(foreign) # for spss-data-format

data <- read.spss("data/ZA5852_v3-0-1.sav", to.data.frame = T)

View(data)

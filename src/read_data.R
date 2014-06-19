setwd("/home/sebastian/projects/uni/multivariante_da")

library(foreign) # for spss-data-format

data_raw <- read.spss("data/ZA5852_v3-0-1.sav", to.data.frame = T)
data_cities  <- subset(data_raw, d25 == "Large town")



# D11 How old are you
# D10 Gender
# D61: On the following scale, step ’1’ corresponds to the lowest level in the society”; step ’10’ corresponds to the highest level in the society”. Could you tell me on which step you would place yourself?

# QD1 How often do you use ...

# QD3 When travelling within cities, how often do you encounter problems that limit your access to activities, goods or services?
# QD4 Do you think that the following issues are an important problem or not within cities ...

View(bc)

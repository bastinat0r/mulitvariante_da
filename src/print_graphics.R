
library(descr)
library(car)
svg( file = "diagrams/qd5.svg", pointsize = 8, width = 8)
freq(data$qd5, y.axis = "percent", col = c("#cccccc"), main = "QD5: In the future, do you think the traffic situation within cities will improve, stay the same or get worse?"
	, ylab = "%")
dev.off()
svg( file = "diagrams/qd3.svg", pointsize = 8, width = 8)
freq(data$qd5, y.axis = "percent", col = c("#cccccc"), main = "QD3: When travelling within cities, how often do you encounter problems that limit your access to
activities, goods or services?"
	, ylab = "%")
#pie, plot, boxplot,
svg( file = "diagrams/pie.svg", pointsize = 8, width = 8)
pie(table(data$qd5), col = c("#cccccc", "#ddddee"), main = "QD3: When travelling within cities, how often do you encounter problems that limit your access to
activities, goods or services?")

dev.off()

svg( file = "diagrams/box.svg", pointsize = 8, width = 8)
boxplot(data$d11 ~ as.numeric(data$qd4_1), col = c("green", "red"), main = "QD3: When travelling within cities, how often do you encounter problems that limit your access to
activities, goods or services?")

dev.off()

source("src/gmedian.R")

A  <- aggregate(list(
			qd4_1 = as.numeric(data$qd4_1),
			qd4_2 = as.numeric(data$qd4_2),
			qd4_3 = as.numeric(data$qd4_3),
			qd4_4 = as.numeric(data$qd4_4),
			qd4_5 = as.numeric(data$qd4_5)
			),
		by = list(age = floor((data$d11) / 30)), 
		FUN = gmedian)


rownames(A)  <- A[,1]
A  <- as.matrix(A[,-1])
svg( file = "diagrams/agreg.svg", family = "sans serif", pointsize = 8, width = 8)
dotchart(A)

dev.off()


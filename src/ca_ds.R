source("src/read_data.R")
library(ca)
library(descr)


print_ca_to_file <- function(var1, var2, filename) {
	ct <- CrossTable(var1, var2,missing.include = T)
	fit <- ca(ct$prop.tbl)
	svg(file=filename)
	plot(fit, mass = c(T,T), contrib = "absolute", arrows = c(F, F), labels = c(2,2))
	dev.off()
}
print_ca_to_file(as.numeric(bc$d10), as.numeric(bc$d11), "diagrams/d10_qd5.svg")

dim(prop.table(table(bc$d10, bc$d10)))

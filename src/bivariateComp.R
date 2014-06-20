# This Function computes the median of a grouped frequency distribution.
bivariateComp <- function(v1, v2) {
# mittelwertvergleich
	crosstab(v1, v2)
	print(assocstats(xtabs(~ v1 + v2)))
	print(wilcox.test(as.numeric(v1) ~ v2))
	print(kruskal.test(as.numeric(v1) ~ v2))
	print(summary(aov(as.numeric(v1) ~ v2)))
	vg  <- tapply(v1, v2, FUN="gmedian")
	plot(vg, type = "b")


}

library(ca)
library(foreign)
library(descr)

#albus datensatz einlesen
ab2010 <- read.spss("data/ZA4610_A10.SAV")

# Kreuztabelle erstellen
# 'as.numeric' um die level nicht einzeln zu benennen
#ct_7_33 <- CrossTable(as.numeric(ab2010$V7), ab2010$V33)
ct_7_33 <- prop.table(table(as.numeric(ab2010$V7), ab2010$V33) ,1)

# prop.tbl für den Normalisierungsschritt (anstelle der absoluten Häufigkeiten werden die relativen Spalten- und Zeilenhäufigkeiten benutzt.
#fit <- ca(ct_7_33$prop.tbl)
fit <- ca(ct_7_33)

# für mögliche Parameter siehe help(plot.ca)
plot(fit)
summary(fit)

plot_ca <- function(var1, var2) {
	ct <- CrossTable(var1, var2)
	fit <- ca(ct$prop.tbl)
	print(summary(fit))
	plot(fit, mass = c(T,T), contrib = "absolute", arrows = c(F, F), labels = c(2,2))

}


plot_ca(as.numeric(ab2010$V7), ab2010$V35)

#ingelhart-index, links-rechts-selbsteinstufung
plot_ca(ab2010$V77, as.numeric(ab2010$V78))

plot_ca(ab2010$V341, as.numeric(ab2010$V7))

svg( file = "diagrams/large_example.svg", pointsize = 8, width = 15, height= 15)
plot_ca(ab2010$V341, as.numeric(ab2010$V7))
dev.off()

library(FactoMineR) # MCA http://www.inside-r.org/packages/cran/FactoMineR/docs/MCA


ab2010$V7_re <- ab2010$V7
levels(ab2010$V7_re) <- c(1:11)
ca_vars <- ab2010[c("V341","V7_re")]
#cats = apply(ca_vars, 2, function(x) nlevels(as.factor(x)))
mca1 = MCA(ca_vars, graph=FALSE)

# table of eigenvalues
#mca1$eig
# column coordinates
#mca1$var$coord
# row coordinates
#mca1$ind$coord

# data frames for ggplot
mca1_vars_df = data.frame(mca1$var$coord, Variable = c("1","2","3","4","5","6","7","8","9","a","b","c","d","e","f","g","h","f","h","i","j"))
#mca1_vars_df = data.frame(mca1$var$coord, Variable=rep(names(cats), cats))
mca1_vars_df = mca1_vars_df[c(1,3:8),]

mca1_obs_df = data.frame(mca1$ind$coord)
library(ggplot2) # ggplot is depricated

 # MCA plot of observations and categories
print(
ggplot(data=mca1_obs_df, aes(x=Dim.1, y=Dim.2)) +
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_point(colour="gray50", alpha=0.7) +
  geom_density2d(colour="gray80") +
  geom_text(data=mca1_vars_df,
            aes(x=Dim.1, y=Dim.2, label=rownames(mca1_vars_df),
colour=Variable)) +
	scale_colour_discrete(name="Variable")
)

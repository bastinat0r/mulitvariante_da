library(ca)
library(descr) # crosstab

# Minimalbeispiel aus dem ca-Paket
data("smoke")
fit <- ca(smoke)
plot(fit)
summary(fit)

# Einlesen des Datensatzes (rp13 WLAN-Accespoints)
rp13 <- read.csv("data/rp13.csv")

# Zeitformat mit hinreichender genauigkeit. %F ~ y-m-d
time_format <- "%FT%H"
time <- strptime(rp13$Zeit, format = time_format)
rp13$Zeit_re <- strftime(time, format = "%a%H")


# Erstellen einer Kreuztabelle. Wichtig für ca: prop.t = T sorgt dafür, dass eine normalisierte Tabelle erstellt wird
ct  <- crosstab(rp13$Zeit_re, rp13$Raum, prop.t = T, plot = F)

# Erstellen der ca
fit <- ca(ct$prop.tbl)

# Ausgabe als Graphik, Parameter siehe help(plot.ca)
plot(fit, mass = c(F,T), contrib = "absolute", arrows = c(F, T), labels = c(2,2))

# Formatierte Textausgabe
summary(fit)

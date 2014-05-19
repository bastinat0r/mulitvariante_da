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
rp13$Zeit_re <- strftime(time, format = "%H")


# Erstellen einer Kreuztabelle. Wichtig für ca: prop.t = T sorgt dafür, dass eine normalisierte Tabelle erstellt wird
ct  <- crosstab(rp13$Zeit_re, rp13$Raum, prop.t = T, plot = F)

# Erstellen der ca
fit <- ca(ct$prop.tbl)

# Ausgabe als Graphik, Parameter siehe help(plot.ca)
plot(fit, mass = c(F,T), contrib = "absolute", arrows = c(F, T), labels = c(2,2))

# Formatierte Textausgabe
summary(fit)

# Gerät & Raum
ct2  <- crosstab(rp13$fortlaufende.Gerätenummer, rp13$Raum, prop.t = T, plot = F)
fit2 <- ca(ct2$prop.tbl)
plot(fit2, mass = c(F,T), contrib = "absolute", arrows = c(F, T), labels = c(0,2))

# Räume ohne Garderobe und vip
#rp13_subset <- subset(rp13, rp13$Raum != "garderobe" & rp13$Raum != "vip")

# Bühnen & Workshop-Räume
#rp13_subset <- subset(rp13, grepl("^(stage|workshop)", rp13$Raum))

# siehe help(regex) und help(grepl)
rp13_subset <- subset(rp13, !grepl("^(stage|workshop|garderobe|vip|lounge)", rp13$Raum))

ct2_subset  <- CrossTable(rp13_subset$fortlaufende.Gerätenummer, rp13_subset$Raum)
fit2_subset <- ca(ct2_subset$prop.tbl)
plot(fit2_subset, mass = c(F,F), contrib = "absolute", arrows = c(F, T), labels = c(0,2))


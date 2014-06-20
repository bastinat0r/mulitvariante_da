install.packages("aod")
install.packages("ggplot2")
install.packages("fmsb")

library(car)
library(descr)
library(vcd)
library(ca)
library(FactoMineR)
library(foreign)

library(fmsb)
library(aod)
library(ggplot2)

ab2010 <- read.spss('data/ZA4610_A10.SAV', use.missings=T, to.data.frame = T)

# Inhalt
# 1. Multiple Regression
# 2. Logistische Regression

# 1. Multiple Regression

#Datenaufbereitung
wohndauer   	<- ab2010$V717
berufsaufstieg 	<- as.numeric(ab2010$V15)
sichererberuf 	<- as.numeric(ab2010$V11)
hoheseinkommen 	<- as.numeric(ab2010$V13)
geschlecht 		<- recode(as.numeric(ab2010$V298), recodes = '1 = 0; 2 = 1; c(99) = NA')
zufriedenheit 	<- as.numeric(ab2010$V749)
region 			<- recode(as.numeric(ab2010$V975), recodes = '1:10 = 0; 11:17 = 1; c(99) = NA')

#mrlaufx = Name kann individuell vergeben werden. hier wird die funktion für die berechnung der regression gespeichert.
#lm() = Befehl zur Generierung der multiplen Regressionsanalyse

#Lauf A: Wohndauer, Berufsaufstiege, sicherer Beruf
mrlaufa <- lm(wohndauer~berufsaufstieg+sichererberuf)
summary(mrlaufa)

#Lauf B: Wohndauer, sicherer Beruf, hohes Einkommen
mrlaufb <- lm(wohndauer~sichererberuf+hoheseinkommen)
summary(mrlaufb)

#Lauf C: Wohndauer, sicherer Beruf, Geschlecht
mrlaufc <- lm(wohndauer~sichererberuf+geschlecht)
summary(mrlaufc)

#Lauf D: Wohndauer, sicherer Beruf, allg. Zufriedenheit
mrlaufd <- lm(wohndauer~sichererberuf+zufriedenheit)
summary(mrlaufd)

#Lauf E: Wohndauer, sicherer Beruf, Region
mrlaufe <- lm(wohndauer~sichererberuf+region)
summary(mrlaufe)

#Lauf F: alle Items
mrlauff <- lm(wohndauer~berufsaufstieg+sichererberuf + hoheseinkommen + geschlecht + zufriedenheit + region)
summary(mrlauff)

#Lauf G: alle Items außer Berufsaufstieg
mrlaufg <- lm(wohndauer~sichererberuf + hoheseinkommen + geschlecht + zufriedenheit + region)
summary(mrlaufg)

#Lauf H: alle Items außer Berufsaufstieg und Geschlecht
mrlaufh <- lm(wohndauer~sichererberuf+ hoheseinkommen + zufriedenheit+region)
summary(mrlaufh)

#Lauf I: alle Items außer Berufsaufstieg und allg. Zufriedenheit
mrlaufi <- lm(wohndauer~sichererberuf + hoheseinkommen + geschlecht+region)
summary(mrlaufi)

#Lauf J: alle Items außer Berufsaufstieg, Geschlecht und all. Zufriedenheit
mrlaufj <- lm(wohndauer~sichererberuf + hoheseinkommen+region)
summary(mrlaufj)

#Lauf J mit Interaktionsterm: alle Items außer Berufsaufstieg, Geschlecht und all. Zufriedenheit ### mit Interaktionsterm
interact <- lm(wohndauer~sichererberuf + hoheseinkommen+region + sichererberuf:hoheseinkommen)
summary(interact)



#ANOVA zwischen Lauf I und J:
anova(mrlaufi, mrlaufj)
anova(mrlaufi, interact)

# 2. Logistische Regression

zufriedenheit <- recode(as.numeric(ab2010$V749), recodes = '1:8 = 0; 9:11 = 1; c(99) = NA')

# folgende Variablen erhalten eine neue Variable (zu besseren Übersicht)

lebensziele <- as.numeric(ab2010$V35)
#wohntyp 	<- as.numeric(ab2010$V714)
wohnort 	<- as.numeric(ab2010$V715)
gesundheit 	<- as.numeric(ab2010$V398)
einkommen 	<- as.numeric(ab2010$V613)

# Dichotomisierung von Berufsstatus durch das Erstellen von Dummy-Variablen
berufstaetigkeitvollzeit <- recode(as.numeric(ab2010$V340), recodes = '1 = 1;  NA = NA; else = 0')         # 0 = ja, 1 = nein
berufstaetigkeitteilzeit <- recode(as.numeric(ab2010$V340), recodes = '2:3 = 1; NA = NA; else = 0')     # 0 = ja, 1 = nein
berufstaetigkeitnein <- recode(as.numeric(ab2010$V340), recodes = '4 = 1; NA = NA; else = 0')                 # 0 = ja, 1 = nein

berufstellungselbststaendig <- recode(as.numeric(ab2010$V341), recodes  = '3 = 1; NA = NA; else = 0')     # 0 = ja, 1 = nein
berufstellungverbeamtet <- recode(as.numeric(ab2010$V341), recodes      = '4 = 1; NA = NA; else = 0')     # 0 = ja, 1 = nein
berufstellungangestellt <- recode(as.numeric(ab2010$V341), recodes      = '5 = 1; NA = NA; else = 0')     # 0 = ja, 1 = nein
berufstellungarbeiter <- recode(as.numeric(ab2010$V341), recodes        = '6 = 1; NA = NA; else = 0')     # 0 = ja, 1 = nein

#das Erstellen der Logistischen Regression:
#lotitzufriedenheit: Name der Funktion zur Erstellung der Logistischen Regression
#glm(): Befehl zur Erstellung der Logistischen Regression
#zufriedenheit): die abhängige Variable, muss binomial sein!
#~: Trennungszeichen zw. der abhängigen und den unabhängigen Variablen
#+: Trennungszeichen zw. den unabhängigen Variablen
#data = ab2010: Adressierung des Datensatzes, aus welchem die Berechnung für die Logistische Regression erfolgt
#family = "binomial": Kennzeichnung des Typs der Regression; binomial für Logistische Regression

# Ausführen der Logistischen Regression
logitzufriedenheit <- glm(zufriedenheit ~ lebensziele + wohnort + gesundheit + einkommen + berufstaetigkeitvollzeit + berufstaetigkeitteilzeit + berufstaetigkeitnein + berufstellungselbststaendig + berufstellungverbeamtet + berufstellungangestellt + berufstellungarbeiter, data = ab2010, family = "binomial")
logitzufriedenheitinteract <- glm(zufriedenheit ~ (lebensziele + wohnort + gesundheit + einkommen + berufstaetigkeitvollzeit + berufstaetigkeitteilzeit + berufstaetigkeitnein + berufstellungselbststaendig + berufstellungverbeamtet + berufstellungangestellt + berufstellungarbeiter)^2, data = ab2010, family = "binomial")

summary(logitzufriedenheit)
summary(logitzufriedenheitinteract)

#optional: schrittweises Herausstreichen einzelner, nicht signifikanter Items, beginnend mit dem nicht signifikantesten => führt zu Veränderungen der Zusammenhänge und Signifikanzen der verbliebenden Items in der Logistischen Regression
#ohne Berufstätigkeitteilzeit und Berufstätigkeitnein
logitzufriedenheita <- glm(zufriedenheit ~ lebensziele + wohnort + gesundheit + einkommen + berufstaetigkeitvollzeit + berufstellungselbststaendig + berufstellungverbeamtet + berufstellungangestellt + berufstellungarbeiter, data = ab2010, family = "binomial")
summary(logitzufriedenheita)

#ohne berufliche Stellung
logitzufriedenheitb <- glm(zufriedenheit ~ lebensziele + wohnort + gesundheit + einkommen + berufstaetigkeitvollzeit, data = ab2010, family = "binomial")
summary(logitzufriedenheitb)

#ohne Wohnort
logitzufriedenheitc<- glm(zufriedenheit ~ lebensziele + gesundheit + einkommen + berufstaetigkeitvollzeit, data = ab2010, family = "binomial")
summary(logitzufriedenheitc)

#ohne Berufstätigkeitvollzeit
logitzufriedenheitd<- glm(zufriedenheit ~ lebensziele + gesundheit + einkommen, data = ab2010, family = "binomial")
summary(logitzufriedenheitd)

#ohne Wohntyp
logitzufriedenheite<- glm(zufriedenheit ~ lebensziele + gesundheit + einkommen, data = ab2010, family = "binomial")
summary(logitzufriedenheite)

#ohne Einkommen
logitzufriedenheitf<- glm(zufriedenheit ~ lebensziele + gesundheit, data = ab2010, family = "binomial")
summary(logitzufriedenheitf)

# mit Interaktionsterm
logitinteract<- glm(zufriedenheit ~ lebensziele + gesundheit + lebensziele:gesundheit, family = "binomial")
summary(logitinteract)

# Pseudo R² (r-squared)
NagelkerkeR2(logitzufriedenheitf)
NagelkerkeR2(logitinteract) 

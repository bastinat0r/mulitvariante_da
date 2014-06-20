#Anweisung in R
#2. Packete/ Libraries laden
library(descr)
library(car)
library(cfa)

#albus datensatz einlesen
ab2010 <- read.csv2(file="data/ZA4610_A10.csv", header=T)
#3. Kodierung der verwendeten Variablen

ab2010$geschlecht <- ab2010$v298
ab2010$bundesland <- recode(ab2010$v304, recodes = '1:11 = 1; 12:17 = 2; c(0, 99) = NA')
ab2010$kont_zu_ausl <- recode(ab2010$v325, recodes = 'c(0, 9, 99) = NA')

#4.Erstellen der Kontingenztafel
ab2010$konfs <- (
  100 * ab2010$geschlecht + 
    10 * ab2010$bundesland + 
    ab2010$kont_zu_ausl
) 

freq(ab2010$konfs)

x <- freq(ab2010$konfs)[,1][1:8]
y <- as.numeric(names(x))
a <- round(y/100) # hiermit erhalte ich die erste Spalte für das data.frame
b <- round((y-a*100)/10) # hiermit die 2te
c <- round((y-a*100-b*10)) # und 
konfMatrix <- data.frame(a,b,c)


#5. KFA/ CFA ausfuehren
cfa(konfMatrix, freq(ab2010$konfs)[1:8], sorton="chisq")

#6. Hierarchische KFA ausfuehren
hcfa(konfMatrix, freq(ab2010$konfs)[1:8])

#7.Funktionale KFA ausfuehren
X <- (model.matrix(~a+b+c, konfMatrix))
td <- c(2,2,2)
fCFA(freq(ab2010$konfs)[1:8], X,  tabdim=td) 
kvCFA(freq(ab2010$konfs)[1:8], X,  tabdim=td) 

#8. Umbennung der Typen
ab2010$typen <- recode(ab2010$konf, recodes='
                       111 = "m,West,mit";
                       112 = "m,West,ohne";
                       121 = "m,Ost,mit";
                       122 = "m,Ost,ohne";
                       211 = "w,West,mit";
                       212 = "w,West,ohne";
                       221 = "w,Ost,mit";
                       222 = "w,Ost,ohne"
                       ')

#Ausgabe von Kreuztabellen mit Variable Schulabschluß
crosstab(ab2010$typen, ab2010$v327)
# mit Variable Familienstand
crosstab(ab2010$typen, ab2010$v405)
# mit Variable politisches Interesse
crosstab(ab2010$typen, ab2010$v72)

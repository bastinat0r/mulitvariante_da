
library(foreign)
library(Rcmdr)
library(fpc)
library(ggplot2)
library(cluster)

print_clusters <- function (labels, k){
  for (i in 1:k) {
    print (paste("cluster", i))
    print (clust_data[labels==i, c("Einkommen",  "Anerkennung",  "Freizeit",	"Kontakte")])
  }
}


# Setzen des Arbeitsverzeichnisses
ab2010 <- read.spss('data/ZA4610_A10.SAV', use.missings=T, to.data.frame = T)
#ab2010 <- read.spss("C:/Users/Mel/Desktop/Uni/Master/2. Semester/Multivariate Datenanalyse mit R/1 - Datensatz/ALLBUS2010.SAV", use.missings=T)




# (0) Variablen auswählen, standardisieren und Clusteranzahl ermitteln
# (1) Clusterverfahren anwenden
#   (1.1) Hierarchisch/ Agglomerativ
#   (1.2) K-Means
# (2) Clusterstabilität überprüfen (Bootstrap)
#

# (0)
# Neue Variable anlegen
Einkommen <- as.numeric(ab2010$V13)
Anerkennung <- as.numeric(ab2010$V17)
Freizeit <- as.numeric(ab2010$V19)
Kontakte <- as.numeric(ab2010$V27)
clust_data <- model.matrix (~-1 + Einkommen + Anerkennung + Freizeit + Kontakte)

# Data Preparation (Remove missings and rescale variables)
clust_data <- na.omit(clust_data)
clust_data <- scale(clust_data)

# Anzahl der Cluster bestimmen.
# Mit Hilfe von pamk()
pamk.best <- pamk(clust_data)
pamk.best$nc
plot(pam(clust_data, pamk.best$nc))


# Oder mit Hilfe des Knick im SSE Plot.
wss <- (nrow(clust_data)-1)*sum(apply(clust_data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(clust_data, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")



# (1) Clusterverfahren anwenden


# (1.1) Hierarchisches Clustern nach Zumel & Mount

# Distanzmatrix (auch mit "manhattan"-method möglich)
d <- dist(clust_data, method="euclidian")

# Clustering
pfit <- hclust(d, method="ward")

# Dendogramm anzeigen (zweite Zeile: Einzeichnen roter Linien für 5-Cluster-Lösung)
plot(pfit, labels = F, hang = -1)
rect.hclust(pfit, k=5)

# Extracting Clusters
groups <- cutree(pfit, k=5)

# Clusterzugehörigkeiten im Ursprungsdatensatz abspeichern
ab2010$clust_hi <- assignCluster(clust_data, ab2010, groups)

# Ausgabe der Cluster als Tabelle
print_clusters (groups, 5)



# (1.2) K-Means mit statmethods.net (5-Cluster-Lösung)

kfit <-kmeans (clust_data, 5)
ab2010$clust_k <- assignCluster(clust_data, ab2010, kfit$cluster)

# Vary parameters for most readable graph
clusplot (clust_data, kfit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot against first 2 discriminant functions (die beiden wichtigsten Variablen?)
plotcluster(clust_data, kfit$cluster)

# Validating cluster solutions (Prüfung), dafür auch "fpc-package" nötig
cluster.stats(d, groups, kfit$cluster)




# (2): Clusterboot für k-Means, S. 194ff. (auch hierfür fpc-package) 
# Führt cboot-Funktion von boot 1 bis boot 100 durch!
kbest.p <- 5
cboot <- clusterboot(clust_data, clustermethod=kmeansCBI, runs=100, iter.max=100, krange=kbest.p)


# Stability Numbers by "Bootmean" and Number of times that the clusters were "dissolved" by "Bootbrd"
cboot$bootbrd 

cboot <- clusterboot(clust_data, clustermethod=kmeansCBI, runs=100, iter.max=10, krange=5)
cboot$bootbrd 

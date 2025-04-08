library(FactoMineR)
library(factoextra)

temperature0 <- read.table(file.choose(),header=TRUE, sep=";", dec=".",
                           row.names=1)
temperature <- temperature0[-(24:35),1:12]
# Si les variables consid´er´ees ne sont pas homog`enes en variance, il est utile
# de r´eduire (standardiser) les variables avant une classification.
#temperature <- scale(temperature)
res.hclust <- hclust(dist(temperature),method="ward")
plot(res.hclust,xlab="nb de classes",ylab="Hauteur")
cl.hclust <- identify(res.hclust)
nb <- length(cl.hclust)
class.hclust <- rep(0,dim(temperature)[1])
for (i in 1:nb) {class.hclust[cl.hclust[[i]]] <- i}


resAUX.class <- kmeans(temperature,centers=10)
resAUX.hclust <- hclust(dist(resAUX.class$centers),method="ward")
plot(resAUX.hclust,xlab="nb de classes",ylab="Hauteur")

# Choisir le nombre de classes : 3,4,5 ?
res.class <- kmeans(temperature,centers=3)
class.kmeans <- res.class$cluster
res.pca <- PCA(temperature0, scale.unit=TRUE, ind.sup=24:35,quanti.sup=13:16,
               quali.sup=17)
plot.PCA(res.pca, choix="ind", habillage=17)
res.pca$eig
barplot(res.pca$eig[,1])
boxplot(res.pca$ind$coord)

res.pca <- PCA(temperature, scale.unit=TRUE, graph=FALSE)
col.hclust <- as.character(factor(class.hclust,levels=1:5,
                                  labels=c("black","red","green","blue","purple")))
plot.PCA(res.pca,col.ind=col.hclust)
# A comparer avec :
#res.hcpc <- HCPC(temperature)
# je ne comprend pas comment je suis censé comparer les deux graphes car quand je tente d'afficher le graphique de res.hcpc, rien ne s'affiche
col.kmeans <- as.character(factor(class.kmeans,levels=1:5,
                                  labels=c("black","red","green","blue","purple")))
plot.PCA(res.pca,col.ind=col.kmeans)


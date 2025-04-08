library(FactoMineR)
library(factoextra)

setwd("C:\\Users\\quent\\Desktop\\github\\analyse-de-donnee\\TP6")


temperature0 <- read.table("temperat.csv",header=TRUE, sep=";", dec=".",
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

#####################

# Choisir le nombre de classes : 3,4,5 ?
res.class <- kmeans(temperature,centers=3)
class.kmeans <- res.class$cluster
fviz_cluster(res.class, data = temperature, geom = "point")  # Affichage du clustering avec des points

#################

res.pca <- PCA(temperature0, scale.unit=TRUE, ind.sup=24:35,quanti.sup=13:16,
               quali.sup=17)
plot.PCA(res.pca, choix="ind", habillage=17)
res.pca$eig
barplot(res.pca$eig[,1])
boxplot(res.pca$ind$coord)

#############

res.pca = PCA(temperature, scale.unit = T, graph = F)
col.hclust = as.character(factor(class.hclust,levels = 1:5,
                                 labels = c("black","red","green","blue","purple")))
plot.PCA(res.pca, habillage = T, col.ind = col.hclust)
# A comparer avec :
#res.hcpc <- HCPC(temperature)
col.kmeans <- as.character(factor(class.kmeans, levels = 1:5, 
                                  labels = c("black", "red", "green", "blue", "purple")))
plot.PCA(res.pca, habillage = T, col.ind = col.kmeans)


###########################

# Sans perte d’information #
res.pca <- PCA(temperature0, scale.unit=TRUE, ncp=Inf, ind.sup=24:35,
               quanti.sup=13:16, quali.sup=17)
# En ´eliminant le bruit #
res.hcpc <- HCPC(res.pca, nb.clust = 3)
plot.HCPC(res.hcpc, choice = "ind")

res.hcpc$call$t$nb.clust
res.hcpc$call$t$within
res.hcpc$call$t$inert.gain
res.hcpc$call$t$quot

res.hcpc <- HCPC(res.pca)

res.hcpc$desc.var

res.hcpc$desc.axe
res.hcpc$spec

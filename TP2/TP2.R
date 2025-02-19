# crime <- read.table(file.choose(),dec=".",row.names=1)
# names(crime) <- c("murder","rape","robbery","assault","burglary",
#                   "larceny","auto")
# summary(crime)
# hist(crime$robbery)
# boxplot(crime$robbery)
#
# var(crime) # la commande cov(crime) permet également d'afficher la matrice
# # de variance-covariance
# cor(crime)
# plot(crime)
#
#
# crime.acp <- princomp(crime, cor=T)
#
# plot(crime.acp)
# boxplot(as.data.frame(crime.acp$scores))
# biplot(crime.acp)
# biplot(crime.acp,choices=2:3)
#
# crime0 <- subset(crime, row.names(crime) != "New_York")
# crime0 <- subset(crime0, row.names(crime0) != "Massachusetts")
#
# crime0.acp <- princomp(crime0, cor=T)
#
# plot(crime0.acp)
# boxplot(as.data.frame(crime0.acp$scores))
# biplot(crime0.acp)
# biplot(crime0.acp, choices=2:3)
#

hotels <- read.csv(file.choose(),row.names=1)
summary(hotels)
cor(hotels[,sapply(hotels, is.numeric)])
pairs(hotels[,sapply(hotels, is.numeric)])

library(FactoMineR)
res.pca <- PCA(hotels, quali.sup = 1, scale.unit = TRUE, ncp = 8,
               graph = FALSE)
barplot(res.pca$eig[,1], ylab = "Inertie expliquee (%)",
        xlab = "Composante")

barplot(res.pca$eig[,2], ylab = "Inertie cumulee expliquee (%)",
        xlab = "Composante")

abline(h = 80, lty = 2, lwd = 2)

plot(res.pca, choix = "ind", habillage = 1)
plot(res.pca, choix = "ind", habillage = 1, axes = c(3,4))

res.pca$var$coord
res.pca$var$cor
res.pca$var$cos2
res.pca$var$contrib
res.pca$ind$coord
res.pca$ind$cos2
res.pca$ind$contrib

plot(res.pca, choix = "var")
plot(res.pca, choix = "var", axes = c(3, 4))

dimdesc(res.pca)


elldata <- cbind.data.frame(hotels[, 1], res.pca$ind$coord)
coordell <- coord.ellipse(elldata, bary = TRUE)
plot.PCA(res.pca, habillage = 1, ellipse = coordell, new.plot = F)
plot.PCA(res.pca, habillage = 1, ellipse = coordell, new.plot = F)

res.pca2 <- PCA(hotels, quali.sup = 1, quanti.sup = 8, scale.unit = TRUE,
                ncp = 8, graph = FALSE)

dimdesc(res.pca2)
if (is.list(coordell)) {
  plot.PCA(res.pca, habillage = 1, ellipse = coordell, new.plot = FALSE)
} else {
  warning("coordell n'est pas une liste, vérifiez la sortie de coord.ellipse()")
}


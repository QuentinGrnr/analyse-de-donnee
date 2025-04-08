# training tp2
setwd("C:\\Users\\quent\\Desktop\\github\\analyse-de-donnee\\TP2")
crime <- read.table("crime.dat", dec=".", row.names = 1)
names(crime) <- c("murder","rape","robbery","assault","burglary",
                  "larceny","auto") # rename des vars
crime

summary(crime$murder)
hist(crime$murder, col = grey(0.6))

# etc....

var(crime) # == cov(crime)
cor(crime)
plot(crime)

# ------------ ACP -------------

pca_result <- princomp(crime, cor = TRUE)

summary_acp <- summary(pca_result) # obtenir les pourcentages de variance
print(summary_acp)

plot(crime.acp) # plot des variances expliquées
boxplot(as.data.frame(crime.acp$scores)) # moustaches des composantes => montre si il y a des composantes avec des valeurs hinabituelle
biplot(crime.acp) # biplot comp 1 et 2= cercle de corrélation + espaces des individus
biplot(crime.acp,choices=2:3) # biplot comp 2 et 3

#-----------------------------------------------

hotels <- read.csv("hotels.csv",row.names=1)

summary(hotels)

# Exclusion de la colonne 'PAYS' et calcul de la matrice de covariance pour les colonnes restantes
hotels_numeric <- hotels[, -1]  # Supprime la première colonne, qui est 'PAYS'
cov(hotels_numeric)
cor(hotels_numeric)

library(FactoMineR)
res.pca <- PCA(hotels, quali.sup = 1 # premiere colone = val qualitative pas use pour le calcul
               , scale.unit = TRUE, # standardise variables
               ncp = 8, # nbr de composantes a garder
               graph = FALSE) # pas de graph pour le moment
summary(res.pca)
# l'inertie expliquée en pourcentage pour chaque composante principale
barplot(res.pca$eig[, "percentage of variance"], ylab = "Inertie expliquée (%)", xlab = "Composante")
# l'inertie cumulée expliquée en pourcentage
barplot(res.pca$eig[, "cumulative percentage of variance"], ylab = "Inertie cumulée expliquée (%)", xlab = "Composante", col = "blue")
# Ajout d'une ligne horizontale à 80% sur le graphique de l'inertie cumulée expliquée
abline(h = 80, lty = 2, lwd = 2, col = "red")

plot(res.pca, choix = "ind", # on prend les individus
     habillage = 1) # on prend la 1ére var quantitative pour les couleurs (pays)
plot(res.pca, choix = "ind", habillage = 1, axes = c(3,4)) # on prend juste les axes 3 et 4

res.pca$var$coord #  : Coordonnées des variables sur les axes des composantes principales.
res.pca$var$cor # Corrélations des variables avec les axes des composantes principales.
res.pca$var$cos2 # Carré du cosinus (cos²) des angles entre les variables et les axes des composantes.
res.pca$var$contrib # Contribution des variables à chaque axe des composantes principales.
res.pca$ind$coord # Coordonnées des individus (observations) sur les axes des composantes principales.
res.pca$ind$cos2 # Carré du cosinus des angles entre les individus et les axes des composantes.
res.pca$ind$contrib # Contribution des individus à chaque axe des composantes principales.

# cercles des correlations
plot(res.pca, choix = "var")
plot(res.pca, choix = "var", axes = c(3, 4))

dimdesc(res.pca) # description automatique des axes de l'acp

elldata = cbind.data.frame(hotels[, 1], res.pca$ind$coord)
coordell = coord.ellipse(elldata, bary = TRUE) # permet de définir la zone ou les pays (var qualitative) sont situé sur les axes en moyenne
plot.PCA(res.pca, habillage = 1, ellipse = coordell, new.plot = F)

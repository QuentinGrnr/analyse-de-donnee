install.packages("factoextra")
library(factoextra)

setwd("TP3")

dpt <- read.table("depart.dat", header = FALSE, sep = "", strip.white = TRUE)

colnames(dpt) <- c("NumDep", "CodeDep", "CodeReg",
                   "TXCR", "ETRA", "URBR",
                   "JEUN", "AGE", "CHOM",
                   "AGRI", "ARTI", "CADR",
                   "EMPL", "OUVR", "PROF",
                   "FISC", "CRIM", "FE90")

dpt0 <- dpt[, c("TXCR", "ETRA", "URBR",
                "JEUN", "AGE", "CHOM",
                "AGRI", "ARTI", "CADR",
                "EMPL", "OUVR", "PROF",
                "FISC", "CRIM", "FE90")]

# Réaliser l'ACP de la table dpt0 avec normalisation (ACP réduite)
dpt.acp <- prcomp(dpt0, scale = TRUE)

# Choisir la dimension à partir des valeurs propres (scree plot)
plot(dpt.acp, type = "l", main = "Scree Plot (Valeurs Propres)")

# Tracer les représentations individus vs. variables (biplot)
biplot(dpt.acp, main = "Biplot - Individus vs. Variables")

# Afficher les valeurs propres
dpt.eigen <- dpt.acp$sdev^2
print(dpt.eigen)

# Afficher la proportion de variance expliquée
dpt.variance <- dpt.eigen / sum(dpt.eigen)
print(dpt.variance)

# Afficher le cumul de variance expliquée
dpt.cumulative <- cumsum(dpt.variance)
print(dpt.cumulative)

# Justification du nombre d'axes : on garde les axes jusqu’à 85-90% de variance expliquée
abline(h = 0.9, col = "red", lty = 2)

# Biplot avec choix des axes principaux (par exemple, axes 2 et 3)
biplot(dpt.acp, choices = c(2, 3), main = "Biplot - Axes 2 et 3")

# Bonus : Boxplot des scores pour chaque composante principale
boxplot(as.data.frame(dpt.acp$x), main = "Boxplot des Scores par Composante")

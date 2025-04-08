library(FactoMineR)
csp <- as.factor(c("csp3","csp1","csp2","csp2","csp3","csp4"))
sport <- as.factor(c("S","R","R","S","T","T"))
fic.tab <- table(sport,csp)
fic.conting <- data.frame(unclass(fic.tab))
# Paramètres graphiques #
x11()
par(mfrow=c(1,3))

fic.afc <- CA(fic.conting,graph=FALSE)
fic.afc
plot(fic.afc,title="AFC \n classique")

# Fonction construisant le tableau disjonctif complet #
disj.comp <- function(tab) {
  n <- dim(tab)[1]
  m <- dim(tab)[2]
  ci <- sapply(tab,max)
  c <- sum(ci)
  disj.tab <- matrix(0,nrow=n,ncol=c)
  id <- 0
  for (j in 1:m) {
    for (i in 1:n) {
      disj.tab[i,id + tab[i,j]] <- 1
    }
    id <- id + ci[j]
  }
  disj.tab
}
# AFC du tableau disjonctif #
fic.disj.comp <- disj.comp(as.data.frame(cbind(csp,sport)))
colnames(fic.disj.comp) <- c(levels(csp),levels(sport))
fic.disj.afc <- CA(fic.disj.comp,graph=FALSE)
fic.disj.afc
plot(fic.disj.afc,title="AFC \n Tableau Disjonctif")

# afc du tableau de Burt
fic.burt <- t(fic.disj.comp) %*% fic.disj.comp
fic.burt.afc <- CA(fic.burt,graph=FALSE)
fic.burt.afc
plot(fic.burt.afc,title="AFC \n Tableau de Burt")

# -------------------
data(JO)
JO.ca <- CA(JO)
JO
# choix nbr axes 
round(JO.ca$eig,2)
barplot(JO.ca$eig[,1],main="Valeurs propres", names.arg=paste("dim",1:nrow(JO.ca$eig)))
# représentation simultanée
plot(JO.ca)
plot(JO.ca,axes=3:4)

# Contributions pays à l'axe 1 #
round(JO.ca$col$contrib[rev(order(JO.ca$col$contrib[,1])),1],2)
# Contributions pays à l'axe 2 #
round(JO.ca$col$contrib[rev(order(JO.ca$col$contrib[,2])),2],2)


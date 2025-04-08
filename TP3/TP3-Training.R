library(FactoMineR)
setwd("C:\\Users\\quent\\Desktop\\github\\analyse-de-donnee\\TP3")

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

dpt0

dpt.acp <- PCA(dpt0, scale.unit = TRUE, # standardise variables
                    ncp = 8, # nbr de composantes a garder
                    graph = FALSE # pas de graph pour le moment
)

summary(dpt.acp)

barplot(dpt.acp$eig[, "percentage of variance"], ylab = "Inertie expliquée (%)", xlab = "Composante")

plot(dpt.acp, choix="var")
plot(dpt.acp, choix="var",  axes = c(2, 3))

plot(dpt.acp, choix ="ind")
plot(dpt.acp, choix ="ind", axes = c(2, 3))

######

n<-rep(0,95); n[dpt[,3]=="NPC"]<-1
n[dpt[,3]=="Pic"]<-1; n[dpt[,3]=="HNo"]<-1
n[dpt[,3]=="ChA"]<-1; n[dpt[,3]=="Als"]<-2
n[dpt[,3]=="Lor"]<-2; n[dpt[,3]=="FrC"]<-2
n[dpt[,3]=="BNo"]<-3; n[dpt[,3]=="Bre"]<-3
n[dpt[,3]=="PaL"]<-3; n[dpt[,3]=="Cen"]<-4
n[dpt[,3]=="Bou"]<-4; n[dpt[,3]=="PoC"]<-5
n[dpt[,3]=="Lim"]<-5; n[dpt[,3]=="Auv"]<-6
n[dpt[,3]=="RhA"]<-6; n[dpt[,3]=="Aqu"]<-7
n[dpt[,3]=="MiP"]<-7; n[dpt[,3]=="LaR"]<-8
n[dpt[,3]=="PAC"]<-8; n[dpt[,3]=="Cor"]<-8
indice=n>0
m<-subset(n,n>0)
xc<-1:95
ind<-xc[indice]
departements<-dpt0[ind,]
lbls<-c("Nord","Est","Ouest","CentreNord","CentreOuest","CentreEst",
        "SudEst","SudOuest")
partition=factor(m,labels=lbls)

library(ade4)
departements.afd <- discrimin(dudi.pca(departements,scan=FALSE),
                              partition,scan=FALSE)
departements.afd
plot(departements.afd)


########


data(iris)
attach(iris)
# Discrimination en 2 dimensions #
plot(iris[,1],iris[,2],col=c("blue","red","green")[Species])
# Discrimination en 3 dimension #
library(rgl)

# plot3d(iris[,1],iris[,2],iris[,3],col=c("blue","red","green")[Species],type="s")

library(ade4)
iris.afd <- discrimin(dudi.pca(iris[,1:4],scan=FALSE),Species,scan=FALSE)
iris.afd
plot(iris.afd)

library(MASS)
train <- sample(1:150, 75)
table(Species[train])
iris.afd <- lda(Species ~ ., iris, prior = c(1,1,1)/3, subset = train)
iris.afd
pred <- predict(iris.afd, iris[-train, ])$class
table(Species[-train],pred)
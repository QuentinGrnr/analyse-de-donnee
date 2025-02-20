data(iris)
attach(iris)

plot3d(iris[,1],iris[,2],iris[,3],col=c("blue","red","green")[Species],type="s")


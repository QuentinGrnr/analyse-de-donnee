# TP 1 training
#charger data
data("iris")

dim(iris) # => nbr de variable

names(iris) # => names vars

iris$Species # => donne toutes les valeurs de la collone species

levels(iris$Species) # => donne chaque modalitée que prend cette variable

summary(iris$Species) # => donne nombre d'occurence de chaque modalitée de la variable species
table(iris$Species) # same qu'au dessus

pie(summary(iris$Species)) # cammebert du tableau en parm
barplot(summary(iris$Species)) # barplot => var qualitative

summary(iris$Petal.Length) # donne infos génére de la table

sort(iris$Petal.Length) # range les valeurs dans l'ordre croissant

hist(iris$Petal.Length, col = grey (0.6), main = "Histogramme") # histogramme

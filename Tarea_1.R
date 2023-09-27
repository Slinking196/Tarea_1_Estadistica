#install.packages("ggplot2", dependencies = TRUE)
#install.packages("GGally")
#install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(GGally)

# Ejercicio 1.-
data_boston_housing = read.csv("BostonHousing.csv")
head(data_boston_housing, n = 5)

# Ejercicio 2.-
mean(data_boston_housing$crim)

# Ejercicio 3.-
pairs(data_boston_housing, pch= 20, col= 'purple')

ggpairs(data_boston_housing)

# Ejercicio 4.-

correlacion=cor(data_boston_housing, method= "pearson")
print(correlacion)

#Ejercicio 5.-

options(repr.plot.width = 8, repr.plot.height = 8)
corrplot(correlacion, method = "number")

#Ejercicio 6.-

corrplot(correlacion, method = "color")

#Ejercicio 7.-

#ggpairs(data_boston_housing)

#Ejercicio 8.-

numeroDeColumnas=ncol(data_boston_housing)
par(mfrow = c(ceiling(sqrt(numeroDeColumnas)), ceiling(sqrt(numeroDeColumnas))))

for (i in 1:numeroDeColumnas) {
  hist(data_boston_housing[, i], main = names(data_boston_housing)[i], xlab = "Valores", ylab = "Frecuencia")
}

par(mfrow = c(1, 1))

#ejercicio 14.-
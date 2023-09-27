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
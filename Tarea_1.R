# Ejercicio 1.-
data_boston_housing = read.csv("BostonHousing.csv")
head(data_boston_housing, n = 5)

# Ejercicio 2.-
mean(data_boston_housing$crim)

# Ejercicio 3.-
pairs(data_boston_housing)

# Ejercicio 4.-
cor(data_boston_housing$crim, data_boston_housing$age)
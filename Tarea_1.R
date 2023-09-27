#install.packages("ggplot2", dependencies = TRUE)
#install.packages("GGally")
#install.packages("corrplot")
#install.packages("gridExtra")
#install.packages("cowplot")
#install.packages("reshape2")
#install.packages("grid")
#install.packages("e1071")
library(e1071)
library(grid)
library(reshape2)
library(gridExtra)
library(cowplot)
library(corrplot)
library(ggplot2)
library(GGally)

# Ejercicio 1.-
data_boston_housing = read.csv("BostonHousing.csv")
head(data_boston_housing, n = 5)

# Ejercicio 2.-
features_data = names(data_boston_housing)

for(feature in features_data) {
    mean_feature = mean(data_boston_housing[[feature]])
    median_feature = median(data_boston_housing[[feature]])
    desv_feature = sd(data_boston_housing[[feature]])

    cat(paste(feature, ':\n', sep = ''))
    cat(paste('Promedio:', round(mean_feature, 3), '\n'))
    cat(paste('Mediana:', round(median_feature, 3), '\n'))
    cat(paste('Desviacion estandar:', round(desv_feature, 3), '\n\n'))
}

# Ejercicio 3.-

pairs(data_boston_housing, pch= 20, col= 'purple')

# Ejercicio 4.-

correlacion=cor(data_boston_housing, method= "pearson")
print(correlacion)

#Ejercicio 5.-

options(repr.plot.width = 8, repr.plot.height = 8)
corrplot(correlacion, method = "number", type = 'lower')
corrplot(correlacion, method = "ellipse", type = 'lower')

#Ejercicio 6.-

corrplot(correlacion, method = "color", type = 'lower')

#Ejercicio 7.-

new_data_corr = data_boston_housing[, c("indus", "nox", "age", "dis", "rad", "tax")]
pairs(new_data_corr, lower.panel = function(x, y) {
    points(x, y)
    abline(lm(y ~ x), col = "red")
}, upper.panel = NULL)

#Ejercicio 8.-

numeroDeColumnas = ncol(data_boston_housing)
par(mfrow = c(ceiling(sqrt(numeroDeColumnas)), ceiling(sqrt(numeroDeColumnas))))

for (i in 1:numeroDeColumnas) {
  hist(data_boston_housing[, i], main = names(data_boston_housing)[i], xlab = "Valores", ylab = "Frecuencia")
}

par(mfrow = c(1, 1))

# Ejercicio 9.-
features_data = names(data_boston_housing)
boxplot_list = list()
size_col = ncol(data_boston_housing)

for(i in 1: size_col) {
    feature = features_data[i]
    boxplot_data = ggplot(data_boston_housing, aes(y= .data[[feature]])) +
                   geom_boxplot() +
                   theme_minimal()
    boxplot_list[[i]] = ggplotGrob(boxplot_data)
}

grid_boxplot = grid.arrange(grobs = boxplot_list)
ggdraw() + draw_plot(grid_boxplot) + draw_plot(textGrob("Boxplot Boston Housing",
                                                        gp= gpar(fontsize= 20)),
                                               x= 0.27, y= -0.35)

# Ejercicio 10.-
positive_corr = ggplot(data_boston_housing, aes(x= .data[['medv']], y= .data[['rm']])) +
                  geom_point() +
                  geom_smooth(method=lm, se=FALSE) +
                  theme_minimal() +
                  ggtitle('MEDV V/S RM')
negative_corr = ggplot(data_boston_housing, aes(x= .data[['medv']], y= .data[['lstat']])) +
                  geom_point() +
                  geom_smooth(method=lm, se=FALSE) +
                  theme_minimal() +
                  ggtitle('MEDV V/S LSTAT')

grid.arrange(positive_corr, negative_corr, ncol = 2)

# Ejercicio 11.-
for(feature in features_data) {
    asimetria = round(skewness(data_boston_housing[[feature]]), 3)
    curtosis = round(kurtosis(data_boston_housing[[feature]]), 3)

    cat(paste(feature, '\n'))
    cat(paste('Asimetria: ', asimetria, ', Curtosis: ', curtosis, '\n\n', sep = ''))
}

# Ejercicio 12.-
features_data = names(data_boston_housing)
densityplot_list = list()
size_col = ncol(data_boston_housing)

for(i in 1: size_col) {
    feature = features_data[i]
    densityplot_data = ggplot(data_boston_housing, aes(x= .data[[feature]])) +
      geom_density(color = 4,    # Color
                   lwd = 1,      # Ancho
                   linetype = 1,
                   alpha = 0.25,
                   fill = 4) +
      theme_minimal()
    densityplot_list[[i]] = ggplotGrob(densityplot_data)
}

grid_densityplot = grid.arrange(grobs = densityplot_list)
ggdraw() + draw_plot(grid_densityplot) + draw_plot(textGrob("Densityplot Boston Housing",
                                                        gp= gpar(fontsize= 20)),
                                               x= 0.27, y= -0.35)

#Ejercicio 13.-
grupo_1_chas= data_boston_housing[data_boston_housing$chas == 0, ]
grupo_1_chas
grupo_2_chas= data_boston_housing[data_boston_housing$chas == 1, ]

#Ejercicio 14
datos_grupo1 = names(grupo_1_chas)
datos_grupo2 = names(grupo_2_chas)

resultados_grupo1 = c()
resultados_grupo2 = c()

for(feature in datos_grupo1) {
    mean_feature = mean(grupo_1_chas[[feature]])
    var_feature = var(grupo_1_chas[[feature]])

    resultados_grupo1 = c(resultados_grupo1, paste(feature, ':\n', sep = ''))
    resultados_grupo1 = c(resultados_grupo1, paste('Media:', round(mean_feature, 3), '\n'))
    resultados_grupo1 = c(resultados_grupo1, paste('varianza:', round(var_feature, 3), '\n\n'))
}

for(feature in datos_grupo2) {
    mean_feature = mean(grupo_2_chas[[feature]])
    var_feature = var(grupo_2_chas[[feature]])

    resultados_grupo2 = c(resultados_grupo2, paste(feature, ':\n', sep = ''))
    resultados_grupo2 = c(resultados_grupo2, paste('Media:', round(mean_feature, 3), '\n'))
    resultados_grupo2 = c(resultados_grupo2, paste('varianza:', round(var_feature, 3), '\n\n'))
}

cat("Resultados Grupo 1:\n", resultados_grupo1, sep = "")
cat("Resultados Grupo 2:\n", resultados_grupo2, sep = "")

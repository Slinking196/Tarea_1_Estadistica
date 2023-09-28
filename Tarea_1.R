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
data_boston_housing <- read.csv("BostonHousing.csv")
head(data_boston_housing, n = 5)

# Ejercicio 2.-
features_data <- names(data_boston_housing)

for(feature in features_data) {
    mean_feature <- mean(data_boston_housing[[feature]])
    median_feature <- median(data_boston_housing[[feature]])
    desv_feature <- sd(data_boston_housing[[feature]])

    cat(paste(feature, ':\n', sep = ''))
    cat(paste('Promedio:', round(mean_feature, 3), '\n'))
    cat(paste('Mediana:', round(median_feature, 3), '\n'))
    cat(paste('Desviacion estandar:', round(desv_feature, 3), '\n\n'))
}

# Ejercicio 3.-

pairs(data_boston_housing, pch= 20, col= 'purple')

# Ejercicio 4.-

correlacion <- cor(data_boston_housing, method= "pearson")
print(correlacion)

#Ejercicio 5.-

options(repr.plot.width = 8, repr.plot.height = 8)
corrplot(correlacion, method = "number", type = 'lower')
corrplot(correlacion, method = "ellipse", type = 'lower')

#Ejercicio 6.-

corrplot(correlacion, method = "color", type = 'lower')

#Ejercicio 7.-

new_data_corr <- data_boston_housing[, c("indus", "nox", "age", "dis", "rad", "tax")]
pairs(new_data_corr, lower.panel = function(x, y) {
    points(x, y)
    abline(lm(y ~ x), col = "red")
}, upper.panel = NULL)

#Ejercicio 8.-

numeroDeColumnas <- ncol(data_boston_housing)
par(mfrow = c(ceiling(sqrt(numeroDeColumnas)), ceiling(sqrt(numeroDeColumnas))))

for (i in 1:numeroDeColumnas) {
  hist(data_boston_housing[, i], main = names(data_boston_housing)[i], xlab = "Valores", ylab = "Frecuencia")
}

par(mfrow = c(1, 1))

# Ejercicio 9.-
# Como podemos apreciar en los siguientes graficos generados, los datos en las caracteristicas
# crim, b, chas, zn, lstat tiene una gran cantidad de outlayers pero a pesar de todos se puede notar
# La presencia de grupos bastantes definidos, también en los gráficos rad y tax, no se ve tanta lejanía entre
# los datos pero la mediana se encuentra sesgada a un solo sitio.
features_data <- names(data_boston_housing)
boxplot_list <- list()
size_col <- ncol(data_boston_housing)

for(i in 1: size_col) {
    feature <- features_data[i]
    boxplot_data <- ggplot(data_boston_housing, aes(y= .data[[feature]])) +
                   geom_boxplot() +
                   theme_minimal()
    boxplot_list[[i]] <- ggplotGrob(boxplot_data)
}

grid_boxplot <- grid.arrange(grobs = boxplot_list)
ggdraw() + draw_plot(grid_boxplot) + draw_plot(textGrob("Boxplot Boston Housing",
                                                        gp= gpar(fontsize= 20)),
                                               x= 0.27, y= -0.35)

# Ejercicio 10.-
# En este ejercicio decidimos escoger las caracteristicas RM y LSTAT como factores con mayor
# correlación en base al ejercicio 5.
# Como se puede apreciar en los siguiente gráficos MEDV con RM tienen una fuerte correlación
# postiva y MEDV con LSTAT tienen una correlación negativa.

positive_corr <- ggplot(data_boston_housing, aes(x= .data[['medv']], y= .data[['rm']])) +
                  geom_point() +
                  geom_smooth(method=lm, se=FALSE) +
                  theme_minimal() +
                  ggtitle('MEDV V/S RM')
negative_corr <- ggplot(data_boston_housing, aes(x= .data[['medv']], y= .data[['lstat']])) +
                  geom_point() +
                  geom_smooth(method=lm, se=FALSE) +
                  theme_minimal() +
                  ggtitle('MEDV V/S LSTAT')

grid.arrange(positive_corr, negative_corr, ncol = 2)

# Ejercicio 11.-
# Como se puede apreciar en los valores entregados por el siguiente código,
# si las formulas de simetría nos entregan un valor que en el caso de ser
# positivo nos indica que los datos tienen una tendencia a la izquierda de
# la media, como por ejemplo lo que ocurre con crim la cual tiene un valor de 5.192,
# en el caso de ser negativa nos indica que los valores tienen una tendencia
# a la derecha de la media, como por ejemplo con "b" la cual tiene un valor de -2.873.
# En el caso de la curtosis cuando tenemos valores positivos estos quieren decir que
# los datos están distrobuidos muy cercanos a la media provocando un mayor pico en su gráfico
# como se puede apreciar en la característica crim el cual tiene como valor 36.596, y cuando
# el valor de la curtosis es negativa quiere decir que los datos están muy esparcidos al rededor
# de la media, como por ejemplo la característica indus la cual tiene un valor de -1.24.
for(feature in features_data) {
    asimetria <- round(skewness(data_boston_housing[[feature]]), 3)
    curtosis <- round(kurtosis(data_boston_housing[[feature]]), 3)

    cat(paste(feature, '\n'))
    cat(paste('Asimetria: ', asimetria, ', Curtosis: ', curtosis, '\n\n', sep = ''))
}

# Ejercicio 12.-
# Como podemos apreciar en los gráficos de densidad y con respecto a los resultados
# obtenidos en la pregunta 11, la variable crim realmente tiene una mayor distribución
# de sus datos en el lado izquierda y la característica "b" tiene mayor distribución a
# la derecha comprobando lo dicho anteriormente, además si nos podemos fijar en relación
# a la curtosis, los valores de la variable crim están muy concentrados provocando un
# mayor pico en su gráfica y en la característica indus los datos están muy distribuidos
# al rededor de su gráfica provocando que su curtosis sea negativa como lo dicho en el
# ejercicio 11.
features_data <- names(data_boston_housing)
densityplot_list <- list()
size_col <- ncol(data_boston_housing)

for(i in 1: size_col) {
    feature <- features_data[i]
    densityplot_data <- ggplot(data_boston_housing, aes(x= .data[[feature]])) +
      geom_density(color = 4,    # Color
                   lwd = 1,      # Ancho
                   linetype = 1,
                   alpha = 0.25,
                   fill = 4) +
      theme_minimal()
    densityplot_list[[i]] <- ggplotGrob(densityplot_data)
}

grid_densityplot <- grid.arrange(grobs = densityplot_list)
ggdraw() + draw_plot(grid_densityplot) + draw_plot(textGrob("Densityplot Boston Housing",
                                                        gp= gpar(fontsize= 20)),
                                               x= 0.27, y= -0.35)

#Ejercicio 13.-
# En este ejercicio decidimos separar chas en los grupo 1 (osea si el tramo limita con el rio)
# y el grupo 2 (caso contrario al grupo 1), de esta forma podremos ver las caracteristicas de
# estos dos casos.
grupo_1_chas <- data_boston_housing[data_boston_housing$chas == 0, ]
grupo_2_chas <-  data_boston_housing[data_boston_housing$chas == 1, ]

# Ejercicio 14
# Crim: La tasa de crimanilidad del grupo 1 en promedio es mayor a la del grupo 2 pero al
#       tener una varianza mucho mayor esto significa que a pesar de que el grupo 1 tenga mayor
#       tasa de criminalidad sus valores son más dispersos.
# Zn: En promedio la proporción de terrenos residuales es mayor en el grupo 1 que en
#     el 2 pero aún así los valores varían más en el grupo uno consiguiendo estabilidad en el 2.
# Medv: En promedio las casas son más caras en el grupo 2 pero aún así sus valores varían mucho
#       más en comparación al grupo 1 que tiene una varianza de 77.
datos_grupo1 <- names(grupo_1_chas)
datos_grupo2 <- names(grupo_2_chas)

resultados_grupo1 <- c()
resultados_grupo2 <- c()

for(feature in datos_grupo1) {
    if(feature != 'chas') {
        mean_feature <- mean(grupo_1_chas[[feature]])
        var_feature <- var(grupo_1_chas[[feature]])

        resultados_grupo1 <- c(resultados_grupo1, paste(feature, ':\n', sep = ''))
        resultados_grupo1 <- c(resultados_grupo1, paste('Media:', round(mean_feature, 3), '\n'))
        resultados_grupo1 <- c(resultados_grupo1, paste('varianza:', round(var_feature, 3), '\n\n'))
    }
}

for(feature in datos_grupo2) {
    if(feature != 'chas') {
        mean_feature <- mean(grupo_2_chas[[feature]])
        var_feature <- var(grupo_2_chas[[feature]])

        resultados_grupo2 <- c(resultados_grupo2, paste(feature, ':\n', sep = ''))
        resultados_grupo2 <- c(resultados_grupo2, paste('Media:', round(mean_feature, 3), '\n'))
        resultados_grupo2 <- c(resultados_grupo2, paste('varianza:', round(var_feature, 3), '\n\n'))
    }
}

cat("Resultados Grupo 1:\n", resultados_grupo1, sep = "")
cat("Resultados Grupo 2:\n", resultados_grupo2, sep = "")

# Ejercicio 15
# Crim: La tasa de criminalidad de los dos grupos juntos es muy identica al grupo 1 y su
#       varianza igual, esto significa que los datos del grupo 2 no son totalmente grandes
#       en comparación al grupo 1.
# Zn: Lo mismo que ocurría en la característica crim ocurre aquí, el analisis de la data
#     del grupo 1 no varía en su totalidad al unir al grupo 2.
features_data <- names(data_boston_housing)

for(feature in features_data) {
    if(feature != 'chas') {
        mean_feature <- mean(data_boston_housing[[feature]])
        var_feature <- var(data_boston_housing[[feature]])

        cat(paste(feature, ':\n', sep = ''))
        cat(paste('Promedio:', round(mean_feature, 3), '\n'))
        cat(paste('Varianza:', round(var_feature, 3), '\n\n'))
    }
}

# Ejercicio 16
# La varianza intragrupo del grupo 1 y 2 de la mayoría de las características es un poco alta
# indicando que los valores de cada grupo están alejados de sus valores medios, aunque como caso
# contrario tenemos la variable nox la cual tiene una varianza intragrupo de 0.013.
features_data <- names(data_boston_housing)

for(feature in features_data) {
    if (feature != 'chas') {
        var_intra <- (nrow(grupo_1_chas) / nrow(data_boston_housing)) * var(grupo_1_chas[[feature]]) +
          (nrow(grupo_2_chas) / nrow(data_boston_housing)) * var(grupo_2_chas[[feature]])

        cat(paste(feature, ':\n'))
        cat(paste('Varianza intragrupo:', round(var_intra, 3), '\n\n'))
    }
}

# Ejercicio 17
# Muchas de las características tienen el calculo entre sus medias valores muy cercanos
# ya que la varianza intergrupo es cercana a 0, aunque hay casos hay aislados como la caracteristica
# tax el cual tiene una varianza intergrupo de 35.901 indicando que la media entre los grupos es muy
# lejana.
features_data <- names(data_boston_housing)

for(feature in features_data) {
    if (feature != 'chas') {
        var_inter <- as.numeric(nrow(grupo_1_chas) / nrow(data_boston_housing)) * ((mean(grupo_1_chas[[feature]]) -
                    mean(data_boston_housing[[feature]]))^2 ) + (nrow(grupo_2_chas) / nrow(data_boston_housing)) *
                    ((mean(grupo_2_chas[[feature]]) - mean(data_boston_housing[[feature]]))^2)

        cat(paste(feature, ':\n'))
        cat(paste('Varianza Intergrupo:', round(var_inter, 3), '\n\n'))
    }
}

# Ejercicio 18

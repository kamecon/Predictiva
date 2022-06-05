pacman::p_load("sandwich", "jtools", "huxtable", "caret", "mfx", "foreign")

# Cargamos los datos
smoking <- read.dta("RegresionLogistica/Exercises/Smoking.dta")

# Pregunta E11.1

## Apartado a
#En esta pregunta nos piden sencillamente la media de la columna 'smoker', la media de dicha columna cuando smkban =1 (afectados por la prohibicion), y cuando smkban =0 (no afectados por la prohibicion)

mean(smoking$smoker)
mean(smoking[smoking$smkban==1, "smoker"])
mean(smoking[smoking$smkban==0, "smoker"])

## Apartado b

modelo_01 <- lm(data = smoking, smoker ~ smkban)
summ(modelo_01, digits = 3, robust = "HC1")

#La diferencia es de -0,078 con un error estándar de 0,009. El estadístico t resultante es -8,66, por lo que el coeficiente es estadísticamente significativo

## Apartado c
#Creamos la variable age^2

smoking$age2 <- smoking$age^2

#Estimamos el modelo de probabilidad lineal

modelo_02 <- lm(data = smoking, smoker ~ .)
summ(modelo_02, digits = 3, robust = "HC1")

#En el modelo 2 la diferencia estimada es de -0,047, menor que el efecto del modelo 1. Evidentemente, 1 sufre un sesgo de variable omitida. Es decir, smkban puede estar correlacionado con los indicadores de educación/raza/género o con la edad.

## Apartado d

#El estadístico t es de -5,27 y el p-valor pro debajo de 0.01, por lo que el coeficiente es estadísticamente significativo al nivel del 1%


## Apartado e

#El estadístico F tiene un p-valor de 0,00, por lo que los coeficientes son globalmente significativos. El estado de educación omitido  es "Máster o superior". Así, los coeficientes muestran el aumento de la probabilidad en relación con alguien con un título de postgrado. Por ejemplo, el coeficiente de Colgrad es 0,045, por lo que la probabilidad de fumar para alguien con titulo universitario es 0,045 (4,5%) mayor que la de alguien con un título de postgrado. Del mismo modo, el coeficiente de HSdrop es 0,323, por lo que la probabilidad de fumar de alguien que abandona la educación secundaria es 0,323 (32,3%) más alta que para alguien con un título de postgrado. Por ultimo, el coeficiente de hsgrad es 0,233, alguien que completa la educación secundaria tiene una probabilidad 0,233 (23,3%) mayor de fumar, que alguien con título de postgrado. Dado que todos los coeficientes son positivos y se reducen a medida que aumenta el nivel educativo, la probabilidad de fumar disminuye a medida que aumenta el nivel de estudios. 

## Apartado f 

#No hay que hacer esta pregunta

# Pregunta E11.2

## apartado a

modelo_03 <- glm(data = smoking, formula =  smoker ~ ., family = binomial(link = probit))
summ(modelo_03, digits = 3, robust = "HC1")

## apartado b

#El estadístico t del coeficiente de smkban es de -5,47, muy similar al valor del modelo de probabilidad lineal. Su p-valor es menor a 0.01, es estadisticamente significactivo al 

## apartado c

#El estado de educación omitido  es "Máster o superior". Así, los coeficientes muestran el aumento de la probabilidad en relación con alguien con un título de postgrado. Por ejemplo, el coeficiente de Colgrad es 0,235, por lo que la probabilidad de fumar para alguien con titulo universitario es 0,235 (23,5%) mayor que la de alguien con un título de postgrado. Del mismo modo, el coeficiente de HSdrop es 1,142, por lo que la probabilidad de fumar de alguien que abandona la educación secundaria es 1,142 (114,2%) más alta que para alguien con un título de postgrado. Por ultimo, el coeficiente de hsgrad es 0,883, alguien que completa la educación secundaria tiene una probabilidad 0,883 (88,3%) mayor de fumar, que alguien con título de postgrado. Dado que todos los coeficientes son positivos y se reducen a medida que aumenta el nivel educativo, la probabilidad de fumar disminuye a medida que aumenta el nivel de estudios. 

## apartado d

#Primero tenemos que crear un nuevo dataframe con los datos del señor A. Recordar que se deben incluir todas las variables con los mismos nombres del dataframe original.

#En este caso (no tiene porque ser así) uso el mismo orden del dataframe original, el cual verifico ejecutando la función colnames(smoking) o head(smoking)

#Introducimos los datos, recuerde que hay que introducir todos menos la primera columna que corresponde a la variable dependiente u objetivo
nuevo01 <- data.frame(0,20,1,0,0,0,0,0,0,20^2)

#Le asignamos nombres a las columnas, aca lo hago usando los colnames del dataframe original, exluyendo la primera columna.
colnames(nuevo01) <- colnames(smoking)[-1]

#Realizamos la prediccion
prediccion_01 <- predict(object = modelo_03, newdata= nuevo01, type = "response")

#Repetimos lo anterior incluyendo la prohibicion a fumar
nuevo02 <- data.frame(1,20,1,0,0,0,0,0,0,20^2)
colnames(nuevo02) <- colnames(smoking)[-1]

prediccion_02 <- predict(object = modelo_03, newdata= nuevo02, type = "response")

#Calculamos la diferencia
cambio1 <- prediccion_01 - prediccion_02
cambio1

#La introducción de la prohibicion a fumar disminuye la probabilidad de fumar en 0,0623 

## apartado e

#Repetimos lo anterior sustituyendo los datos del enunciado

nuevo03 <- data.frame(0,40,0,0,0,1,1,0,1,40^2)
colnames(nuevo03) <- colnames(smoking)[-1]

prediccion_03 <- predict(object = modelo_03, newdata= nuevo03, type = "response")
prediccion_03

nuevo04 <- data.frame(1,40,0,0,0,1,1,0,1,40^2)
colnames(nuevo04) <- colnames(smoking)[-1]

prediccion_04 <- predict(object = modelo_03, newdata= nuevo04, type = "response")
prediccion_04

cambio2 <- prediccion_03 - prediccion_04
cambio2

## apartado f

#Repetimos todo lo anterior pero usando el modelo de probabilidad lineal (modelo_02)

prediccion_01_lineal <- predict(object = modelo_02, newdata= nuevo01, type = "response")
prediccion_01_lineal

prediccion_02_lineal <- predict(object = modelo_02, newdata= nuevo02, type = "response")
prediccion_02_lineal

cambio3 <- prediccion_01_lineal - prediccion_02_lineal
cambio3

prediccion_03_lineal <- predict(object = modelo_02, newdata= nuevo03, type = "response")
prediccion_03_lineal

prediccion_04_lineal <- predict(object = modelo_02, newdata= nuevo04, type = "response")
prediccion_04_lineal

cambio4 <- prediccion_03_lineal - prediccion_04_lineal
cambio4


## apartado g

#En el modelo lineal, el cambio de probabilidad del señor A y la señora B es el mismo e igual al valor del coeficiente smkban. El modelo de probabilidad lineal asume que el impacto marginal de la prohibición de fumar en el lugar de trabajo sobre la probabilidad de que un individuo fume, no depende de las otras características del individuo, no existe interacción entre ellos, al menos que se introduzca de manera especifica.

#En el caso del modelo probit, el impacto marginal de la prohibición de fumar en el lugar de trabajo sobre la probabilidad de fumar, si depende de características individuales

#apartado h

#No hay que hacer esta pregunta

###EJERCICIO ADICIONAL##
#REPETIR LA PREGUNTA E11.2 ESTIMANDO UN MODELO LOGIT

#Las respuestas a este último apartado adicional estará disponible en la carpeta de soluciones
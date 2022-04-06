# Problema 5

## Cargamos las librerias

pacman::p_load("sandwich", "haven", "jtools", "huxtable")

## Cargamos los datos 

PesoRN <- read_dta("RegresionLineal/Exercises/Data/birthweight_smoking.dta")

### Inspeccionamos los datos
head(PesoRN)
str(PesoRN)
summary(PesoRN)

## Hacemos las regresiones
### i.
modelo_peso01 <- lm(data = PesoRN, formula = birthweight ~ smoker)
summ(modelo_peso01, robust = "HC1")
### ii.
modelo_peso02 <- lm(data = PesoRN, formula = birthweight ~ smoker + alcohol + nprevist)
summ(modelo_peso02, robust = "HC1")
### iii.
modelo_peso03 <- lm(data = PesoRN, formula = birthweight ~ smoker + alcohol + nprevist + unmarried)
summ(modelo_peso03, robust = "HC1")

### Comparamos los 3 modelos
export_summs(modelo_peso01, modelo_peso02, modelo_peso03, robust="HC1")

### Intervalos de confianza para smoker
#### i.
summ(modelo_peso01, robust = "HC1", confint = TRUE)
#### ii.
summ(modelo_peso02, robust = "HC1", confint = TRUE)
#### iii.
summ(modelo_peso03, robust = "HC1", confint = TRUE)


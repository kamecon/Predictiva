## Cargamos las librerías 

pacman::p_load("wooldridge", "sandwich", "jtools", "huxtable")

# Ejemplo 4.3

## Cargamos los datos
data(gpa1)

## Inspeccionamos los datos
str(gpa1)
head(gpa1)

## Estimamos el modelo
ejemplo_4.3 <-  lm(formula =  colGPA ~ hsGPA + ACT + skipped, data=gpa1)
summ(ejemplo_4.3, digits = 3)

## Comparamos los t-valores con los valores criticos al 10%, 5% y 1%

## 10%
critico_10 <- qt(p = 0.95, df = 141 - 4)
critico_10

## 5%
critico_5 <- qt(p = 0.975, df = 141 - 4)
critico_5

## 1%
critico_1 <- qt(p = 0.995, df = 141 - 4)
critico_1

#Los t-valores de hsGPA y skipped superan el valor critico al 1%. El t-valor de ACT no supera a ninguno de los 3 valores criticos, no es estadisticamente significativo a niguno de los 3 niveles de confianza

## Extraemos los datos que necesitamos para el contraste (ver el HMTL "Regresion Lineal Simple en R")
### Coeficientes de la regresion
beta_1 <- unname(ejemplo_4.3$coefficients[2])
beta_2 <- unname(ejemplo_4.3$coefficients[3])
beta_3 <- unname(ejemplo_4.3$coefficients[4])

### Desviaciones tipicas
se_beta_1 <-  summary(ejemplo_4.3)$coefficients[2,2]
se_beta_2 <-  summary(ejemplo_4.3)$coefficients[3,2]
se_beta_3 <-  summary(ejemplo_4.3)$coefficients[4,2]

### Estadisticos t
t_beta_1 <- beta_1 / se_beta_1
t_beta_2 <- beta_2 / se_beta_2
t_beta_3 <- beta_3 / se_beta_3

#Redondeamos a 3 puntos decimales
round(t_beta_1, 3)
round(t_beta_2, 3)
round(t_beta_3, 3)

#Obtenemos los mismos Beta's que observamos en el resumen de `summ`

#Reproducimos tambien los p-valores
pvalor_1  <- 2*pt(q = -abs(t_beta_1),df = 141 - 4)
pvalor_2  <- 2*pt(q = -abs(t_beta_2),df = 141 - 4) 
pvalor_3  <- 2*pt(q = -abs(t_beta_3),df = 141 - 4) 

round(pvalor_1, 3)
round(pvalor_2, 3)
round(pvalor_3, 3)

# Ejemplo 4.5

## Cargamos los datos
data("hprice2")

## Inspeccionamos los datos
str(hprice2)
head(hprice2)

#Vemos en el `head`del conjunto de datos, que no esta la variable logaritmo de la distancia (log(dist)), así que debemos incluirla

hprice2$ldist <- log(hprice2$dist)

## Estimamos el modelo
ejemplo_4.5 <-  lm(formula =  lprice ~ lnox + ldist + rooms + stratio, data=hprice2)
summ(ejemplo_4.5, digits = 3)

## Comparamos los t-valores con los valores criticos al 10%, 5% y 1%

## 10%
critico_10 <- qt(p = 0.95, df = 506 - 5)
critico_10

## 5%
critico_5 <- qt(p = 0.975, df = 506 - 5)
critico_5

## 1%
critico_1 <- qt(p = 0.995, df = 506 - 5)
critico_1

#En el libro se plantea que lo que se quiere es contrastar la hipotesis nula Beta_1 = -1, por lo que construimos el estadistico t correspondiente

beta_1 <- unname(ejemplo_4.5$coefficients[2])
se_beta_1 <-  summary(ejemplo_4.5)$coefficients[2,2]
round(beta_1, 3)
round(se_beta_1, 3)

## Estadistico t
t_beta_1 <- (beta_1 - (-1)) / se_beta_1
round(t_beta_1, 3)

#Observamos que el estdistico t es muy pequeño y esta por debajo de los valores criticos. Confrimamos lo anterior calculando el p-valor 
pvalor_1  <- 2*pt(q = -abs(t_beta_1),df = 141 - 4)
round(pvalor_1, 3)

#Esta muy por encima de 0.1, 0.05 y 0.01

# Ejemplo 4.8

## Cargamos los datos
data("rdchem")

## Inspeccionamos los datos
str(rdchem)
head(rdchem)

## Estimamos el modelo
ejemplo_4.8 <- lm(lrd ~ lsales + profmarg, data = rdchem)
summ(ejemplo_4.8, digits = 3)

#Construimos un intervalo de confianza del para los parametros de la regresion
summ(ejemplo_4.8, digits = 3, confint = TRUE)

#Los resultados anteriores coinciden con el libro


# Ejemplo 4.10

## Cargamos los datos
data("meap93")

## Inspeccionamos los datos
str(meap93)
head(meap93)

#Definimos la nueva variable beneficios 7 savalrio
meap93$b_s <- meap93$benefits / meap93$salary

## Estimamos los 3 modelos
ejemplo_4.10_1 <- lm( lsalary ~ b_s , data = meap93)
ejemplo_4.10_2 <- lm( lsalary ~ b_s + lenroll + lstaff, data = meap93)
ejemplo_4.10_3 <- lm( lsalary ~ b_s + lenroll + lstaff + droprate + gradrate, data=meap93)

export_summs(ejemplo_4.10_1, ejemplo_4.10_2, ejemplo_4.10_3, digits=3)


#La hipotesis nula es que Beta_1 = -1, se realiza el contraste como en el ejemplo anterior
beta_1_1 <- unname(ejemplo_4.10_1$coefficients[2])
se_beta_1_1 <-  summary(ejemplo_4.10_1)$coefficients[2,2]
round(beta_1_1, 3)
round(se_beta_1_1, 3)

## Estadistico t
t_beta_1_1 <- (beta_1_1 - (-1)) / se_beta_1_1
round(t_beta_1_1, 3)

#Comparamos con los valores criticos
## 10%
critico_10 <- qt(p = 0.95, df = 408 - 2)
critico_10

## 5%
critico_5 <- qt(p = 0.975, df = 408 - 2)
critico_5

## 1%
critico_1 <- qt(p = 0.995, df = 408 - 2)
critico_1

#Observamos que el estadistico t es menor que los valores criticos, por lo que no podemos rechazar la hipotesis nula

#Probamos ahora con el segundo modelo
beta_1_2 <- unname(ejemplo_4.10_2$coefficients[2])
se_beta_1_2 <-  summary(ejemplo_4.10_2)$coefficients[2,2]
round(beta_1_2, 3)
round(se_beta_1_2, 3)

## Estadistico t
t_beta_1_2 <- (beta_1_2 - (-1)) / se_beta_1_2
round(t_beta_1_2, 3)

#Comparamos con los valores criticos
## 10%
critico_10 <- qt(p = 0.95, df = 408 - 4)
critico_10

## 5%
critico_5 <- qt(p = 0.975, df = 408 - 4)
critico_5

## 1%
critico_1 <- qt(p = 0.995, df = 408 - 4)
critico_1

#Se rechaza la hipotesis nula al 5%, pero no al 1%

#Probamos ahora con el tercer modelo
beta_1_3 <- unname(ejemplo_4.10_3$coefficients[2])
se_beta_1_3 <-  summary(ejemplo_4.10_3)$coefficients[2,2]
round(beta_1_3, 3)
round(se_beta_1_3, 3)

## Estadistico t
t_beta_1_3 <- (beta_1_3 - (-1)) / se_beta_1_3
round(t_beta_1_3, 3)

#Comparamos con los valores criticos
## 10%
critico_10 <- qt(p = 0.95, df = 408 - 6)
critico_10

## 5%
critico_5 <- qt(p = 0.975, df = 408 - 6)
critico_5

## 1%
critico_1 <- qt(p = 0.995, df = 408 - 6)
critico_1

#Al igual que el caso anterior, se rechaza la hipotesis nula al 5%, pero no al 1%

# Ejemplo 7.2 

## Cargamos los datos
data(gpa1)

## Inspeccionamos los datos
str(gpa1)
head(gpa1)

## Estimamos el modelo
ejemplo_7.2 <-  lm(formula =  colGPA ~ PC + hsGPA + ACT, data=gpa1)
summ(ejemplo_7.2, digits = 3)

#Observamos como el efecto de tener PC es estadisticamente significativo
#Verificar que se entiende la interpretacion del parametro de PC, tal y como se explica en el ejemplo

### Estadistico t de PC (para seguir el libro, aunque con la información de la tabla de `summ` ya es suficiente)
beta_1 <- unname(ejemplo_7.2$coefficients[2])
se_beta_1 <-  summary(ejemplo_7.2)$coefficients[2,2]
round(beta_1, 3)
round(se_beta_1, 3)

## Estadistico t
t_beta_1<- (beta_1 - 0) / se_beta_1
round(t_beta_1, 3)

#Comparamos con los valores criticos
## 10%
critico_10 <- qt(p = 0.95, df = 141 - 4)
critico_10

## 5%
critico_5 <- qt(p = 0.975, df = 141 - 4)
critico_5

## 1%
critico_1 <- qt(p = 0.995, df = 141 - 4)
critico_1

## Segundo modelo eliminando las otras dos variables
ejemplo_7.2_2 <-  lm(formula =  colGPA ~ PC , data=gpa1)
summ(ejemplo_7.2_2, digits = 3)

#Que coincide con los resultados del libro

# Ejemplo 7.3

## Cargamos los datos
data("jtrain")

## Inspeccionamos los datos
str(jtrain)
head(jtrain)

#En el caso de este modelo, solo tenemos que utilizar los datos de 1988, asi que hay que filtralos primero
jtrain98 <- jtrain[jtrain$year == 1988, ]

## Estimamos el modelo
ejemplo_7.3 <-  lm(formula =  hrsemp ~ grant + lsales + lemploy, data=jtrain98)
summ(ejemplo_7.3, digits = 2)

#Verificar que se entiende la explicacion del ejemplo en el libro

# Ejemplo 7.4

## Cargamos los datos
data("hprice1")

## Inspeccionamos los datos
str(hprice1)
head(hprice1)

## Estimamos el modelo
ejemplo_7.4 <-  lm(formula =  lprice ~ llotsize + lsqrft + bdrms + colonial, data=hprice1)
summ(ejemplo_7.4, digits = 3)

#Verificar que se entiende la explicacion del ejemplo en el libro

# Ejemplo 7.5

## Cargamos los datos
data("wage1")

## Inspeccionamos los datos
str(wage1)
head(wage1)

## Estimamos el modelo

#Para introducir las variables al cuadrado dentro de una formula de R, se utilizar la funcion `I()` 

ejemplo_7.5 <-  lm(formula =  lwage ~ female + educ + exper + I(exper^2) + tenure + I(tenure^2), data=wage1)
summ(ejemplo_7.5, digits = 3)

#Verificar que se entiende la explicacion del ejemplo en el libro

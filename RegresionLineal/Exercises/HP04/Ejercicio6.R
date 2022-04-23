## Cargamos las librerías 

pacman::p_load("wooldridge", "sandwich", "jtools", "huxtable")

# Ejemplo 4.3

## Cargamos los datos
data(gpa1)

## Inspeccionamos los datos
str(gpa1)
head(gpa1)

## Estimamos el modelo
ejemplo_4.3 <-  lm(formula =  colGPA ~ hsGPA+ACT+skipped, data=gpa1)
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

#Los t-valores de hsGPA y skipped superan el valor critico al 1%. El t-valor no supera a ninguno de los 3 valores criticos, no es estadisticamente significativo a niguno de los 3 niveles de confianza

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

#Construimos un intervalo de confianza del para los paramteros de la regresion
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
ejemplo_4.8_1 <- lm( lsalary ~ b_s , data = meap93)
ejemplo_4.8_2 <- lm( lsalary ~ b_s + lenroll + lstaff, data = meap93)
ejemplo_4.8_3 <- lm( lsalary ~ b_s + lenroll + lstaff + droprate + gradrate, data=meap93)

export_summs(ejemplo_4.8_1, ejemplo_4.8_2, ejemplo_4.8_3, digits=3)

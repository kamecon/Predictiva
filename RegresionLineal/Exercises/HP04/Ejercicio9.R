# Ejercicio 9

#Segun la expresion para obtener el vector de betas, que podrán encontrar en la diapositiva 19 de la presentación de clase "Regresión lineal múltiple: estimación", podemos obtener los beta's multiplicando la matriz por el vector

Matriz <- matrix(data = c(3.48, -0.104, -1.801, -0.104, 0.0034, 0.049, -1.801, 0.049, 1.352),
                 nrow = 3,
                 ncol = 3)
Vector <- c(96,2458,46)

#Multiplicamos
Betas <- Matriz %*% Vector 

## Calculamos la varianza estimada de los betas y su error estándar:
Vbetahat <- 1.67 * Matriz
seBeta_hat <- sqrt( diag(Vbetahat))
seBeta_hat

## Calculamos el estadístico t asociado a Beta_2
beta3_t <- (Betas[3,1]-0)/seBeta_hat[3]
beta3_t

## Comparamos con el valor critico
qt(p = 0.975, df = 75-3)

#En este caso el estadistico t es mayor que el valor critico, por lo que el parametro es estadisticamente distinto de cero y si tiene influencia la audiencia sobre el precio del anuncio

## Prediccion
prediccion <- Betas[1,1] + Betas[2,1]*17 +  Betas[3,1]*5   
prediccion



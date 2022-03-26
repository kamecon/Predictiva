
#Distribuciones t y F (sacado del libro Learning Statistics with R de Danielle Navarro https://learningstatisticswithr.com/)

# La distribución chi cuadrado es una función "deducida de la normal", es decir, que se obtiene a partir de manipulaciones de realizaciones de la distribución normal. Especificamente, la chi cuadrado es el resultado de la suma de variables aleatorias normales estándar elevadas al cuadrado, replicamos este procedimiento con R

set.seed(1234)

#Generamos 3 normales estandar de 1000 realizaciones cada una

normal.a <- rnorm( n=1000 )
normal.b <- rnorm( n=1000 ) 
normal.c <- rnorm( n=1000 ) 

#Hacemos la suma al cuadrado 3 veces

chicuadrado.3 <- (normal.a)^2 + (normal.b)^2 + (normal.c)^2

#Tenemos una chi cuadrado con 3 grados de libertad

hist(chicuadrado.3, xlab = "", ylab = "", main = "Chi-cuadrado con 3 grados de libertad", freq = FALSE)

#Generasmos una chi cuadrado de 20 grados de libertad usando la funcion `chisq` de R

chicuadrado.20 <- rchisq( 1000, 20)

hist(chicuadrado.20, xlab = "", ylab = "", main = "Chi-cuadrado con 20 grados de libertad", freq = FALSE)


#La distribución F se parece un poco a una distribución chi cuadrado, y surge siempre que se necesita comparar dos distribuciones chi cuadrado entre sí. 
# La distribución chi cuadrado surge cuando estamos tomando una "suma de cuadrados", si quieres comparar dos "sumas de cuadrados" diferentes probablemente estás hablando de algo que tiene una distribución F. 


F.10.2 <- (chicuadrado.3/3)/(chicuadrado.20/20)

hist(F.10.2, xlab = "", ylab = "", main = "F con 3 y 20 grados de libertad", freq = FALSE)

#Distribuciones en R

# Distribución normal:
# qnorm: le das una probabilidad (por ejemplo 5%), te va a dar es el valor tal que
# a la derecha o a la izquierda tengas esa probabilidad
qnorm(0.05) # Número tal que el 5% de probabilidad está a la izquierda
qnorm(0.05, lower.tail = FALSE) # Número tal que el 5% de probabilidad está a la derecha

# pnorm: le das un número y te da la probabilidad a la izquierda o a la derecha
pnorm(1.83) # La probabilidad a la izquierda
pnorm(1.83, lower.tail = FALSE) # La probabilidad a la derecha

# Distribución t (familia de distribuciones indexado por los grados de libertad):
# qt: le das una probabilidad (por ejemplo 5%), te va a dar es el valor tal que
# a la derecha o a la izquierda tengas esa probabilidad
qt(0.05, df = 18) # Número tal que el 5% de probabilidad está a la izquierda
qt(0.05, df = 18, lower.tail = FALSE) # Número tal que el 5% de probabilidad está a la derecha

# pt: le das un número y te da la probabilidad a la izquierda o a la derecha
pt(1.83, df = 18) # La probabilidad a la izquierda
pt(1.83, df = 18, lower.tail = FALSE) # La probabilidad a la derecha

# Distribución chi-cuadrado (familia de distribuciones indexado por los grados de libertad):
# qchisq: le das una probabilidad (por ejemplo 5%), te va a dar es el valor tal que
# a la derecha o a la izquierda tengas esa probabilidad
qchisq(0.05, df = 18) # Número tal que el 5% de probabilidad está a la izquierda
qchisq(0.05, df = 18, lower.tail = FALSE) # Número tal que el 5% de probabilidad está a la derecha

# pchisq: le das un número y te da la probabilidad a la izquierda o a la derecha
pchisq(27, df = 18) # La probabilidad a la izquierda
pchisq(27, df = 18, lower.tail = FALSE) # La probabilidad a la derecha

#Distribucion F
#INTENTAR HACERLO POR SU CUENTA SIGUIENDO LO ANTERIOR
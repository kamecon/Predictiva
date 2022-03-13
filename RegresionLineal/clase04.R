# Cargamos las librerias --------------------------------------------------

#Vamos a cargar las librerias con "pacman"

#Verificamos si esta instalado la libreria pacman, en caso que no lo este, se instala
if (! ('pacman' %in% installed.packages())) install.packages('pacman')

#Una vez instalada, este comando carga la libreria, y si no esta instalada, lo hace
pacman::p_load("haven", "wooldridge")



# Practica regresion lineal -----------------------------------------------

caschool <- read_dta("RegresionLineal/Data/caschool.dta")

estimacion_01 <- lm(formula = testscr ~str, data = caschool)

summary(estimacion_01)

#Acceder a los coeficientes
estimacion_01$coefficients
estimacion_01$coefficients[1]
estimacion_01$coefficients[2]

coef(estimacion_01)

####
## Adicional: Regresión robusta ##
#Se explicara esto más adelante con detalle

pacman::p_load("lmtest", "sandwich")

coeftest(estimacion_01, vcov = vcovHC(estimacion_01, type = "HC0"))

###

#Acceder a mas informacion (R cuadrado, ESR)

attributes(summary(estimacion_01))

summary(estimacion_01)$r.squared
summary(estimacion_01)$sigma

#Construir los residuos

bhat <- estimacion_01$coefficients
yhat <- estimacion_01$coefficients[1] + estimacion_01$coefficients[2]*caschool$str
residuo <- caschool$testscr - yhat

plot(residuo, type = "l")
lines(estimacion_01$residuals, col="red")


#Obtener las predicciones del modelo y los residuos. Verlos en una tabla

Str <-  caschool$str
Test_scr <-  caschool$testscr
testscr_hat <- estimacion_01$fitted.values
u_hat <- estimacion_01$residuals

#Usamos el comando cbind() para unir columnas de datos

tabla01 <- cbind(Str, Test_scr, testscr_hat, u_hat)

head(tabla01)

## Ejemplo 2.3 libro Wooldridge

data(ceosal1)

estimacion_wool01 <- lm(formula = salary ~roe, data = ceosal1)
estimacion_wool01

plot(ceosal1$roe, ceosal1$salary, xlab = "ROE", ylab = "Salario de los CEO's")
abline(estimacion_wool01)


## Ejemplo 2.4 libro Wooldridge

#Relacion entre salario (wage) y educacion (educ) (Tarea: inspeccionar los datos)

data("wage1")

estimacion_wool02 <- lm()
estimacion_wool02

plot(, xlab = , ylab = )
abline(estimacion_wool02)

#Ejemplo 2.5 libro Wooldridge

data("vote1")

?vote1

#Relacion entre resultado de las elecciones (voteA) y el gasto en la campaña electoral (shareA)



# Distribucion muestral de los betas --------------------------------------

#Creamos un vector vacio de betas

beta_1 <- vector(length = 1000)

#Para tomar una muestra de un data frame, hacemos lo siguiente

sample(nrow(caschool),100)
caschool[sample(nrow(caschool),100), ]

#Rellenamos el vector con los betas estimados tomando solo una muestra de los datos originales

for (i in seq_along(beta_1)) {
  estima <- lm(data =  caschool[sample(nrow(caschool),100),], formula = testscr ~str)
  beta_1[i] <- estima$coefficients[2]
}

mean(beta_1)
hist(beta_1)

#NOTA DE R: En lugar de la función `seq_along`se puede usar una notación más convencional en otros lenguajes como for (i in 1..1000) o for (i in 1:1000) o for i in range(1000), que significa "repite esta operación en índices que van del 1 al 1000 en saltos de 1" que es una manera sofisticada de decir "repite esta operación 1000 veces" 

#NOTA DE R: En lugar de usar un bucle se puede usar la función `replicate`, una de las funciones de la familia `apply`, las cuales aplican una función a un vector de valores, que es lo mismo que hace un bucle cuando _aplica_ de manera repetida la misma operación a un conjunto de valores representado por un vector. En este caso estamos haciendo algo más sencillo, repetir un número de veces una instrucción encapsulada en una función, en este caso que nos extraiga de manera repetida una muestra de una población y calcule una regresion. Así que `replicate` en nuestro ejemplo repite 1000 veces la operación descrita arriba.
#Este tipo de operaciones se realiza muchas veces en estadística, ya que de esa manera hacemos _simulaciones_, repetir muchas veces realizaciones de una distribución de probabilidad o un fenómeno cuyo resultado sea incierto (lanzar un dado 50000 veces)

betas_1_2 <- replicate(expr = lm(data =  caschool[sample(nrow(caschool),100),], formula = testscr ~str)$coefficients[2],
                          n = 1000)

mean(betas_1_2)
hist(betas_1_2)

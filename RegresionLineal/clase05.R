# Cargamos las librerias --------------------------------------------------

#Vamos a cargar las librerias con "pacman"

#Verificamos si esta instalado la libreria pacman, en caso que no lo este, se instala
if (! ('pacman' %in% installed.packages())) install.packages('pacman')

#Una vez instalada, este comando carga la libreria, y si no esta instalada, lo hace
pacman::p_load("haven", "wooldridge")




# Regresion lineal:parametros ---------------------------------------------

caschool <- read_dta("Intro/Data/caschool.dta")

estimacion_01 <- lm(formula = testscr ~str, data = caschool)
summary(estimacion_01)

#Ya sabemos como acceder a los betas, el ESR y el R^2
#Ahora vamos a acceder a los errores estándar de los parámetros, el t-valor y el p-valor

#Volvemos a los atributos de la función summary

attributes(summary(estimacion_01))

#Inspeccionamos el atributo coefficients

summary(estimacion_01)$coefficients

#Podemos ver que es una matriz de dimension 2,4

class(summary(estimacion_01)$coefficients)
dim(summary(estimacion_01)$coefficients)

#Sabiendo esto, podemos extraer los elementos deseados
#Error estándar de Beta 0
summary(estimacion_01)$coefficients[1,2]
#Error estándar de Beta 1
summary(estimacion_01)$coefficients[2,2]
#Estadístico t para contrastar que Beta 1 es 0
summary(estimacion_01)$coefficients[2,3]
#p-valor del contraste de Beta 1
summary(estimacion_01)$coefficients[2,4]

#NOTA: Si desea comparar el estadístico t con el valor critico, lo puede hacer con la funcion qt()
#Al 99% de confianza (0.05 de probabilidad en cada cola en el caso del contraste bilateral), el estadistico de contraste seria
qt(p = 0.995, df = 418)

#Una manera de comapararlo
t_valor <- summary(estimacion_01)$coefficients[2,3]
contraste <- qt(p = 0.995, df = 418)
ifelse(abs(t_valor)>abs(contraste),"Rechaza","Acepta")

#Intervalos de confianza para Beta 1
confint(estimacion_01)

#Tambien se podria calcular "manualmente" lo anterior
#Para eso, necesitariamos el numero de observaciones
n <- nobs(estimacion_01)
n


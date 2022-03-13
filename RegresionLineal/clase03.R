# Cargamos las librerias ----

#Vamos a cargar las librerías con "pacman"

#Verificamos si esta instalado la libreria pacman, en caso que no lo este, se instala
if (! ('pacman' %in% installed.packages())) install.packages('pacman')

#Una vez instalada, este comando carga la libreria, y si no esta instalada, lo hace
pacman::p_load("haven", "AER")


# Primer ejemplo del libro de Stock y Watson ----

## Dos formas de cargar los datos

### La primera es accediendo a la web del libro y descargar los datos. Acceder a esta dirección https://wps.pearsoned.com/aw_stock_ie_3/178/45691/11696965.cw/index.html y pinchar en "Replication Files" en el menu de la barra izquierda, luego pinchar en "Datasets for Replicating Empirical Results (Original Edition)". Descargar los datos "California Test Score Data Used in Chapters 4-9" en formato STATA.

### Vamos a practicar con el formato de STATA por dos motivos: debemos acostumbrarnos a trabajar con distintos formatos de datos mas alla del csv y xlsx, y porque STATA (de momento) sigue siendo el software estadístico predominante en organimos multilaterales y gubernamentales economicos, asi como en gran parte de la investigacion academica en economia

### Una vez descargados los datos, lo leemos con la libreria haven 

caschool <- read_dta("Intro/Data/caschool.dta")

### Vamos a replicar la regresion lineal del capitulo 4 trabajada en clase, la cual explicaba la calificación de la prueba de habilidad numerica y verbal en funcion del numero de alumnos por profesor

### En R la funcion lm() lleva a cabo la regresion lineal, veamos la ayuda

?lm

### De momento nos concentramos en los primeros dos argumentos de la funcion, formula y data

lm(data = caschool, formula = testscr ~ str)

### Vemos que con el argumento data de la función lm, indicamos de que fuente de datos se deben obtener las variables, otra forma es no indicar dentro de la funcion la fuente de los datos, y usar la funcion attach previo a lm

attach(caschool)
lm(testscr ~ str)

### Asimismo, es deseable referenciar (asignar un nombre) el objeto que surge de la función lm 

estimacion_01 <- lm(testscr ~ str)
estimacion_01

### Más adelante aprenderemos más acerca de la funcion lm

### Para reproducir el gráfico del libro, usamos las funciones plot y abline

plot(str, testscr)
abline(estimacion_01)

### Mas parecido al gráfico del libro

plot(str, testscr,
     ylim = c(600,720),   #limites del eje y
     xlim = c(10,30),     #limites del eje x
     pch = 20,            #forma del simbolo (circulo)
     cex = 1.5,           #tamaño de la forma
     ylab = "Test score", #titulo del eje y
     xlab = "Student-teacher ratio", #titulo del eje x
     axes = FALSE)        #Sin ejes
axis(1) #eje x
axis(2) #eje y
abline(estimacion_01, col = "deepskyblue", lwd =3)


### Si leen el documento de características de los datos, podran ver que las dos variables que hemos usado no estaban en los datos originales, sino que se han construido
### Esto es algo que deben hacer continuamente en cualquier trabajo empirico, por lo que deberian practicarlo
### Para esto, vamos a acceder los mismos datos de otra fuente, la libreraia AER (Applied Econometrics with R), accedamos al Vignette

### Veamos las bases de datos del libro de Stock y Watson

help("StockWatson2007", package = "AER")

### Cargamos los datos 

data(CASchools)

### Construimos las variables

CASchools$STR <- CASchools$students/CASchools$teachers

CASchools$Testscr <- (CASchools$math+CASchools$read)/2

lm(formula = Testscr ~ STR, data = CASchools)


# Regresion Lineal: Poblacion vs muestra ----


#Creamos la función del proceso generador de datos Y=2+3x 

x <- seq(-2, 2, length = 100)
y <- 2 + 3*x

#A partir de 100 realizaciones de una normal con media cero y desviación típica 0.5, creamos el modelo Y=f(x)+u
dt <- 1
y_02 <- y + rnorm(100, 0, dt)
df <- data.frame(x,y,y_02)

#Estimamos el modelo a partir de los datos con ruido
fit1 <- lm(formula = y_02 ~ x, data = df)
fit1

#Repetimos el procedimiento anterior con 100 realizaciones distintas del error y estimamos los parámetros
y_03 <- y + rnorm(100, 0, dt)
df$y_03 <- y_03
fit2 <- lm(formula = y_03 ~ x, data = df)
fit2

#Lo hacemos de nuevgo
y_04 <- y + rnorm(100, 0, 1)
df$y_04 <- y_04
fit3 <- lm(formula = y_04 ~ x, data = df)
fit3

#Observamos que las estimaciones de la ordenada en el origen y la pendiente cambian con distintas realizaciones del ruido

#Graficacmos el proceso generador de datos "real" con las estimaciones de la regresión lineal
plot(x,y_02, ylab = "Y", main = "Figura 3.3 Libro ISLR")
lines(x,y, col="red", lwd=4)
lines(x, fitted(fit1), col=colors()[10*1], lwd=2)

#Vamos a generar 10 nuevos modelos repitiendo lo anterior

#Creamos um data frame vacio de 100 filas (x's) y 10 columnas (modelos), para rellenarlo luego con las distintas versiones del modelo
df2 <- data.frame(matrix(, nrow=100, ncol=10))

#Añadimos una columna con la variable X
df2$x <- x

#Generamos los 10 distintos conjuntos de datos a partir del proceso generador de datos y el ruido

#Primero la columna Y
for (i in 1:10) {
  df2[,i] <- y + rnorm(100, 0, dt)
}

#Luego estimamos los 10 modelos y lo representamos graficamente
for (i in 1:10) {
  fitt <- lm(data = df2, formula = df2[,i]~x)
  lines(x, fitted(fitt), col=colors()[12*i])
}


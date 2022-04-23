
## Cargamos las librerías 

pacman::p_load("sandwich", "haven", "jtools", "huxtable")

# Ejercicio 6.2

## Cargamos los datos 
college_distance <- read_dta("RegresionLineal/Exercises/Data/CollegeDistance.dta")

## Inspeccionamos los datos
str(college_distance)
head(college_distance)

## a)
modelo_distance01 <- lm(data = college_distance, formula =  ed ~ dist)
summ(modelo_distance01, robust = "HC1")


## b)
#En esta direccion pueden descargarse la descripcion de la base de datos https://wps.pearsoned.com/wps/media/objects/11422/11696965/empirical/empex_tb/CollegeDistance_DataDescription.pdf
modelo_distance02 <- lm(data = college_distance, formula =  ed ~ dist + bytest + female + black + hispanic + incomehi + ownhome + dadcoll + cue80 + stwmfg80)
summ(modelo_distance02, robust = "HC1")

#El efecto negativo estimado de la variable Dist sobre la variable ED es menor (-0.03 vs. -0.07)

## d) Ver formula de R cuadrado ajustado

## e) la diferencia en los años de educacion completada de dos individuos idénticos excepto en el hecho de que el padre tenga un titulo universitario

## f) Son indicadores del mercado de trabajo, la tasa de paro del condado en 1980 y el salario por hora estatal en manufactura de 1980
#Discuta cual deberia ser el signo de los coeficientes

## g) Creamos un data frame donde introducimos los datos del enunciado en el mismo orden que aparecen en el objeto lm que hemos estimado y con los nombres de cada variable (ver la descripcion de los datos y el head de la base de datos para ver como se representan los datos en la base)

bob <- as.data.frame( list(20, 58, 0, 1, 0, 1, 1, 0, 7.5, 9.75), col.names = tail(names(modelo_distance02$coefficients),-1))
bob

#Usamos la función `predict` la cual nos da la predicción del modelo con datos nuevos no presentes en los datos de entrenamiento

predict(object = modelo_distance02, newdata = bob)

#Este seria el numero de años de estudio completado por Bob

### h)

jim <- as.data.frame( list(40, 58, 0, 1, 0, 1, 1, 0, 7.5, 9.75), col.names = tail(names(modelo_distance02$coefficients),-1))
jim

predict(object = modelo_distance02, newdata = jim)


# Ejercicio 7.2

## Cargamos los datos 
ratings <- read_dta("RegresionLineal/Exercises/Data/TeachingRatings.dta")

## Inspeccionamos los datos
str(ratings)
head(ratings)

## a)
modelo_ratings01 <- lm(data = ratings, formula = course_eval  ~ beauty)
summ(modelo_ratings01, robust = "HC1", confint = TRUE)


## b)
#En esta direccion pueden descargarse la descripcion de la base de datos https://wps.pearsoned.com/wps/media/objects/11422/11696965/empirical/empex_tb/TeachingRatings_Description.pdf

#Incluimos todas las variables
modelo_ratings02 <- lm(data = ratings, formula = course_eval  ~ . )
summ(modelo_ratings02, robust = "HC1")

#La edad y que el curso sea introductorio o no, no poseen un efecto sobre ratings estadisticamente distinto de cero
#Repetimos la regrsión sin esas variables
modelo_ratings03 <- lm(data = ratings, formula = course_eval  ~ . - age - intro )
summ(modelo_ratings03, robust = "HC1")

#Incluimos los intervalos de confianza
summ(modelo_ratings03, robust = "HC1", confint = TRUE)

#El intervalo de confianza no cambia de manera abrupta, pasa de (0.07  , 0.20) a (0.10 , 0.23)
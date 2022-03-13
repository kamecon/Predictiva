# Cargamos las librerias --------------------------------------------------

#Vamos a cargar las librerias con "pacman"

#Verificamos si esta instalado la libreria pacman, en caso que no lo este, se instala
if (! ('pacman' %in% installed.packages())) install.packages('pacman')

#Una vez instalada, este comando carga la libreria, y si no esta instalada, lo hace
pacman::p_load("haven")


# Soluciones --------------------------------------------------------------

#E.4.2

#Leemos los datos (Recuerde cambiar la ruta)
ratings <- read_dta("TeachingRatings.dta")

#Inspeccionamos los datos
str(ratings)
head(ratings)

#Estimamos el modelo
modelo_ratings <- lm(data = ratings, formula = course_eval  ~ beauty)

#Vemos los resultados
summary(modelo_ratings)

#E.4.4

#Leemos los datos (Recuerde cambiar la ruta)
growth <- read_dta("Growth.dta")

#Inspeccionamos los datos
str(growth)
head(growth)

#Estimamos el modelo
modelo_growth <- lm(data = growth, formula = growth ~ tradeshare)

#Vemos los resultados
summary(modelo_growth)

#Hacemos el gráfico
plot(growth$tradeshare, growth$growth)

#Estimamos el modelo sin Malta (eliminamos la última fila, donde esta dicho pais)
modelo_growth <- lm(data = growth[-65,], formula = growth ~ tradeshare)

#Vemos los resultados
summary(modelo_growth)

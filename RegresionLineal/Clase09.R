pacman::p_load(haven,jtools)

#Cargamos los datos
caschool <- read_dta("RegresionLineal/Data/caschool.dta")

head(caschool)

#Estimamos el modelo con todos los datos (comparar con tabla 7.1 del libro)
modelo_full <- lm(data = caschool, formula = testscr ~ str + el_pct + meal_pct + calw_pct)
summ(modelo_full, robust = "HC1", digits = 3)

#Dividimos la muestra, dejamos algunas observaciones fuera, para luego predecri con las caracteristicas de estas observaciones
set.seed(1234)

#Determinamos de manera aleatoria que elementos sacamos de la muestra
filas <- sample(x = rownames(caschool),20)
filas

#Dividimos la muestra en dos
caschool_train <- caschool[-as.numeric(filas),]
caschool_test <- caschool[as.numeric(filas),]

#Estimamos con los datos restantes
modelo_train <- lm(data = caschool_train, formula = testscr ~ str + el_pct + meal_pct + calw_pct)
summ(modelo_train, robust = "HC1", digits = 3)

#Predecimos los valores de las observaciones que hemos dejado fuera
prediccion <-  predict(object = modelo_train, newdata = caschool_test, interval = "prediction", level = 0.95)
prediccion

#Construimos una tabla con el valor real de la variable dependiente
prediccion2 <-  cbind(prediccion, caschool_test$testscr)
prediccion2

#colocamos un nombre a la ultima columna
colnames(prediccion2)

colnames(prediccion2)[4] <- "real"

prediccion2

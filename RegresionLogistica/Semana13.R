pacman::p_load("AER", "sandwich", "jtools", "huxtable", "caret", "mfx", "stargazer", "caTools", "ROCR")

#Repetimos la estimacion del libro
data("HMDA")
HMDA$deny2 <- as.numeric(HMDA$deny) - 1

modelo_cap11_06 <- glm(data = HMDA, deny2 ~ pirat + afam, family = binomial(link = logit))
summ(modelo_cap11_06, digits = 3, robust = "HC1")

#Calculamos los efectos marginales
modelo_cap11_06_marg <- logitmfx(data = HMDA, deny2 ~ pirat + afam, atmean = FALSE, robust = TRUE)
modelo_cap11_06_marg

#Dividimos la muestra, dejamos algunas observaciones fuera, para luego predecir con las caracteristicas de estas observaciones
set.seed(1234)

#Determinamos de manera aleatoria que elementos sacamos de la muestra
filas <- sample(x = rownames(HMDA),300)
filas

#Dividimos la muestra en dos

HMDA_train <- HMDA[-as.numeric(filas),]
HMDA_test <- HMDA[as.numeric(filas),]

#Estimamos con los datos restantes
modelo_cap11_train <- glm(data = HMDA_train, deny2 ~ pirat + afam, family = binomial(link = logit))
summ(modelo_cap11_train, digits = 3, robust = "HC1")


#Predecimos los valores de las observaciones que hemos dejado fuera
prediccion <-  predict(object = modelo_cap11_train, newdata = HMDA_test, type = "response")
prediccion

prediccion_deny <- ifelse(prediccion > 0.5, 1, 0)
prediccion_deny

#Casos
table(HMDA_test$deny2)

#Matriz de confusion
table(prediccion_deny, HMDA_test$deny2)

#Reescribimos las predicciones en terminos de la variable original deny y lo convertimos en factor
prediccion_deny2 <- ifelse(prediccion > 0.5, "yes", "no")
prediccion_deny2 <- as.factor(x = prediccion_deny2)

#Matriz de confusion con caret
confusionMatrix(prediccion_deny2, HMDA_test$deny, positive = "yes")

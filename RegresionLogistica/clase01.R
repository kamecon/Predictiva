pacman::p_load("ISLR2", "AER", "sandwich", "jtools", "huxtable")

ISLR2::Default

colores <- c("blue", "orange")
formas <- c(1,3)

# set.seed(123)
# filas <- sample(x = rownames(Default),5000)
# filas
# 
# Default_muestra <- Default[as.numeric(filas),]

plot(Default$balance, Default$income,
     xlab = "Balance",
     ylab = "Income",
     col = colores[Default$default],
     pch=formas[Default$default],
     cex=1,
     lwd=2)

?Default

table(Default$default)
prop.table(table(Default$default))

oldpar <- par( mfrow=c(1,2))
plot(x = Default$default, y = Default$balance,
     xlab = "Default",
     ylab= "Balance",
     col = colores)
plot(x = Default$default, y = Default$income,
     xlab = "Default",
     ylab= "Balance",
     col = colores)
par(oldpar)


Default$probabilidad <- ifelse(Default$default == "No", 0, 1)

modelo_lineal <- lm(data = Default, formula = as.numeric(Default$probabilidad) ~ balance)

plot(Default$balance, Default$probabilidad, col = "orange")
abline(modelo_lineal, col = "blue", lwd = 2)

modelo_logistico <- glm(data = Default, formula = probabilidad ~balance, family = binomial)
summ(modelo_logistico, digits = 4)

nuevo <- data.frame(balance = c(1000, 2000))
prediccion <-  predict(modelo_logistico, nuevo, type = "response")
prediccion

Default$estudiante <- ifelse(Default$student == "No", 0, 1)

modelo_logistico2 <- glm(data = Default, formula = probabilidad ~ student, family = binomial)
summ(modelo_logistico2, digits = 4)

nuevo2 <- data.frame(student = as.factor(c("Yes", "No")))
prediccion2 <-  predict(modelo_logistico2, nuevo2, type = "response")
prediccion2
round(prediccion2, 4)

#Ejemplo Stock y Watson capitulo 11

data("HMDA")

?HMDA

head(HMDA)
summary(HMDA)

table(HMDA$deny)
prop.table(table(HMDA$deny))

plot(HMDA$pirat, HMDA$deny)

modelo_cap11_01 <- lm(data = HMDA, deny ~ pirat)

HMDA$deny2 <- as.numeric(HMDA$deny) - 1

plot(HMDA$pirat, HMDA$deny2)

modelo_cap11_01 <- lm(data = HMDA, deny2 ~ pirat)
summ(modelo_cap11_01, digits = 3, robust = "HC1")

plot(HMDA$pirat, HMDA$deny2)
abline(modelo_cap11_01)

# Grafico del libro, sacado de https://www.econometrics-with-r.org/ capitulo 11
plot(x = HMDA$pirat, 
     y = HMDA$deny2,
     main = "Diagrama de dispersión de las denegaciones en solicitudes de hipoteca y ratio pagos-ingresos",
     xlab = "Ratio P/I",
     ylab = "Denegar",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Hipoteca denegada")
text(2.5, -0.1, cex= 0.8, "Hipoteca concedida") 
text(1.75, 0.5, cex= 0.8, "Modelo de probabilidad lineal")

# add the estimated regression line
abline(modelo_cap11_01, 
       lwd = 2, 
       col = "steelblue")

table(HMDA$afam)
prop.table(table(HMDA$afam))

modelo_cap11_02 <- lm(data = HMDA, deny2 ~ pirat + afam)
summ(modelo_cap11_02, digits = 3, robust = "HC1")

pnorm(q = -0.8)


modelo_cap11_03 <- glm(data = HMDA, deny2 ~ pirat, family = binomial(link = probit))
summ(modelo_cap11_03, digits = 3, robust = "HC1")

nuevo <- data.frame(pirat = c(0.3, 0.4))
prediccion <-  predict(modelo_cap11_03, nuevo, type = "response")
prediccion
round(prediccion, 3)
(prediccion[2]-prediccion[1])*100
diff(prediccion)

# Grafico del libro, sacado de https://www.econometrics-with-r.org/ capitulo 11
plot(x = HMDA$pirat, 
     y = HMDA$deny2,
     main = "Modelo Probit de la probabilidad de denegación. Dada la ratio P/I",
     xlab = "Ratio P/I",
     ylab = "Denegar",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.85)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Hipoteca denegada")
text(2.5, -0.1, cex= 0.8, "Hipoteca concedida") 
text(1.25, 0.5, cex= 0.8, "Modelo Probit")

# add estimated regression line
x <- seq(0, 3, 0.01)
y <- predict(modelo_cap11_03, list(pirat = x), type = "response")

lines(x, y, lwd = 2, col = "steelblue")

modelo_cap11_04 <- glm(data = HMDA, deny2 ~ pirat + afam, family = binomial(link = probit))
summ(modelo_cap11_04, digits = 3, robust = "HC1")

nuevo2 <- data.frame(afam = as.factor(c("no", "yes")), pirat=c(0.3, 0.3))
prediccion2 <-  predict(modelo_cap11_04, nuevo2, type = "response")
prediccion2
round(prediccion2, 3)
(prediccion2[2]-prediccion2[1])*100

modelo_cap11_05 <- glm(data = HMDA, deny2 ~ pirat, family = binomial(link = logit))
summ(modelo_cap11_05, digits = 3, robust = "HC1")

modelo_cap11_06 <- glm(data = HMDA, deny2 ~ pirat + afam, family = binomial(link = logit))
summ(modelo_cap11_06, digits = 3, robust = "HC1")



# Grafico del libro, sacado de https://www.econometrics-with-r.org/ capitulo 11
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Modelos Probit y Logit  para la probabilidad de denegación, dada la variable ratio P/I",
     xlab = "Ratio P/I ",
     ylab = "Denegar",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.9)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Hipoteca denegada")
text(2.5, -0.1, cex= 0.8, "Hipoteca concedida") 

# add estimated regression line of Probit and Logit models
x <- seq(0, 3, 0.01)
y_probit <- predict(modelo_cap11_03, list(pirat = x), type = "response")
y_logit <- predict(modelo_cap11_05, list(pirat = x), type = "response")

lines(x, y_probit, lwd = 1.5, col = "steelblue")
lines(x, y_logit, lwd = 1.5, col = "black", lty = 2)

# add a legend
legend("topleft",
       horiz = TRUE,
       legend = c("Probit", "Logit"),
       col = c("steelblue", "black"), 
       lty = c(1, 2))

# Evaluación del modelo

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

table(HMDA_test$deny2)
table(prediccion_deny, HMDA_test$deny2)
mean(prediccion_deny == HMDA_test$deny2)
mean(prediccion_deny != HMDA_test$deny2)


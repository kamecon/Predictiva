pacman::p_load("AER", "sandwich", "jtools", "huxtable")

#Ejemplo Stock y Watson capitulo 11

data("HMDA")

?HMDA

head(HMDA)
summary(HMDA)

table(HMDA$deny)
prop.table(table(HMDA$deny))

plot(HMDA$pirat, HMDA$deny)

# Ecuacion 11.1
modelo_cap11_01 <- lm(data = , formula = )

HMDA$deny2 <- 

plot(HMDA$pirat, HMDA$deny2)

modelo_cap11_01 <- lm(data = , formula = )
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

abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Hipoteca denegada")
text(2.5, -0.1, cex= 0.8, "Hipoteca concedida") 
text(1.75, 0.5, cex= 0.8, "Modelo de probabilidad lineal")

abline(modelo_cap11_01, 
       lwd = 2, 
       col = "steelblue")

# Ecuacion 11.3

table(HMDA$afam)
prop.table(table(HMDA$afam))

modelo_cap11_02 <- lm(data = , formula = )
summ(modelo_cap11_02, digits = 3, robust = "HC1")

pnorm(q = -0.8)


# Ecuacion 11.7
modelo_cap11_03 <- glm(data = , formula = , family = binomial(link = probit))
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

abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Hipoteca denegada")
text(2.5, -0.1, cex= 0.8, "Hipoteca concedida") 
text(1.25, 0.5, cex= 0.8, "Modelo Probit")

x <- seq(0, 3, 0.01)
y <- predict(modelo_cap11_03, list(pirat = x), type = "response")

lines(x, y, lwd = 2, col = "steelblue")

# Ecuacion 11.7 + afroamericano
modelo_cap11_04 <- glm(data = , , family = binomial(link = probit))
summ(modelo_cap11_04, digits = 3, robust = "HC1")

nuevo2 <- data.frame(afam = as.factor(c("no", "yes")), pirat=c(0.3, 0.3))
prediccion2 <-  predict(modelo_cap11_04, nuevo2, type = "response")
prediccion2
round(prediccion2, 3)
(prediccion2[2]-prediccion2[1])*100


# Ecuacion 11.10 (2 versiones)
modelo_cap11_05 <- glm(data = , , family = binomial(link = logit))
summ(modelo_cap11_05, digits = 3, robust = "HC1")

modelo_cap11_06 <- glm(data = , , family = binomial(link = logit))
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

abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Hipoteca denegada")
text(2.5, -0.1, cex= 0.8, "Hipoteca concedida") 

x <- seq(0, 3, 0.01)
y_probit <- predict(modelo_cap11_03, list(pirat = x), type = "response")
y_logit <- predict(modelo_cap11_05, list(pirat = x), type = "response")

lines(x, y_probit, lwd = 1.5, col = "steelblue")
lines(x, y_logit, lwd = 1.5, col = "black", lty = 2)

legend("topleft",
       horiz = TRUE,
       legend = c("Probit", "Logit"),
       col = c("steelblue", "black"), 
       lty = c(1, 2))

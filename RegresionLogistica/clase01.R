library(ISLR2)
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

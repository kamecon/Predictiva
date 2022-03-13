#Ejemplo sacado de Aprendizaje Estadístico de Rubén Fernández Casal, Julián
#Costa Bouzas y Manuel Oviedo de la Fuente https://rubenfcasal.github.io/aprendizaje_estadistico/

# Simulación datos
n <- 30
x <- seq(0, 1, length = n)

#Polinomio grado 4 2+4(5x-1)(4x-2)(x-0.8)^2
mu <- 2 + 4*(5*x - 1)*(4*x - 2)*(x - 0.8)^2 
sd <- 0.5

set.seed(1)
y <- mu + rnorm(n, 0, sd)
plot(x, y) 
lines(x, mu, lwd = 2)

# Ajuste de los modelos
#Lineal
fit1 <- lm(y ~ x)
lines(x, fitted(fit1))

fit2 <- lm(y ~ poly(x, 4))
lines(x, fitted(fit2), lty = 2)

fit3 <- lm(y ~ poly(x, 20)) 
# NOTA: poly(x, degree, raw = FALSE) tiene un problema de desbordamiento si degree > 25
lines(x, fitted(fit3), lty = 3)

legend("topright", legend = c("Verdadero", "Ajuste con grado 1", 
                              "Ajuste con grado 4", "Ajuste con grado 20"), 
       lty = c(1, 1, 2, 3), lwd = c(2, 1, 1, 1))


#Bondad de ajuste
knitr::kable(t(sapply(list(Fit1 = fit1, Fit2 = fit2, Fit3 = fit3), 
                      function(x) with(summary(x), 
                                       c(MSE = mean(residuals^2), R2 = r.squared, R2adj = adj.r.squared)))), 
             caption="Medidas de bondad de ajuste de los modelos polinómicos.", digits = 2, position = "!htb")


#si generamos nuevas respuestas de este proceso, la precisión del modelo más complejo empeorará considerablemente:

y.new <- mu + rnorm(n, 0, sd)
plot(x, y) 
points(x, y.new, pch = 2)
lines(x, mu, lwd = 2)
lines(x, fitted(fit1))
lines(x, fitted(fit2), lty = 2)
lines(x, fitted(fit3), lty = 3)
legend("topright", legend = c("Verdadero", "Muestra", "Ajuste con grado 1", "Ajuste con grado 4", 
                              "Ajuste con grado 20", "Nuevas observaciones"), 
       lty = c(1, NA, 1, 2, 3, NA), lwd = c(2, NA, 1, 1, 1, NA), pch = c(NA, 1, NA, NA, NA, 2))

#Veamos como va con nuevos datos

MSEP <- sapply(list(fit1 = fit1, fit2 = fit2, fit3 = fit3), 
               function(x) mean((y.new - fitted(x))^2))
MSEP

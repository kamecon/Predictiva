#Cargamos las librerias
pacman::p_load(haven, wooldridge)

#Motivación: Regresion lineal multiple
caschool <- read_dta("RegresionLineal/Data/caschool.dta")

modelo_cas01 <- lm(data = caschool, testscr ~ str)
summary(modelo_cas01)

modelo_cas02 <- lm(data = caschool, testscr ~ str + avginc)
summary(modelo_cas02)

#Ejemplo 3.2 libro Wooldridge pag. 76 (salario real por hora)
#526 observaciones sobre trabajadores en la base de datos WAGE1, las variables educ (años de educación), exper (años de experiencia en el mercado laboral) y tenure (años de antigüedad en el empleo actual) se incluyen en una ecuación para explicar log(wage)

data("wage1")

estimacion_wage <- lm(data = wage1, log(wage) ~ educ + exper + tenure)
summary(estimacion_wage)

#Estadistico de contraste
qt(p = 0.975, df = 522)

#p-valor
pt(2.391, df = 522, lower.tail = FALSE)*2


#Regresión lineal multiple en notacion matricial. Ejemplo sacado de http://www.urfie.net/downloads03.html (sería interesante revisar este libro para replicar en R los ejemplos del libro de Wooldridge)
data(gpa1)

# Tamaño de la muestra y numero de regresores
n <- nrow(gpa1)
k <- 2

# Extraemos la variable independiente y
y <- gpa1$colGPA

# Construimos la matriz de regresores X (en este caso 2) y añadimos una columna de unos (ver diapositiva 9)
X <- cbind(1, gpa1$hsGPA, gpa1$ACT)

# Display first rows of X:
head(X)

# Estimamos los betas usando la expresión de la diapositiva 18:
beta_hat <- solve( t(X)%*%X ) %*% t(X)%*%y 
beta_hat

# Calculamos los residuos
uhat <- y - X %*% beta_hat

# Calculamos la varianza estimada de u y el ESR:
sigsqhat <- as.numeric( t(uhat) %*% uhat / (n-k-1) )
ESR <- sqrt(sigsqhat) 
sigsqhat
ESR

# Varianza estimada de los betas y su error estándar:
Vbetahat <- sigsqhat * solve( t(X)%*%X )
seBeta_hat <- sqrt( diag(Vbetahat))
seBeta_hat

#Verificamos los valores
modelo_gpa01 <- lm(data = gpa1, formula = colGPA ~ hsGPA + ACT )
summary(modelo_gpa01)

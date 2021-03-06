pacman::p_load(ISLR, ISLR2, wooldridge, jtools)

ISLR2::Credit

head(Credit)

?Credit

plot(Credit$Limit, Credit$Age)

plot(Credit$Limit, Credit$Rating)

plot(Credit)

model_1 <- lm(data = Credit, formula = Balance ~ Age + Limit)
summ(model_1, digits = 3)

model_2 <- lm(data = Credit, formula = Balance ~ Rating + Limit)
summ(model_2, digits = 3)

credito <- ISLR2::Credit

credito$propietario <- ifelse(credito$Own == "Yes", 1, 0)

model_3 <- lm(data = credito, formula = Balance ~ propietario)
summ(model_3)

wooldridge::wage1

head(wage1)

model_04 <- lm(data = wage1, formula = wage ~ female + educ + exper + tenure)
summ(model_04)

# Ejemplo pag. 97 ISLR

head(Credit)

model_b <- lm(data = Credit, formula = Balance ~ Income + Student)
summ(model_b, digits = 4)

credito <- Credit

credito$estudiante <- ifelse(credito$Student == "Yes", 1, 0)

head(credito)

model_b2 <- lm(data = credito, formula = Balance ~ Income + estudiante)
summ(model_b2, digits = 4)

# Interacciones

model_b3 <- lm(data = credito, formula = Balance ~ Income + estudiante + Income*estudiante)
summ(model_b3, digits = 4)

Advertising <- read.csv("Intro/Data/Advertising.csv")

modelo_publicidad <- lm(data = Advertising, formula = sales ~ TV + radio + newspaper)
summ(modelo_publicidad, digits = 4)

modelo_publicidad2 <- lm(data = Advertising, formula = sales ~ TV + radio)
summ(modelo_publicidad2, digits = 4)

modelo_publicidad3 <- lm(data = Advertising, formula = sales ~ TV + radio + TV*radio)
summ(modelo_publicidad3, digits = 4)


pacman::p_load(ggiraphExtra)

ggPredict(modelo_publicidad3, se = FALSE, interactive = TRUE)

ggPredict(model_3, se = FALSE, interactive = TRUE)

ggPredict(model_b, se = FALSE, interactive = TRUE)

model_041 <- lm(data = wage1, formula = wage ~  educ + exper + female)
ggPredict(model_041, se = FALSE, interactive = TRUE)


pacman::p_load(performance)

oldpar <- par( mfrow=c(2,2))
plot(modelo_publicidad3)
par(oldpar)

check_model(modelo_publicidad3)
check_autocorrelation(modelo_publicidad3)
check_collinearity(modelo_publicidad3)
check_heteroscedasticity(modelo_publicidad3)
check_normality(modelo_publicidad3)


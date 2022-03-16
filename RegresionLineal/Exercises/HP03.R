library(haven)


cps08 <- read_dta("RegresionLineal/Exercises/Data/cps08.dta")

modelo_cps08 <- lm(data = cps08, formula = ahe  ~ age)
modelo_cps08
summary(modelo_cps08)

summary(modelo_cps08)$coefficients[2,4]

confint(modelo_cps08)

beta_1 <- unname(modelo_cps08$coefficients[2])
se_beta_1 <- summary(modelo_cps08)$coefficients[2,2]

limite_sup <- beta_1 + 1.96*se_beta_1
limite_inf <- beta_1 - 1.96*se_beta_1

limite_inf
limite_sup

#5.2

ratings <- read_dta("RegresionLineal/Exercises/Data/TeachingRatings.dta")

modelo_ratings <- lm(data = ratings, formula = course_eval  ~ beauty)
summary(modelo_ratings)

summary(modelo_ratings)$coefficients[2,4]

#5.3

collegeDistance <- read_dta("RegresionLineal/Exercises/Data/CollegeDistance.dta")

modelo_collDist <- lm(data = collegeDistance, formula = ed  ~ dist)
summary(modelo_collDist)

beta_1 <- unname(modelo_collDist$coefficients[2])
se_beta_1 <- summary(modelo_collDist)$coefficients[2,2]

limite_sup <- beta_1 + 1.96*se_beta_1
limite_inf <- beta_1 - 1.96*se_beta_1

limite_inf
limite_sup

confint(modelo_collDist)

library(haven)


cps08 <- read_dta("Intro/Data/cps08.dta")
str(cps08)
head(cps08)
modelo_cps08 <- lm(data = cps08, formula = ahe  ~ age)
modelo_cps08
summary(modelo_cps08)

ratings <- read_dta("Intro/Exercises/TeachingRatings.dta")
str(ratings)
head(ratings)
modelo_ratings <- lm(data = ratings, formula = course_eval  ~ beauty)
summary(modelo_ratings)

collegeDistance <- read_dta("Intro/Exercises/CollegeDistance.dta")
str(collegeDistance)
head(collegeDistance)
modelo_collDist <- lm(data = collegeDistance, formula = ed  ~ dist)
summary(modelo_collDist)

growth <- read_dta("Intro/Exercises/Growth.dta")
str(growth)
head(growth)
modelo_growth <- lm(data = growth, formula = growth ~ tradeshare)
summary(modelo_growth)
plot(growth$tradeshare, growth$growth)
modelo_growth <- lm(data = growth[-65,], formula = growth ~ tradeshare)
summary(modelo_growth)

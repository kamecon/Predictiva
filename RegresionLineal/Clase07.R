#Cargamos las librerias
pacman::p_load(haven, jtools, vtable, huxtable)

#Motivación: Regresion lineal multiple
caschool <- read_dta("RegresionLineal/Data/caschool.dta")

vtable(caschool)

modelo_cas01 <- lm(data = caschool, testscr ~ str)
summary(modelo_cas01)

#En lugar de usa summary, vamos a usar la funcion `summ` de la libreria `jtools`

summ(modelo_cas01)


summ(modelo_cas01, robust = "HC1")

#Siguiendo el ejemplo del libro, vamos a introducir una nueva variable, el porcentaje de estudiantes que son estudiantes de inglés (es decir, estudiantes para quienes el inglés es un segundo idioma)

modelo_cas03 <- lm(data = caschool, testscr ~ str + el_pct)
summ(modelo_cas03)

#Vamos a usar errores robustos a heterocedasticidad, para que los resultados sean comparables a los del libro

summ(modelo_cas03, robust = "HC1")

#Incluimos el intervalo de confianza en la salida de la estimacion

summ(modelo_cas03, robust = "HC1", confint = TRUE)

#Si se desea incluir mas decimales

summ(modelo_cas03, robust = "HC1", confint = TRUE, digits = 3)

#Otra manera de presentar el resumen de la estimacion
export_summs(modelo_cas03)

#Con errores robustos
export_summs(modelo_cas03, robust=TRUE)

#Este formato es util si deseamos comparar modelos
export_summs(modelo_cas01, modelo_cas03, robust=TRUE)

#Tambien podemos asignar nombres a las variables, asi se entiende mejor la estimacion al presentarla a un tercero
export_summs(modelo_cas01, modelo_cas03,
             robust=TRUE,
             coefs = c("Intercepto" = "(Intercept)",
                       "Alumnos por profesor" = "str",
                       "% de estudiantes de inglés" = "el_pct")
             )

#Y cambiar el nombre de los modelos
export_summs(modelo_cas01, modelo_cas03,
             robust=TRUE,
             coefs = c("Intercepto" = "(Intercept)",
                       "Alumnos por profesor" = "str",
                       "% de estudiantes de inglés" = "el_pct"),
             model.names = c("Modelo 1", "Modelo 2")
            )


#Contraste de hipotesis conjunta(robusta)

modelo_cas04 <- lm(data = caschool, testscr ~ str + el_pct + expn_stu)

export_summs(modelo_cas04,
             robust=TRUE,
             coefs = c("Intercepto" = "(Intercept)",
                       "Alumnos por profesor" = "str",
                       "% de estudiantes de inglés" = "el_pct",
                       "Gasto por alumno" = "expn_stu")
)

waldtest(modelo_cas01, modelo_cas03, vcov = vcovHC(modelo_cas03, type = "HC0"))

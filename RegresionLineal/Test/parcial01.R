pacman::p_load("openintro", "sandwich", "jtools", "huxtable")

# Inspeccionamos los datos 
str(loans_full_schema)
dim(loans_full_schema)
head(loans_full_schema)

# Miramos las estadisticas de la base de datos
summary(loans_full_schema)

#Estimar los modelos 


#Grafico de diagnostico
oldpar <- par( mfrow=c(2,2))
plot()
par(oldpar)

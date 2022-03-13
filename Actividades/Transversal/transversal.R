# Cargar librerias --------------------------------------------------------
if (! ('pacman' %in% installed.packages())) install.packages('pacman')
pacman::p_load(pwt10, tidyverse, remotes)
if (! ('maddison' %in% installed.packages())) remotes::install_github("expersso/maddison")
library(maddison)


# Analisis de convergencia ------------------------------------------------


## Manipulacion de datos ---------------------------------------------------

Maddisson_convergence <- maddison %>% 
  dplyr::filter(year %in% c(1870,2016)) %>% #Nos quedamos solo con los años de interes
  mutate(log_rgp_capita = log(rgdpnapc)) %>% #Calculamos el logaritmo del PIN per capita
  group_by(country) %>%
  dplyr::filter(!any(rgdpnapc == "NA")) %>% #Agrupamos por paises y nos quedamos con aquellos non NA en 1870
  ungroup() %>% 
  dplyr::filter(country %in% (maddison %>%
                                dplyr::filter(year==1870) %>%
                                pull(country) #vector con paises que tienen datos en 1870
  )
  ) #Hay paises que solo tienen datos para 2019, los eliminamos

## Grafico ---------------------------------------------------

#Nos quedamos solo con los datos necesarios para el grafico y la regresion
Maddison_graph <- Maddisson_convergence %>% 
  select(country,region, year,log_rgp_capita) %>% 
  pivot_wider(names_from = year, values_from=log_rgp_capita) %>% 
  mutate(crecimiento=`2016`-`1870`)

#Grafico  
Convergence_graph <- ggplot(Maddison_graph, aes(y= crecimiento, x=`1870`))+
  geom_point()+
  geom_text(aes(label=country),hjust=-0.2, vjust=0.2)+
  labs(x="Logaritmo de la renta per cápita en 1870",
       y="Logaritmo del crecimiento de la renta per cápita entre 1870-2016")

Convergence_graph

## Regresion lineal ---------------------------------------------------

convergencia01 <- lm(formula = crecimiento ~ `1870`, data = Maddison_graph)

summary(convergencia01)

## Regresiones por area ---------------------------------------------------

#Hacemos las regresiones por regiones 

#Funcion a utilizar en el map
modelo_region <- function(df){
  lm(formula = crecimiento ~ `1870`, data = df)
}

#Agrupamos por regiones, anidamos para poder estimar los modelos después y nos quedamos solo con aquellas que tengan mas de 5 paises
regiones <- Maddison_graph %>% 
  group_by(region) %>% 
  nest() %>% 
  mutate(size = map_int(data,nrow)) %>% 
  arrange(-size) %>% 
  filter(size>=5) 

#Con map estimamos el modelo por regiones
modelos_regiones01 <- regiones %>% 
  mutate(modelo=map(data, modelo_region))

#Con Broom y las funciones tidy y glance presentamos los resultados del modelo por columna "desanidando" los modelos
modelos_regiones02 <- modelos_regiones01 %>% 
  mutate(coeficientes=map(modelo, broom::tidy)) %>%
  mutate(glance = map(modelo, broom::glance)) %>% 
  unnest(coeficientes, .drop=TRUE) %>%
  unnest(glance, .drop=TRUE) %>% 
  select(region,nobs,term, estimate, std.error, statistic, p.value, r.squared, adj.r.squared, p.value1)


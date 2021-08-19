library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(sf) # Simple Features for R, CRAN v1.0-0
library(geofacet) # 'ggplot2' Faceting Utilities for Geographical Data
library(geoAr) # Argentina's Spatial Data Toolbox
library(gganimate) # "Package currently not installed"
library(gifski) # Highest Quality GIF Encoder
library(png) # Read and write PNG images
library(transformr) # Polygon and Path Transformations
options(scipen = 999) # Eliminar notación cientifica

# Cargo los datos exportados de los TPs anteriores:
#   - Terrenos, con sus variaciones porcentuales por barrio
#   - Cotización del dolar al día de la fecha


terrenos <- read.csv ("https://raw.githubusercontent.com/luisemiliotisocco/IAU2-TP1/master/data/terrenos.csv", encoding = "utf-8")
cotizacion <- read.csv ("https://raw.githubusercontent.com/luisemiliotisocco/IAU2-TP2/master/data/cotizacion.csv")

barrios <- st_read("data/barrios/barrios_badata.shp") %>% 
  select(BARRIO, COMUNA) #agrego la información geográfica de los barrios de CABA


#-----------------------------------------------------------
## REPASO

skimr::skim(terrenos) #una mirada rápida
 
ggplot()+
  geom_bar(data=terrenos, aes(y=reorder(BARRIO, -VARIACION_19_20), weight=VARIACION_19_20, fill="Variación 2019-2020"), color="black", alpha=.8)+
  geom_bar(data=terrenos %>% filter (rebote=="Positivo"), 
           aes(y=BARRIO, weight=VARIACION_18_19, fill="Variación 2018-2019 \nRebote positivo"), color="black", linetype="dashed", alpha=.7)+
  geom_bar(data=terrenos %>% filter (rebote=="Negativo"), 
           aes(y=BARRIO, weight=VARIACION_18_19, fill="Variación 2018-2019 \nRebote negativo"), color="black", linetype="dashed", alpha=.7)+
  labs(x="USD", 
       y="Barrio",
       title="Variación porcentual de precios de los terrenos en venta",
       subtitle="Período 2019-2020 + Efecto rebote en variación interanual",
       caption="Se omiten los casos que continuaron con la misma tendencia \nFuente: GCBA",
       fill="Referencia")+
  geom_vline (xintercept = 0, linetype="dashed", size=1)+
  scale_fill_manual(values = c("indianred", "darkseagreen", "brown4"))+
  theme_minimal()

# en el TP1 vimos cómo se comportó la variación del precio del m2 en USD en CABA en los últimos 2 años
# evidenciamos un comportamiento interanual que llamamos "EFECTO REBOTE", donde la variación porcentual parece invertirse 


skimr::skim(cotizacion) #una mirada rápida

# por otro lado, en el TP2 scrappeamos la cotización del dolar al día de la fecha
# convertimos luego todos los valores en numéricos, listos para ser utilizados en este ejercicio



#-----------------------------------------------------------
## INFO GEOGRÁFICA

#¿Existe una patrón territorial en la variación de los precios?


terrenos_barrios <- left_join(barrios, terrenos, by="BARRIO") #recuperamos la geometría


# A fines de este TP vamos a estudiar la variación en el pedíodo 2019-2020
ggplot()+
  geom_sf(data=terrenos_barrios, fill="gray", color="black")+
  geom_sf(data=terrenos_barrios, aes(fill=VARIACION_19_20), alpha=.75)+
  scale_fill_continuous(limits=c(-100, 100), breaks=c(-100, -75, -50, -25, 0, 25, 50, 75, 100))+
  geom_sf_text(data=terrenos_barrios, aes(label=round(VARIACION_19_20),0), size=4, fontface = "bold")+
  scale_fill_viridis_c(alpha=.75)+
  labs(title="Variación porcentual por Barrio período 2019-2020", 
       subtitle = "Período 2019-2020",
       caption="Fuente: GCBA")+
  theme_void()

# ¿Qué barrios crecieron en 2019-2020?
ggplot()+
  geom_sf(data=terrenos_barrios, fill="gray", color="black")+
  geom_sf(data=terrenos_barrios %>% filter(VARIACION_19_20>=0), fill="chartreuse3")+
  geom_sf(data=terrenos_barrios %>% filter(VARIACION_19_20<0), fill="brown1", color="black")+
  scale_fill_continuous(limits=c(-100, 100), breaks=c(-100, -75, -50, -25, 0, 25, 50, 75, 100))+
  geom_sf_text(data=terrenos_barrios, aes(label=BARRIO), size=1.5)+
  labs(title="¿Qué barrios crecieron en el último período?", 
       subtitle = "Período 2019-2020",
       caption="Fuente: GCBA")+
  theme_void()

# ¿Qué patrón espacial tiene el rebote?
ggplot()+
  geom_sf(data=terrenos_barrios %>% drop_na(), aes(fill=rebote), color="black")+
  scale_fill_viridis_d()+
  labs(title="¿Existe un patrón espacial?", 
       subtitle = "Período 2019-2020",
       caption="Fuente: GCBA")+
  facet_wrap(~rebote)+
  theme_void()
#a simple viste podemos ver que los barrios que comparten un comportamiento similar tienden a estan agrupados


#Veámoslo por comuna
(CABA <- geoAr::get_grid(district = "CABA") %>% 
    mutate(code=substring(code, 1)) %>% # %>% #elimino los primeros caracteres, para quedarme sólo con los numéricos 
    mutate(code=as.numeric(code)))
geofacet::grid_preview(CABA)

terrenos_grilla <- terrenos_barrios %>%
  as.data.frame() %>% 
  select(-geometry) %>% 
  mutate(code=COMUNA)

ggplot(terrenos_grilla) +
  geom_col(aes(x = VARIACION_19_20, y = rebote, fill=rebote), show.legend = FALSE) +
  theme_minimal() +
  scale_fill_viridis_d()+
  labs(title="¿Cómo fue el comportamiento comunal?", 
       subtitle = "Período 2019-2020",
       caption="Fuente: GCBA",
       x="", 
       y="")+
  facet_geo(~code, grid = CABA)

# Cuando agrupamos los barrios por comuna, vemos que el comportamiento suele tener lógica espacialmente
# La comuna 1 pareciera ser la que más se benefició, sin registrar caídas
# El caso de la comuna 15 es llamativo, ya que registra los 3 comportamiento en un grado considerable


#Preparameos los datos para visualizar el cambio anual de otra manera

terrenos_barrios2018 <- terrenos_barrios %>% 
  as.data.frame() %>% 
  select(BARRIO, PROMEDIOUSD_2018) %>% 
  mutate(AÑO=2018) %>% 
  rename(PROMEDIOUSD=PROMEDIOUSD_2018)

terrenos_barrios2019 <- terrenos_barrios %>% 
  as.data.frame() %>% 
  select(BARRIO, PROMEDIOUSD_2019) %>% 
  mutate(AÑO=2019) %>% 
  rename(PROMEDIOUSD=PROMEDIOUSD_2019)

terrenos_barrios2020 <- terrenos_barrios %>% 
  as.data.frame() %>% 
  select(BARRIO, PROMEDIOUSD_2020) %>% 
  mutate(AÑO=2020) %>% 
  rename(PROMEDIOUSD=PROMEDIOUSD_2020)

terrenos_anual <- full_join(terrenos_barrios2018, terrenos_barrios2019) %>% 
  full_join(terrenos_barrios2020) %>% 
  left_join(barrios, by="BARRIO")

#ahora sí conseguimos una matriz que nos permitirá animar la información para verla con mayor claridad 

p <- ggplot()+
  geom_col(data=terrenos_anual, aes(x= PROMEDIOUSD, y = BARRIO, fill=PROMEDIOUSD), show.legend = FALSE) +
  scale_fill_viridis_c()+
  theme_minimal()+
  labs(title = "Año: {closest_state}", 
       subtitle = "Variación del precio del m2 en USD durante el período 2018-2019-2020",
       caption="Fuente: GCBA",
       x="Precio de m2 en USD", 
       y="Barrio")+
  transition_states(AÑO, transition_length = 2, state_length = 1)+
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
print(p)  

# veámoslo en el territorio, para evidenciar si existe un patron territorial  
p2 <- ggplot(terrenos_anual)+
  geom_sf(aes(fill=PROMEDIOUSD, geometry = geometry), color="black")+
  scale_fill_viridis_c()+
  theme_void()+
  labs(title = "Año: {closest_state}", 
       subtitle = "Variación del precio del m2 en USD durante el período 2018-2019-2020",
       caption="Fuente: GCBA",
       x="Precio de m2 en USD", 
       y="Barrio")+
  transition_states(AÑO, transition_length = 2, state_length = 1)+
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
print(p2)



#¿Lo pesificamos al cambio del día?

USDBLUE <- cotizacion$venta[2] #nos quedamos sólo con el valor del dolar blue

terrenos_anual <- terrenos_anual %>% 
  mutate(PROMEDIOARS=PROMEDIOUSD*USDBLUE)

#recuperemos el gráficos animado de barras pero con valores en peso...
p3 <- ggplot()+
  geom_col(data=terrenos_anual, aes(x= PROMEDIOARS, y = BARRIO, fill=PROMEDIOARS), show.legend = FALSE) +
  scale_fill_viridis_c()+
  theme_minimal()+
  labs(title = "Año: {closest_state}", 
       subtitle = "Variación del precio del m2 en USD durante el período 2018-2019-2020",
       caption="Fuente: GCBA",
       x="Precio de m2 en ARS", 
       y="Barrio")+
  transition_states(AÑO, transition_length = 2, state_length = 1)+
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
print(p3) 






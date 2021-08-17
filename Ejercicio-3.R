library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(sf) # Simple Features for R, CRAN v1.0-0
library(vroom) # Read and Write Rectangular Text Data Quickly 
library(skimr) # Compact and Flexible Summaries of Data 
options(scipen = 999) # Eliminar notación cientifica


# Cargo los datos exportados de los TPs anteriores:
#   - Terrenos, con sus variaciones porcentuales por barrio
#   - Cotización del dolar al día de la fecha

terrenos <- vroom("https://github.com/luisemiliotisocco/IAU2-TP1/blob/master/data/terrenos.csv")
cotizacion <- vroom("https://github.com/luisemiliotisocco/IAU2-TP2/blob/master/data/cotizacion.csv")

barrios <- st_read("data/barrios/barrios_badata.shp") %>% 
  select(BARRIO, COMUNA) #agrego la información geográfica de los barrios de CABA


#-----------------------------------------------------------
## REPASO

skim(terrenos) #una mirada rápida
 
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


skim(cotizacion) #una mirada rápida

# por otro lado, en el TP2 scrappeamos la cotización del dolar al día de la fecha
# convertimos luego todos los valores en numéricos, listos para ser utilizados en este ejercicio



#-----------------------------------------------------------
## INFO GEOGRÁFICA

#¿Existe una patrón territorial en la variación de los precios?


terrenos_barrios <- left_join(barrios, terrenos, by="BARRIO") #recuperamos la geometría

# primero miraremos cómo fue el comportamiento en el último año
ggplot()+
  geom_sf(data=terrenos_barrios, aes(fill=VARIACION_19_20), alpha=.75)+
  scale_fill_continuous(limits=c(-100, 100), breaks=c(-100, -75, -50, -25, 0, 25, 50, 75, 100))+
  geom_sf_text(data=terrenos_barrios, aes(label=round(VARIACION_19_20),0), size=4, fontface = "bold")+
  scale_fill_viridis_c(alpha=.75)+
  labs(title="Variación porcentual por Barrio período 2019-2020", 
       subtitle = "Período 2019-2020",
       caption="Fuente: GCBA")+
  theme_void()

# ¿Qué barrios crecieron y cuáles cayeron en promedio?
ggplot()+
  geom_sf(data=terrenos_barrios %>% filter(VARIACION_19_20>=0), fill="chartreuse3")+
  geom_sf(data=terrenos_barrios %>% filter(VARIACION_19_20<0), fill="brown1")+
  scale_fill_continuous(limits=c(-100, 100), breaks=c(-100, -75, -50, -25, 0, 25, 50, 75, 100))+
  geom_sf_text(data=terrenos_barrios, aes(label=BARRIO), size=1.5)+
  labs(title="¿Qué barrios crecieron?", 
       subtitle = "Período 2019-2020",
       caption="Fuente: GCBA")+
  theme_void()


#ggplot(terrenos_barrios)+
#  geom_sf(aes(fill=rebote), color="black",)+
#  facet_wrap(~rebote, color="black")+
#  labs(title="¿Cómo fue el rebote?", 
#       caption="Fuente: GCBA")+
#  scale_fill_viridis_d()+
#  theme_void()







library(tidyverse)
library(sf)
library(readxl)

#abrimos datos de precipitación 
df<-read_xlsx("./data/precipitacion_2100_ssp126.xlsx")
str(df)

#abrimos datos de la grid
malla<-read_sf("./data/malla_hex_10km.gpkg")
#Checamos atributos geoespaciales 
st_crs(malla)
str(malla)

#Vamos a unir por id de hexagon 
malla_precip_mm_anual<-left_join(malla,df,by = "hex_id")

#Vamos a extraer columnas de los estados y datos relevantes
estados_mx<-unique(malla_precip_mm_anual$estado)
zona_hex<-unique(malla_precip_mm_anual$zona)

#Vamos a tomar los metadatos
meta<-read_xlsx("./data/precipitacion_2100_ssp126.xlsx",sheet = 2)

#Utilizamos un summary para los datos por estados para agregar a la pagina 
mean_precip_mm_anual<-malla_precip_mm_anual|>
  group_by(estado)|>
  summarise(promedio_mm=round(mean(precipitacion_mm_anio),digits = 2),
            sd=sd(precipitacion_mm_anio),
            max_mm=round(max(precipitacion_mm_anio),digits = 2),
            min_mm=round(min(precipitacion_mm_anio),digits = 2))|>
  st_drop_geometry()


#Guardamos para usar en la shiny 
save.image("./results/malla_precip_mm_anual.Rdata")

load("./results/malla_precip_mm_anual.Rdata")






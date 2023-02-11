#install.packages("rgdal")
library(rgdal)
library(sf)
library(tidyverse)

# fixo working directory
setwd("c:/Users/joana/Dropbox/upf/UPF22-23/MEA")

# descarguem dades de renta
df01 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30824.csv?nocab=1")

# altres matrius
#df02 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30825.csv?nocab=1")
#df03 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30826.csv?nocab=1")
#df04 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30827.csv?nocab=1")
#df05 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30828.csv?nocab=1")
#df06 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30829.csv?nocab=1")
#df07 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30830.csv?nocab=1")
#df08 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30831.csv?nocab=1")
#df09 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/37677.csv?nocab=1")
#df10 <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30832.csv?nocab=1")



# descripcio de cada matriu
taules<-c("Indicadores de renta media y mediana"
,"Distribución por fuente de ingresos"
,"Porcentaje de población con ingresos por unidad de consumo por debajo de determinados umbrales fijos por sexo"
,"Porcentaje de población con ingresos por unidad de consumo por debajo de determinados umbrales fijos por sexo y tramos de edad"
,"Porcentaje de población con ingresos por unidad de consumo por debajo de determinados umbrales fijos por sexo y nacionalidad"
,"Porcentaje de población con ingresos por unidad de consumo por debajo/encima de determinados umbrales relativos por sexo"
,"Porcentaje de población con ingresos por unidad de consumo por debajo/encima de determinados umbrales relativos por sexo y tramos de edad"
,"Porcentaje de población con ingresos por unidad de consumo por debajo/encima de determinados umbrales relativos por sexo y nacionalidad"
,"Índice de Gini y Distribución de la renta P80/P20"
,"Indicadores demográficos")

dim(df01)

#dim(df02)
#dim(df03)
#dim(df04)
#dim(df05)
#dim(df06)
#dim(df07)
#dim(df08)
#dim(df09)
#dim(df10)

head(df01)

# ordenem dades
df01_W <- df01 %>% 
  filter(sapply(id_sscc,nchar)>0, Periodo == 2020) %>%
  mutate(id_sscc = substr(df01$Secciones,1,10)
         , var_r = sapply(Indicadores.de.renta.media,function(x) gsub(" ","_",x))
         , Total_R = as.numeric(gsub(".", "", Total, fixed=TRUE))) %>% 
  select(id_sscc, Periodo, var_r, Total_R) %>% 
  pivot_wider(names_from=var_r,values_from=Total_R, values_fill = 0)

# verifiquem
colnames(df01_W)
head(df01_W[is.na(df01_W$Renta_neta_media_por_persona_),])
summary(df01_W)

# guadem dades
saveRDS(df01_W, file="data/ine/dadesRENTA_01.rds")
# A futur llegir dades
readRDS(df01_W, file="data/ine/dadesRENTA_01.rds")


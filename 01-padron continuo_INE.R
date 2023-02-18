# carga datos INE - PC-aXIS PAdrón
# https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&idp=1254734710990#!tabs-1254736195461
# https://www.ine.es/dynt3/inebase/es/index.htm?type=pcaxis&file=pcaxis&path=%2Ft20%2Fe245%2Fp07%2F%2Fa2021


library(dplyr)
library(tidyr)
library(pxR)
library(ggplot2)
library(maptools)


#install.packages("pxR")
#install.packages("maptools")

baixa_fitxers_padro <- function(any, carpeta){
  download.file(paste0("https://ine.es/pcaxisdl/t20/e245/p07/a",any,"/l0/0001.px"),destfile = paste0(carpeta,"/0001.px"))
  download.file(paste0("https://ine.es/pcaxisdl/t20/e245/p07/a",any,"/l0/0002.px"),destfile = paste0(carpeta,"/0002.px"))
  download.file(paste0("https://ine.es/pcaxisdl/t20/e245/p07/a",any,"/l0/0003.px"),destfile = paste0(carpeta,"/0003.px"))
  download.file(paste0("https://ine.es/pcaxisdl/t20/e245/p07/a",any,"/l0/0004.px"),destfile = paste0(carpeta,"/0004.px"))
  download.file(paste0("https://ine.es/pcaxisdl/t20/e245/p07/a",any,"/l0/0005.px"),destfile = paste0(carpeta,"/0005.px"))
  download.file(paste0("https://ine.es/pcaxisdl/t20/e245/p07/a",any,"/l0/0006.px"),destfile = paste0(carpeta,"/0006.px"))
  return(paste0(paste0(carpeta,"000",1:6,".px")))

}



# la caroeta ha d'existir abans,sense barra final
dir_fitxers<- baixa_fitxers_padro(2019,"data/ine/2019")
dir_fitxers<- baixa_fitxers_padro(2020,"data/ine/2020")
dir_fitxers<- baixa_fitxers_padro(2021,"data/ine/2021")

# data load 2019
px0001 <- as.data.frame(read.px("data/ine/2019/0001.px"))
px0002 <- as.data.frame(read.px("data/ine/2019/0002.px"))
px0003 <- as.data.frame(read.px("data/ine/2019/0003.px"))
px0004 <- as.data.frame(read.px("data/ine/2019/0004.px"))
px0005 <- as.data.frame(read.px("data/ine/2019/0005.px"))
px0006 <- as.data.frame(read.px("data/ine/2019/0006.px"))

# data load 2020
px0001 <- as.data.frame(read.px("data/ine/2020/0001.px"))
px0002 <- as.data.frame(read.px("data/ine/2020/0002.px"))
px0003 <- as.data.frame(read.px("data/ine/2020/0003.px"))
px0004 <- as.data.frame(read.px("data/ine/2020/0004.px"))
px0005 <- as.data.frame(read.px("data/ine/2020/0005.px"))
px0006 <- as.data.frame(read.px("data/ine/2020/0006.px"))

# data load 2021
px0001 <- as.data.frame(read.px("data/ine/2021/0001.px"))
px0002 <- as.data.frame(read.px("data/ine/2021/0002.px"))
px0003 <- as.data.frame(read.px("data/ine/2021/0003.px"))
px0004 <- as.data.frame(read.px("data/ine/2021/0004.px"))
px0005 <- as.data.frame(read.px("data/ine/2021/0005.px"))
px0006 <- as.data.frame(read.px("data/ine/2021/0006.px"))

# primera (dades i tercera zolumna sexo)
px0001W <- px0001 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")
px0002W <- px0002 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")
px0003W <- px0003 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")
px0004W <- px0004 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")
px0005W <- px0005 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")
px0006W <- px0006 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")

# unim taules
padro_tot <- px0001W
padro_tot$ANY <- 2019
padro_tot$ANY <- 2020
padro_tot$ANY <- 2021
padro_tot <- padro_tot[,c("sección","ANY",colnames(padro_tot)[!colnames(padro_tot)%in%c("sección","ANY")])]

padro_tot <- merge(padro_tot, px0002W[,c("sección",colnames(px0002W)[!colnames(px0002W)%in%colnames(padro_tot)])], by = "sección")
padro_tot <- merge(padro_tot, px0003W[,c("sección",colnames(px0003W)[!colnames(px0003W)%in%colnames(padro_tot)])], by = "sección")
padro_tot <- merge(padro_tot, px0004W[,c("sección",colnames(px0004W)[!colnames(px0004W)%in%colnames(padro_tot)])], by = "sección")
padro_tot <- merge(padro_tot, px0005W[,c("sección",colnames(px0005W)[!colnames(px0005W)%in%colnames(padro_tot)])], by = "sección")
padro_tot <- merge(padro_tot, px0006W[,c("sección",colnames(px0006W)[!colnames(px0006W)%in%colnames(padro_tot)])], by = "sección")

# prepara
padro_tot <- padro_tot[padro_tot$sección!="TOTAL",]
padro_tot$sección <- as.character(padro_tot$sección)
padro_tot$id_prov <- substr(padro_tot$sección,1,2)
padro_tot$id_mun <-  substr(padro_tot$sección,1,5)
padro_tot$id_dis <-  substr(padro_tot$sección,1,7)
padro_tot$id_sc <-   substr(padro_tot$sección,1,10)

saveRDS(padro_tot,file="data/ine/padron_tot_2019.rds")
saveRDS(padro_tot,file="data/ine/padron_tot_2020.rds")
saveRDS(padro_tot,file="data/ine/padron_tot_2021.rds")

## llegim
padro_tot <- readRDS(file="data/ine/padron_tot_2019.rds")
padro_tot <- readRDS(file="data/ine/padron_tot_2020.rds")
padro_tot <- readRDS(file="data/ine/padron_tot_2021.rds")


# padro municipi

padro_mun <- padro_tot %>% 
  select(id_mun,id_prov,ANY, starts_with("Ambos Sexos_"), starts_with("Mujeres_"), starts_with("Hombres_")) %>% 
  mutate(
    pob_15_64 = `Ambos Sexos_15-19`+`Ambos Sexos_20-24`+`Ambos Sexos_25-29`+`Ambos Sexos_30-34`+
      `Ambos Sexos_35-39`+`Ambos Sexos_40-44`+`Ambos Sexos_45-49`+`Ambos Sexos_50-54`+
      `Ambos Sexos_55-59`+`Ambos Sexos_60-64`,
    muj_15_64 = `Mujeres_15-19`+`Mujeres_20-24`+`Mujeres_25-29`+`Mujeres_30-34`+
      `Mujeres_35-39`+`Mujeres_40-44`+`Mujeres_45-49`+`Mujeres_50-54`+
      `Mujeres_55-59`+`Mujeres_60-64`,
    hom_15_64 = `Hombres_15-19`+`Hombres_20-24`+`Hombres_25-29`+`Hombres_30-34`+
      `Hombres_35-39`+`Hombres_40-44`+`Hombres_45-49`+`Hombres_50-54`+
      `Hombres_55-59`+`Hombres_60-64`,
    pob_15_24 = `Ambos Sexos_15-19`+`Ambos Sexos_20-24`,
    muj_15_24 = `Mujeres_15-19`+`Mujeres_20-24`,
    hom_15_24 = `Hombres_15-19`+`Hombres_20-24`,
    pob_25_44 = `Ambos Sexos_25-29`+`Ambos Sexos_30-34`+`Ambos Sexos_35-39`+`Ambos Sexos_40-44`,
    muj_25_44 = `Mujeres_25-29`+`Mujeres_30-34`+`Mujeres_35-39`+`Mujeres_40-44`,
    hom_25_44 = `Hombres_25-29`+`Hombres_30-34`+`Hombres_35-39`+`Hombres_40-44`,
    pob_45_64 = `Ambos Sexos_45-49`+`Ambos Sexos_50-54`+`Ambos Sexos_55-59`+`Ambos Sexos_60-64`,
    muj_45_64 = `Mujeres_45-49`+`Mujeres_50-54`+`Mujeres_55-59`+`Mujeres_60-64`,
    hom_45_64 = `Hombres_45-49`+`Hombres_50-54`+`Hombres_55-59`+`Hombres_60-64`) %>% 
  group_by(id_mun,id_prov,ANY) %>% 
  summarise(pob_15_64 = sum(pob_15_64)
            ,muj_15_64 = sum(muj_15_64)
            ,hom_15_64 = sum(hom_15_64)
            ,pob_15_24 = sum(pob_15_24)
            ,muj_15_24 = sum(muj_15_24)
            ,hom_15_24 = sum(hom_15_24)
            ,pob_25_44 = sum(pob_25_44)
            ,muj_25_44 = sum(muj_25_44)
            ,hom_25_44 = sum(hom_25_44)
            ,pob_45_64 = sum(pob_45_64)
            ,muj_45_64 = sum(muj_45_64)
            ,hom_45_64 = sum(hom_45_64)) %>% 
  ungroup()

head(padro_mun)

saveRDS(padro_mun,file="data/ine/padro_mun_2019.rds")
saveRDS(padro_mun,file="data/ine/padro_mun_2020.rds")
saveRDS(padro_mun,file="data/ine/padro_mun_2021.rds")


# filtrem CAtalunya
padro_cat <- padro_tot %>% 
  filter(substr(sección,1,2)%in%c("08","17","25","43"))

# validacions
## dimensions
dim(px0001W)
dim(padro_tot)
## totals sumen
padro_tot[padro_tot$sección=="TOTAL","Ambos Sexos_Total"]
sum(padro_tot[padro_tot$sección!="TOTAL","Ambos Sexos_Total"])
## no hi han NAs
sum(is.na(padro_tot))


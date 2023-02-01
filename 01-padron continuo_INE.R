# carga datos INE - PC-aXIS PAdrón
# https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&idp=1254734710990#!tabs-1254736195461
# https://www.ine.es/dynt3/inebase/es/index.htm?type=pcaxis&file=pcaxis&path=%2Ft20%2Fe245%2Fp07%2F%2Fa2021


library(dplyr)
library(tidyr)
library(pxR)
library(ggplot2)
library(maptools)

setwd("C:/Users/joana/Dropbox/upf/UPF22-23/MEA/DadesEstadistiques")

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

# data load 2021
px0001 <- as.data.frame(read.px("data/ine/2019/0001.px"))
px0002 <- as.data.frame(read.px("data/ine/2019/0002.px"))
px0003 <- as.data.frame(read.px("data/ine/2019/0003.px"))
px0004 <- as.data.frame(read.px("data/ine/2019/0004.px"))
px0005 <- as.data.frame(read.px("data/ine/2019/0005.px"))
px0006 <- as.data.frame(read.px("data/ine/2019/0006.px"))

# primera (dades i tercera zolumna sexo)
px0001W <- px0001 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")
px0002W <- px0002 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")
px0003W <- px0003 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")
px0004W <- px0004 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")
px0005W <- px0005 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")
px0006W <- px0006 %>% pivot_wider(id_cols="sección",names_from=c(3,1),values_from="value")

# unim taules
padro_tot <- px0001W
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

### eleccions
M20191_SE<-read.csv2("data/electorals/M20191_ME-SE-DM-MU-CO/M20191-Columnes-SE.csv")
M20151_SE<-read.csv2("data/electorals/M20151_ME-SE-DM-MU-CO/M20151-Columnes-SE.csv")
A20211_SE<-read.csv2("data/electorals/A20211_ME-SE-DM-MU-CO/A20211-Columnes-SE.csv")
A20171_SE<-read.csv2("data/electorals/A20171_ME-SE-DM-MU-CO/A20171-Columnes-SE.csv")
G20192_SE<-read.csv2("data/electorals/G20192_ME-SE-DM-MU-CO/G20192-Columnes-SE.csv")
G20191_SE<-read.csv2("data/electorals/G20191_ME-SE-DM-MU-CO/G20191-Columnes-SE.csv")

arregla_eleccions <- function(df){
  df$id_prov <- sprintf("%02.0f",df$X.Codi.Província)
  df$id_mun  <- sprintf("%05.0f",df$Codi.Municipi)
  df$id_dis  <- sprintf("%07.0f",100*df$Codi.Municipi + df$Districte)
  df$id_sc  <- sprintf("%010.0f",100000*df$Codi.Municipi + 1000*df$Districte + df$Secció)
  
  return(df)
  
}

M20191_SE <- arregla_eleccions(M20191_SE)
M20151_SE <- arregla_eleccions(M20151_SE)
A20211_SE <- arregla_eleccions(A20211_SE)
A20171_SE <- arregla_eleccions(A20171_SE)
G20192_SE <- arregla_eleccions(G20192_SE)
G20191_SE <- arregla_eleccions(G20191_SE)

#  ajuntem
head(M20191_SE$id_sc)
tail(padro_cat$id_sc,20)
colnames(M20191_SE )[colnames(M20191_SE ) %in%colnames(padro_cat)]

M20191_SE_PAD <- merge(M20191_SE, padro_cat)

dim(M20191_SE_PAD)
dim(M20191_SE)
dim(padro_cat)

colnames(M20191_SE_PAD)
colnames(padro_cat)

# model abstencio

M20191_SE_PAD2 <- M20191_SE_PAD %>% mutate(PER_ABST = Abstenció / Cens
                                          ,COD_VEG=Nom.Vegueria
                                          ,COD_PROV=Nom.Província
                                          ,POR_Municipio=(`Ambos Sexos_Misma Comunidad Autónoma. Misma Provincia. Mismo Municipio`)/
                                            `Ambos Sexos_Total`
                                          ,POR_m14=(`Ambos Sexos_0-4`+
                                                      `Ambos Sexos_5-9`+
                                                      `Ambos Sexos_10-14`+
                                                      `Ambos Sexos_0-4`)/`Ambos Sexos_Total`
                                          ,POR_M80=(`Ambos Sexos_80-84`+
                                                      `Ambos Sexos_80-84`+
                                                      `Ambos Sexos_85-89`+
                                                      `Ambos Sexos_90-94`+
                                                      `Ambos Sexos_95-99`+
                                                      `Ambos Sexos_100 y más`
                                                      )/`Ambos Sexos_Total`
                                          )

model1 <- lm(PER_ABST ~ COD_VEG + POR_Municipio + POR_m14 + POR_M80, data = M20191_SE_PAD2)

summary(model1)

"Ambos Sexos_Total"
"Ambos Sexos_Nacidos en España"                                            
"Ambos Sexos_En la misma Comunidad Autónoma"                               
"Ambos Sexos_Misma Comunidad Autónoma. Misma Provincia. Mismo Municipio"   

table(M20191_SE_PAD$Nom.Vegueria)
Nom.Província
Hombres_Española

[20] "Ambos Sexos_80-84"                                                        
[21] "Ambos Sexos_85-89"                                                        
[22] "Ambos Sexos_90-94"                                                        
[23] "Ambos Sexos_95-99"                                                        
[24] "Ambos Sexos_100 y más"                                                    

[4] "Ambos Sexos_0-4"                                                          
[5] "Ambos Sexos_5-9"                                                          
[6] "Ambos Sexos_10-14"                                                        


# variables interessants
head(M20191_SE$id_dis)
head(padro_cat)
Nom.Província
Codi.Vegueria
Nom.Vegueria
Codi.Comarca
Nom.Comarca
Nom.Municipi
Cens
Participació.20.00 Abstenció Vots.nuls
Vots.en.blanc Vots.a.candidatures Vots.vàlids
PSC.CP.Vots ERC.AM.Vots Cs.Vots PP.Vots ERC
BARCELONA.EN.COMÚ.ECG.Vots
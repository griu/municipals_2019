
library(dplyr)
library(tidyr)
library(pxR)
library(ggplot2)
library(maptools)


### eleccions
M20191_SE<-read.csv2("data/electorals/M20191-Columnes-SE.csv")
#M20151_SE<-read.csv2("data/electorals/M20151-Columnes-SE.csv")
#A20211_SE<-read.csv2("data/electorals/A20211-Columnes-SE.csv")
#A20171_SE<-read.csv2("data/electorals/A20171-Columnes-SE.csv")
#G20192_SE<-read.csv2("data/electorals/G20192-Columnes-SE.csv")
#G20191_SE<-read.csv2("data/electorals/G20191-Columnes-SE.csv")

arregla_eleccions <- function(df){
  df$id_prov <- sprintf("%02.0f",df$X.Codi.Província)
  df$id_mun  <- sprintf("%05.0f",df$Codi.Municipi)
  df$id_dis  <- sprintf("%07.0f",100*df$Codi.Municipi + df$Districte)
  df$id_sc  <- sprintf("%010.0f",100000*df$Codi.Municipi + 1000*df$Districte + df$Secció)
  
  return(df)
  
}

M20191_SE <- arregla_eleccions(M20191_SE)
#M20151_SE <- arregla_eleccions(M20151_SE)
#A20211_SE <- arregla_eleccions(A20211_SE)
#A20171_SE <- arregla_eleccions(A20171_SE)
#G20192_SE <- arregla_eleccions(G20192_SE)
#G20191_SE <- arregla_eleccions(G20191_SE)

#  ajuntem
head(M20191_SE$id_sc)
tail(padro_cat$id_sc,20)
colnames(M20191_SE )[colnames(M20191_SE ) %in%colnames(padro_cat)]

# fusio padro

padro_tot <- readRDS(file="data/ine/padron_tot_2019.rds")

# filtrem CAtalunya
padro_cat <- padro_tot %>%
  mutate(id_prov = substr(sección,1,2)) %>% 
  filter(id_prov %in% c("08","17","25","43"))

# fusio
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
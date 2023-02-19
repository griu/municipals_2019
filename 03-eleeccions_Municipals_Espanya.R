
library(dplyr)
library(tidyr)
library(ggplot2)

# https://infoelectoral.interior.gob.es/opencms/es/elecciones-celebradas/area-de-descargas/

# congreso
# https://infoelectoral.interior.gob.es/estaticos/docxl/apliextr/02201911_MESA.zip

download.file("https://infoelectoral.interior.gob.es/estaticos/docxl/apliextr/04201905_MESA.zip"
              ,"data/electorals/04201905_MESA.zip")
unzip("data/electorals/04201905_MESA.zip",exdir="data/electorals/ESP_MUN_2019/")


df03_esp_mun_2019 <- read.fwf("data/electorals/ESP_MUN_2019/03041905.DAT"
         ,widths = c(2,4,2,6,50,150,6,6,6)
         ,stringsAsFactors=FALSE
         ,comment.char=""
         ,col.names = c("tipo_eleccion","anyo_proceso","mes_proceso","cod_candidatura"
                        ,"siglas_candidatura","denom_candidatura","cod_candidatura_prov"
                        ,"cod_candidatura_autonom","cod_candidatura_nacional")
         ,colClasses = c("numeric","numeric","numeric","numeric","character","character","numeric","numeric","numeric")
         ,fileEncoding="Latin1"
)

desc_09 <- read.table("data/electorals/09_desc.txt", header=TRUE, sep="\t")
desc_10 <- read.table("data/electorals/10_desc.txt", header=TRUE, sep="\t")
desc_11 <- read.table("data/electorals/11_desc.txt", header=TRUE, sep="\t")
desc_12 <- read.table("data/electorals/12_desc.txt", header=TRUE, sep="\t")

df09_esp_mun_2019 <- read.fwf("data/electorals/ESP_MUN_2019/09041905.DAT"
                                  ,widths = desc_09$width
                                  ,stringsAsFactors=FALSE
                                  ,comment.char=""
                                  ,col.names = desc_09$nom
 #                                 ,colClasses = desc_09$tipo
                                  ,fileEncoding="Latin1")

df09_esp_mun_2019$id_sscc <- sprintf("%010.0f",df09_esp_mun_2019$id_sscc)

df10_esp_mun_2019 <- read.fwf("data/electorals/ESP_MUN_2019/10041905.DAT"
                                 ,widths = desc_10$width
                                 ,stringsAsFactors=FALSE
                                 ,comment.char=""
                                 ,col.names = desc_10$nom
#                                 ,colClasses = desc_10$tipo
                                 ,fileEncoding="Latin1")

df10_esp_mun_2019$id_sscc <- sprintf("%010.0f",df10_esp_mun_2019$id_sscc)

df11_esp_mun_2019 <- read.fwf("data/electorals/ESP_MUN_2019/11041905.DAT"
                              ,widths = desc_11$width
                              ,stringsAsFactors=FALSE
                              ,comment.char=""
                              ,col.names = desc_11$nom
#                              ,colClasses = desc_11$tipo
                              ,fileEncoding="Latin1")
df11_esp_mun_2019$id_mun <- sprintf("%05.0f",df11_esp_mun_2019$id_mun)


df12_esp_mun_2019 <- read.fwf("data/electorals/ESP_MUN_2019/12041905.DAT"
                              ,widths = desc_12$width
                              ,stringsAsFactors=FALSE
                              ,comment.char=""
                              ,col.names = desc_12$nom
                              ,colClasses = desc_12$tipo
                              ,fileEncoding="Latin1")

df12_esp_mun_2019$id_mun <- sprintf("%05.0f",df12_esp_mun_2019$id_mun)

saveRDS(df03_esp_mun_2019,file="data/electorals/df03_esp_mun_2019.rds")  # dades candidatures
saveRDS(df09_esp_mun_2019,file="data/electorals/df09_esp_mun_2019.rds")  # resultats nivell municipis grans
saveRDS(df10_esp_mun_2019,file="data/electorals/df10_esp_mun_2019.rds")  # resultats candidatures nivell muncipis grans
saveRDS(df11_esp_mun_2019,file="data/electorals/df11_esp_mun_2019.rds")  # resultats nivell municipis petits
saveRDS(df12_esp_mun_2019,file="data/electorals/df12_esp_mun_2019.rds")  # resultats candidatures nivell municipis petits

#   llegir
df03_esp_mun_2019 <- readRDS(file="data/electorals/df03_esp_mun_2019.rds")  # dades candidatures
df09_esp_mun_2019 <- readRDS(file="data/electorals/df09_esp_mun_2019.rds")  # resultats nivell municipis grans
df10_esp_mun_2019 <- readRDS(file="data/electorals/df10_esp_mun_2019.rds")  # resultats candidatures nivell muncipis grans
df11_esp_mun_2019 <- readRDS(file="data/electorals/df11_esp_mun_2019.rds")  # resultats nivell municipis petits
df12_esp_mun_2019 <- readRDS(file="data/electorals/df12_esp_mun_2019.rds")  # resultats candidatures nivell municipis petits



summary(df03_esp_mun_2019)
summary(df09_esp_mun_2019)
summary(df10_esp_mun_2019)
summary(df11_esp_mun_2019)
summary(df12_esp_mun_2019)


### juntar muncipios grandes y pequenyos

df09_esp_mun_2019_temp <- df09_esp_mun_2019 %>% 
  mutate( tipo_municipio = 1) %>%   # más de 250 hab. 
  select(tipo_eleccion,anyo_proceso,mes_proceso,num_vuelta,id_ca,id_sscc,control_sscc,id_mesa
         ,n_censo_ine,n_escrutinio_censo,n_escrutinio_censo_extr,n_votantes_censo_extr
         ,n_partici_primer_av,n_partici_segund_av,n_vot_blanco,n_vot_nulos,n_vot_candidaturas,ind_dat_oficiales,tipo_municipio)


df11_esp_mun_2019_temp <- df11_esp_mun_2019 %>% 
  mutate(tipo_eleccion = 4  # municipales
         , id_sscc = paste0(id_mun,"01001")  # suposem 1 seccio y disticte
         , id_mesa = "A"   #   inventamos una mesa
         , control_sscc=" ") %>% 
  select(tipo_eleccion,anyo_proceso,mes_proceso,num_vuelta,id_ca,id_sscc,control_sscc,id_mesa
         ,n_censo_ine,n_escrutinio_censo,n_escrutinio_censo_extr,n_votantes_censo_extr
         ,n_partici_primer_av,n_partici_segund_av,n_vot_blanco,n_vot_nulos,n_vot_candidaturas,ind_dat_oficiales,tipo_municipio)

d0911_esp_mun_2019 <- rbind(df09_esp_mun_2019_temp,df11_esp_mun_2019_temp)

table(d0911_esp_mun_2019$id_mesa)

head(sort(unique(d0911_esp_mun_2019$id_sscc)))
tail(sort(unique(d0911_esp_mun_2019$id_sscc)))
length(sort(unique(d0911_esp_mun_2019$id_sscc)))

colnames(df10_esp_mun_2019)
colnames(df12_esp_mun_2019)

### juntar muncipios grandes y pequenyos

df10_esp_mun_2019_temp <- df10_esp_mun_2019 %>% 
  mutate( tipo_municipio = 4) %>%   # municipio ma de 250 hab.
  select(tipo_eleccion,anyo_proceso,mes_proceso,num_vuelta,id_sscc,control_sscc,id_mesa
         ,cod_candidatura,n_vot_candidatura)

df12_esp_mun_2019_temp <- df12_esp_mun_2019 %>% 
  mutate(tipo_eleccion = 4   # municipales
         , id_sscc = paste0(id_mun,"01001")  # suposem 1 seccio y disticte
         , id_mesa = "A"   #   inventamos una mesa
         , control_sscc=" ") %>% 
  group_by(tipo_eleccion,anyo_proceso,mes_proceso,num_vuelta,id_sscc,control_sscc,id_mesa
           ,cod_candidatura) %>% 
  summarise(n_vot_candidatura = sum(n_vot_candidatura, na.rm=TRUE)) %>%
  ungroup() %>% 
  select(tipo_eleccion,anyo_proceso,mes_proceso,num_vuelta,id_sscc,control_sscc,id_mesa
         ,cod_candidatura,n_vot_candidatura)

d1012_esp_mun_2019 <- rbind(df10_esp_mun_2019_temp,df12_esp_mun_2019_temp)

d1012_esp_mun_2019$cod_candidatura_nacional <- df03_esp_mun_2019$cod_candidatura_nacional[match(d1012_esp_mun_2019$cod_candidatura, df03_esp_mun_2019$cod_candidatura)]


### ahora hay que montar el fichero por mesa y columnas las candidaturas nacionales principales

cand_nac<-df03_esp_mun_2019[df03_esp_mun_2019$cod_candidatura %in% df03_esp_mun_2019$cod_candidatura_nacional,]
dim(cand_nac)
head(cand_nac)
resum<-cbind.data.frame(cod_candidatura=cand_nac$cod_candidatura,siglas_candidatura=substr(cand_nac$siglas_candidatura,1,15),denom_candidatura = substr(cand_nac$denom_candidatura,1,30))
resum[grepl("ERC",toupper(resum$siglas_candidatura )),]
resum[grepl("REPUBLICANA",toupper(resum$denom_candidatura )),] # 6649    
resum[grepl("JUNTS",toupper(resum$denom_candidatura )),]
resum[grepl("CUP",toupper(resum$denom_candidatura )),]

d1012_esp_mun_2019[substr(d1012_esp_mun_2019$id_sscc,1,5) =="08113",] %>% 
#  group_by(cod_candidatura_nacional) %>% 
  group_by(cod_candidatura) %>% 
  summarise(n_vot_candidatura = sum(n_vot_candidatura)) %>% 
  ungroup() %>% 
  arrange(-n_vot_candidatura)

cod_candidatura_nacional n_vot_candidatura
<dbl>             <dbl>
  1                     6649              9177
2                     1464              9119
3                     1287              5036
4                     1458              3275
5                     1532              2389
6                     8015              1083
7                     1522               979
8                     8016               792
9                     2334               583
10                     5779               480


cod_candidatura n_vot_candidatura
<dbl>             <dbl>
  1            6649              9119
2            4306              9109
3            4254              4743
4            3606              3269
5            2227              2282
6            3879               988
7            4237               840
8            4025               792
9            6699               583
10            5447               429


df03_esp_mun_2019[grepl("1458",toupper(df03_esp_mun_2019$cod_candidatura )),]

df03_esp_mun_2019[grepl("CUP",toupper(df03_esp_mun_2019$denom_candidatura )),c("cod_candidatura_nacional", "siglas_candidatura")]

dim(df10_esp_mun_2019)
df10_esp_mun_2019_cand<-merge(df03_esp_mun_2019 %>% select(cod_candidatura,siglas_candidatura,denom_candidatura,cod_candidatura_nacional)
      ,df10_esp_mun_2019)
dim(df10_esp_mun_2019_cand)


df10_esp_mun_2019_cand %>% 
  group_by(cod_candidatura_nacional) %>% 
  mutate(n_vist_cand_nac = sum(n_vot_candidatura)) %>% 
  ungroup() %>% 
  arrange(-n_vist_cand_nac) %>% 
  head(50)

top_vots_nac<-df10_esp_mun_2019_cand %>% 
  group_by(cod_candidatura_nacional) %>% 
  summarise(n_vist_cand_nac = sum(n_vot_candidatura)) %>% 
  ungroup() %>% 
  arrange(-n_vist_cand_nac)

aux <- df03_esp_mun_2019[match(top_vots_nac$cod_candidatura_nacional,df03_esp_mun_2019$cod_candidatura),] %>%  
  select(cod_candidatura,siglas_candidatura,denom_candidatura,cod_candidatura_nacional)

top_vots_nac$siglas_cand_nac <- aux$siglas_candidatura
top_vots_nac$denom_cand_nac <- aux$denom_candidatura
top_vots_nac$siglas_cand_nac <- aux$siglas_candidatura

head(data.frame(top_vots_nac),100)
PSOE <- c(1287)
PP   <- c(1522)
Cs   <- c(1532,1456)
ERC   <- c(6649)
VOX   <- c(5779,4621)
JUNTS   <- c(1464)
PNV   <- c(1245)
PODEMOS_IU   <- c(8014,2334,1185,4437,1415,4960,1428,8012,5141,5914,3384)
EH_Bildu   <- c(5380)
BNG <- c(2364)
CUP <- c(1458,7185)


table(d1012_esp_mun_2019$control_sscc  )

head(d1012_esp_mun_2019)

detecta_partit <- function(codis, label, total, vots){
  aux_1 <- vots %>% 
    filter(cod_candidatura_nacional %in% codis) %>% 
    group_by(id_sscc, id_mesa) %>% 
    summarise(n_vot_candidatura = sum(n_vot_candidatura)) %>% 
    ungroup()
  total[[label]] <- aux_1$n_vot_candidatura[
    match(paste0(total$id_sscc,total$id_mesa,sep="-")
          , paste0(aux_1$id_sscc,aux_1$id_mesa,sep="-"))]
  total[[label]][is.na(total[[label]])] <- 0
  return(total)
}


d0911_esp_mun_2019 <- detecta_partit(codis = PSOE, label = "PSOE", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)
d0911_esp_mun_2019 <- detecta_partit(codis = PP, label = "PP", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)
d0911_esp_mun_2019 <- detecta_partit(codis = Cs, label = "Cs", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)
d0911_esp_mun_2019 <- detecta_partit(codis = ERC, label = "ERC", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)
d0911_esp_mun_2019 <- detecta_partit(codis = VOX, label = "VOX", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)
d0911_esp_mun_2019 <- detecta_partit(codis = JUNTS, label = "JUNTS", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)
d0911_esp_mun_2019 <- detecta_partit(codis = PNV, label = "PNV", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)
d0911_esp_mun_2019 <- detecta_partit(codis = PODEMOS_IU, label = "PODEMOS_IU", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)
d0911_esp_mun_2019 <- detecta_partit(codis = EH_Bildu, label = "EH_Bildu", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)
d0911_esp_mun_2019 <- detecta_partit(codis = BNG, label = "BNG", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)
d0911_esp_mun_2019 <- detecta_partit(codis = CUP, label = "CUP", total = d0911_esp_mun_2019, vots = d1012_esp_mun_2019)

head(d0911_esp_mun_2019)

summary(d0911_esp_mun_2019)

dim(d0911_esp_mun_2019)

colnames(d0911_esp_mun_2019)

d0911_esp_mun_2019_sscc <- d0911_esp_mun_2019 %>% 
  group_by(tipo_eleccion,anyo_proceso,mes_proceso,id_ca,id_sscc) %>% 
  summarise(n_censo_ine = sum(n_censo_ine)
            ,n_escrutinio_censo = sum(n_escrutinio_censo)
            ,n_escrutinio_censo_extr = sum(n_escrutinio_censo_extr)
            ,n_votantes_censo_extr = sum(n_votantes_censo_extr)
            ,n_partici_primer_av = sum(n_partici_primer_av)
            ,n_partici_segund_av = sum(n_partici_segund_av)
            ,n_vot_blanco = sum(n_vot_blanco)
            ,n_vot_nulos = sum(n_vot_nulos)
            ,tipo_municipio = max(tipo_municipio)
            ,PSOE = sum(PSOE)
            ,PP = sum(PP)
            ,Cs = sum(Cs)
            ,ERC = sum(ERC)
            ,VOX = sum(VOX)
            ,JUNTS = sum(JUNTS)
            ,PNV = sum(PNV)
            ,PODEMOS_IU = sum(PODEMOS_IU)
            ,EH_Bildu = sum(EH_Bildu)
            ,BNG = sum(BNG)
            ,CUP = sum(CUP)
  ) %>% 
  ungroup()


d0911_esp_mun_2019_mun <- d0911_esp_mun_2019 %>% 
  mutate(id_mun = substr(id_sscc,1,5)) %>% 
  group_by(tipo_eleccion,anyo_proceso,mes_proceso,id_ca,id_sscc) %>% 
  summarise(n_censo_ine = sum(n_censo_ine)
            ,n_escrutinio_censo = sum(n_escrutinio_censo)
            ,n_escrutinio_censo_extr = sum(n_escrutinio_censo_extr)
            ,n_votantes_censo_extr = sum(n_votantes_censo_extr)
            ,n_partici_primer_av = sum(n_partici_primer_av)
            ,n_partici_segund_av = sum(n_partici_segund_av)
            ,n_vot_blanco = sum(n_vot_blanco)
            ,n_vot_nulos = sum(n_vot_nulos)
            ,tipo_municipio = max(tipo_municipio)
            ,PSOE = sum(PSOE)
            ,PP = sum(PP)
            ,Cs = sum(Cs)
            ,ERC = sum(ERC)
            ,VOX = sum(VOX)
            ,JUNTS = sum(JUNTS)
            ,PNV = sum(PNV)
            ,PODEMOS_IU = sum(PODEMOS_IU)
            ,EH_Bildu = sum(EH_Bildu)
            ,BNG = sum(BNG)
            ,CUP = sum(CUP)
  ) %>% 
  ungroup()

saveRDS(d0911_esp_mun_2019,file="data/electorals/d0911_esp_mun_2019.RData")
saveRDS(d0911_esp_mun_2019_sscc,file="data/electorals/d0911_esp_mun_2019_sscc.RData")
saveRDS(d0911_esp_mun_2019_mun,file="data/electorals/d0911_esp_mun_2019_mun.RData")

d0911_esp_mun_2019 <- readRDS(file="data/electorals/d0911_esp_mun_2019.RData")
d0911_esp_mun_2019_sscc <- readRDS(file="data/electorals/d0911_esp_mun_2019_sscc.RData")
d0911_esp_mun_2019_mun <- readRDS(file="data/electorals/d0911_esp_mun_2019_mun.RData")

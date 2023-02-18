#install.packages("rgdal")
#library(rgdal)
library(sf)
library(tidyverse)
library(readxl)


# ja sha executat per tant no es necessari tornar.ho a fer.
if (1==2) {
  # fixo working directory
  setwd("c:/Users/joana/Dropbox/upf/UPF22-23/MEA")
  
  # origen dades SEPE: https://www.sepe.es/HomeSepe/que-es-el-sepe/estadisticas/datos-estadisticos/municipios/2022/diciembre.html
  
  # descarguem dades de paro
  any_sepe <- c(2019,2020,2021,2022) 
  df_sepe <- data.frame()
  for (k in any_sepe) {
    for (j in 1:12){
      for (i in seq(3,105,2)) {
        df_tmp <- read_xls(paste0("data/SEPE/",k,"/ESTADISTICA_MUNICIPIOS (",j,").xls"),sheet=i,skip=8, col_names = FALSE, col_types = "text")
        df_tmp$ANY = k
        df_tmp$ANYMES = k * 100 + j
        df_sepe<-rbind(df_sepe,df_tmp)
      }
    }
  }

  noms_atur<- c("id_mun","nom_mun","n_atur","n_atur_h_m24","n_atur_h_25_44"
                ,"n_atur_h_M45","n_atur_d_m24","n_atur_d_25_44","n_atur_d_M45"
                ,"n_atur_agri","n_atur_indus","n_atur_constr","n_atur_servi","n_atur_no_empl","ANY","ANYMES")
  
  colnames(df_sepe) <- noms_atur
  # saveRDS(df_sepe,file="data/SEPE/df_sepe.RDS")

  df_sepe <- readRDS(file="data/SEPE/df_sepe.RDS")
  
  head(df_sepe)
  dim(df_sepe)
  head(df_sepe)
  
  
  dim(df_sepe)
  
  length(unique(df_sepe$id_mun[df_sepe$ANY=="2019"]))
  length(unique(df_sepe$id_mun[df_sepe$ANY=="2020"]))
  length(unique(df_sepe$id_mun[df_sepe$ANY=="2021"]))
  length(unique(df_sepe$id_mun[df_sepe$ANY=="2022"]))
  table(df_sepe$ANY)
  
  df_sepe <- df_sepe[!is.na(df_sepe$id_mun),]

  df_sepe_hist_2019 <- as.data.frame(sapply(df_sepe[df_sepe$ANY == 2019, c(1,3:16)],as.numeric))
  df_sepe_hist_2020 <- as.data.frame(sapply(df_sepe[df_sepe$ANY == 2020, c(1,3:16)],as.numeric))
  df_sepe_hist_2021 <- as.data.frame(sapply(df_sepe[df_sepe$ANY == 2021, c(1,3:16)],as.numeric))

  df_sepe_hist <- rbind(df_sepe_hist_2019, df_sepe_hist_2020, df_sepe_hist_2021)
  
  df_sepe_hist$id_mun <- sprintf("%05.0f",as.numeric(df_sepe_hist$id_mun))
  
  df_sepe_hist[is.na(df_sepe_hist)]<-0
  
  
  # mitjana anual
  df_sepe_hist_m <- df_sepe_hist %>% 
    group_by(id_mun,ANY) %>% 
    summarize(n_atur=mean(n_atur)
              ,n_atur_h_m24=mean(n_atur_h_m24)
              ,n_atur_h_25_44=mean(n_atur_h_25_44)
              ,n_atur_h_M45=mean(n_atur_h_M45)
              ,n_atur_d_m24=mean(n_atur_d_m24)
              ,n_atur_d_25_44=mean(n_atur_d_25_44)
              ,n_atur_d_M45=mean(n_atur_d_M45)
              ,n_atur_agri=mean(n_atur_agri)
              ,n_atur_indus=mean(n_atur_indus)
              ,n_atur_constr=mean(n_atur_constr)
              ,n_atur_servi=mean(n_atur_servi)
              ,n_atur_no_empl=mean(n_atur_no_empl)
    ) %>% 
    ungroup() %>% 
    mutate(n_atur_m24 = n_atur_h_m24 + n_atur_d_m24
           ,n_atur_25_44 = n_atur_h_25_44 + n_atur_d_25_44
           ,n_atur_M45=n_atur_h_M45 + n_atur_d_M45)
  
  saveRDS(df_sepe_hist_m,file="data/SEPE/df_sepe_hist_m.RDS")

}

df_sepe_hist_m <- readRDS(file="data/SEPE/df_sepe_hist_m.RDS")

head(df_sepe_hist_m)
summary(df_sepe_hist_m)

table(df_sepe_hist_m$ANY)

# fusionem amb padro
padro_mun_2019 <- readRDS(file="data/ine/padro_mun_2019.rds")
padro_mun_2020 <- readRDS(file="data/ine/padro_mun_2020.rds")
padro_mun_2021 <- readRDS(file="data/ine/padro_mun_2021.rds")

padro_mun_hist <- rbind(padro_mun_2019,padro_mun_2020,padro_mun_2021)

(noms_vars <- colnames(padro_mun_hist))
(noms_vars <- noms_vars[-which(noms_vars %in% c("id_mun","id_prov","ANY"))])

df_sepe_hist_m_padro <- merge(df_sepe_hist_m,padro_mun_hist)

dim(df_sepe_hist_m_padro)
table(df_sepe_hist_m_padro$ANY)


df_sepe_hist_m_padro <- df_sepe_hist_m_padro %>% mutate(
  n_atur_h = n_atur_h_m24 + n_atur_h_25_44 + n_atur_h_M45
  ,n_atur_d = n_atur_d_m24 + n_atur_d_25_44 + n_atur_d_M45
  ,t_atur         = ifelse(pob_15_64 == 0,0,n_atur          /  pob_15_64)
  ,t_atur_m24     = ifelse(pob_15_24 == 0,0,n_atur_m24      /  pob_15_24)
  ,t_atur_25_44   = ifelse(pob_25_44 == 0,0,n_atur_25_44    /  pob_25_44)
  ,t_atur_M45     = ifelse(pob_45_64 == 0,0,n_atur_M45      /  pob_45_64)
  ,t_atur_h       = ifelse(hom_15_64 == 0,0,n_atur_h        /  hom_15_64)
  ,t_atur_h_m24   = ifelse(hom_15_24 == 0,0,n_atur_h_m24    /  hom_15_24)
  ,t_atur_h_25_44 = ifelse(hom_25_44 == 0,0,n_atur_h_25_44  /  hom_25_44)
  ,t_atur_h_M45   = ifelse(hom_45_64 == 0,0,n_atur_h_M45    /  hom_45_64)
  ,t_atur_d       = ifelse(muj_15_64 == 0,0,n_atur_d        /  muj_15_64)
  ,t_atur_d_m24   = ifelse(muj_15_24 == 0,0,n_atur_d_m24    /  muj_15_24)
  ,t_atur_d_25_44 = ifelse(muj_25_44 == 0,0,n_atur_d_25_44  /  muj_25_44)
  ,t_atur_d_M45   = ifelse(muj_45_64 == 0,0,n_atur_d_M45    /  muj_45_64)
)

saveRDS(df_sepe_hist_m_padro,file="data/SEPE/df_sepe_hist_m_padro.RDS")

head(df_sepe_hist_m_padro)

#install.packages("rgdal")
library(rgdal)
library(sf)
library(tidyverse)

# fixo working directory
setwd("c:/Users/joana/Dropbox/upf/UPF22-23/MEA")

# descargar mapes
download.file("https://www.ine.es/prodyser/cartografia/seccionado_2020.zip", destfile = "seccionado_2020.zip")
# descomprimir
unzip("seccionado_2020.zip",exdir="secc2020INE")  # unzip your file 
# llegir mapes
sscc1 <- read_sf("secc2020INE/SECC_CE_20200101.shp")

# guardem
saveRDS(sscc1, file="data/ine/seccionado_2020.rds")

# Alternativa a lo anterior: descarregar des de des de Dropbox
# https://www.dropbox.com/s/ehal0n8fcph2fmh/seccionado_2020.rds?dl=0

# Per llegir a futur llegim
sscc1<- readRDS(file="data/ine/seccionado_2020.rds")

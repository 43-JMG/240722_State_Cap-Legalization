library(readr)
library(dplyr)
library(tidyverse)
source("sep_est.R")

codigos_estados <- data.frame(
  estado = as.numeric(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
             "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
             "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", 
             "31", "32")),
  nombre_estado = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", 
                    "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", 
                    "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", 
                    "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", 
                    "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", 
                    "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))
file_names <- paste0("datos/spre_hu", 1:6, ".csv")
data_frames <- list()

for (i in seq_along(file_names)) {
  data_frames[[i]] <- read_csv(file_names[i]) %>%
    mutate(ubic_geo = as.numeric(ubic_geo))
}

sp15<-as.data.frame(bind_rows(data_frames))|>
  rowwise() |>
  mutate(separados = list(separar_estado_municipio(ubic_geo))) |>
  mutate(estado = as.numeric(separados$estado),
         municipio = separados$municipio) |>
  select(-separados) |> 
  ungroup() |>
  group_by(tipo_cor,estado,municipio)|>
  summarise(persomun=sum(tt_perso,na.rm=T))|>
  unique()|>
  left_join(codigos_estados)|>
  mutate(año=2015)

sp17<-read_csv("datos/segurpub17.csv")|>
  select(-2,-3,-20)|>
  pivot_longer(cols=c(2:17),
               names_to="tipo_cor",
               values_to="persomun")|>
  group_by(ubic_geo,tipo_cor)|>
  mutate(persomun=as.numeric(persomun))|>
  summarise(persomun=sum(persomun,na.rm=T))|>
  unique()|>
  rowwise() |>
  mutate(separados = list(separar_estado_municipio(ubic_geo))) |>
  mutate(estado = as.numeric(separados$estado),
         municipio = separados$municipio)|>
  left_join(codigos_estados)|>
  select(-separados)|>
  mutate(tipo_cor = as.numeric(str_extract(tipo_cor, "\\d+")),
         año=2017)

sp19<-read_csv("datos/pers_cor_cngmd2019.csv")|>
  select(-2,-3,-20)|>
  pivot_longer(cols=c(2:17),
               names_to="tipo_cor",
               values_to="persomun")|>
  group_by(ubic_geo,tipo_cor)|>
  mutate(persomun=as.numeric(persomun))|>
  summarise(persomun=sum(persomun,na.rm=T))|>
  unique()|>
  rowwise() |>
  mutate(separados = list(separar_estado_municipio(ubic_geo))) |>
  mutate(estado = as.numeric(separados$estado),
         municipio = separados$municipio)|>
  left_join(codigos_estados)|>
  select(-separados)|>
  mutate(tipo_cor = as.numeric(str_extract(tipo_cor, "\\d+")))|>
  mutate(año=2019)

sp21<-read_csv("datos/m3s1p2_cngmd2021.csv")|>
  select(-sexostt)|>
  rowwise() |>
  mutate(separados = list(separar_estado_municipio(ubicageo_c))) |>
  mutate(estado = as.numeric(separados$estado),
         municipio = separados$municipio)|>
  left_join(codigos_estados)|>
  select(-separados)|>
  mutate(año=2021)

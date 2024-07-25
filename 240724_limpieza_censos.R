library(readr)
library(dplyr)
source("sep_est.R")

codigos_estados <- data.frame(
  estado = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
             "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
             "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", 
             "31", "32"),
  nombre_estado = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", 
                    "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", 
                    "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", 
                    "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", 
                    "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", 
                    "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"))

mexmun15<-read_csv("datos/rec_huma_mun15.csv")|>
  mutate(año=2015,
         pais="Mexico",
         estado = substr(ubic_geo, 1, 2),
         municipio = substr(ubic_geo, 3, 5))|>
  group_by(municipio,estado)|>
  summarise(persomun=sum(tt_perso,na.rm=T),año)|>
  unique()|>
  mutate(año=2015)
mexmun17<-read_csv("datos/rec_huma_mun17.csv")|>
  mutate(año=2017,
         pais="Mexico",
         estado = substr(ubic_geo, 1, 2),
         municipio = substr(ubic_geo, 3, 5))|>
  group_by(municipio,estado)|>
  summarise(persomun=sum(as.numeric(totalca1),na.rm=T),año)|>
  unique()|>
  mutate(año=2017)
mexmun19<-read_csv("datos/rec_huma_mun19.csv")|>
  mutate(año=2019,
         pais="Mexico",
         estado = substr(ubic_geo, 1, 2),
         municipio = substr(ubic_geo, 3, 5))|>
  group_by(municipio,estado)|>
  summarise(persomun=sum(as.numeric(totalca1),na.rm=T),año)|>
  unique()|>
  mutate(año=2019)


mexmun21 <- read_csv("datos/rec_huma_mun21.csv")|>
  mutate(año = 2021,
         pais = "Mexico") |>
  rowwise() |>
  mutate(separados = list(separar_estado_municipio(ubicageo_c))) |>
  mutate(estado = separados$estado,
         municipio = separados$municipio) |>
  select(-separados) |>  
  ungroup() |>
  group_by(municipio,estado) |>
  summarise(persomun = sum(as.numeric(sexostt), na.rm = TRUE), año) |>
  unique()|>
  mutate(año=2021)
mexmun23<-read_csv("datos/rec_huma_mun23.csv")|>
  mutate(año=2023,
         pais="Mexico") |>
  rowwise() |>
  mutate(separados = list(separar_estado_municipio(ubicageo_f))) |>
  mutate(estado = separados$estado,
         municipio = separados$municipio) |>
  select(-separados) |> 
  ungroup() |>
  group_by(municipio,estado) |>
  summarise(persomun = sum(as.numeric(sexostt), na.rm = TRUE), año) |>
  unique()|>
  mutate(año=2023)

df<-bind_rows(mexmun15,mexmun17,mexmun19,mexmun21,mexmun23)|>
  left_join(codigos_estados)

saveRDS(df,"trabsmuns.RDS")

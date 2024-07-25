library(readr)
library(tidyverse)
########Data Censal#######
######## Acá voy a usar el censo 2010, 2020 y la encuensta intercensal 2015 y ps a imputar lo de enmedio
pob2010<-read_csv("datos/iter_00_cpv2010.csv")|>
  select(1:4,7:10)|>
  rename(estado=entidad,
         municipio=mun,
         pop=pobtot,
         nombre_estado=nom_ent,
         nombre_mun=nom_mun)|>
  filter(!str_starts(nombre_mun, "Total"))|>
  group_by(estado,municipio)|>
  summarise(pop=sum(pop,na.rm=T),nombre_estado,nombre_mun)|>
  mutate(año=2010)|>
  unique()

pob2020<-read_csv("datos/iter_00CSV20.csv")|>
  select(1:4,7:10)|>
  rename(estado=ENTIDAD,
         municipio=MUN,
         pop=POBTOT,
         nombre_estado=NOM_ENT,
         nombre_mun=NOM_MUN)|>
  filter(!str_starts(nombre_mun, "Total"))|>
  group_by(estado,municipio)|>
  summarise(pop=sum(pop,na.rm=T),nombre_estado,nombre_mun)|>
  mutate(año=2020)|>
  unique()


library(purrr)
pop<-bind_rows(pob2010,pob2020)
pop <- data.frame(
  estado = pop$estado,
  municipio = pop$municipio,
  nombre_estado = pop$nombre_estado,
  nombre_mun = pop$nombre_mun,
  year = pop$año,
  pop = pop$pop
)

years_to_impute <- data.frame(year = c(2015, 2017, 2019))

# Crear un dataframe con todos los años
all_years <- expand.grid(
  estado = unique(pop$estado),
  municipio = unique(pop$municipio),
  year = c(2010, 2015, 2017, 2019, 2020)
)

# Unir el dataframe original con el dataframe de todos los años
pop <- all_years |>
  left_join(pop, by = c("estado", "municipio", "year")) |>
  arrange(estado, municipio, year) |>
  group_by(estado, municipio) |>
  fill(nombre_estado, nombre_mun, .direction = "downup") |>
  mutate(
    pop = if (sum(!is.na(pop)) >= 2) approx(year, pop, year)$y else pop,
    estado=as.numeric(estado)
  )|>
  ungroup()|>
  rename(año=year)
########Ingresos Municipales######
file_names <- list.files(pattern = "efipem_municipal_anual_tr_cifra_\\d{4}.csv")


load_and_process_file <- function(file) {
  data <- read_csv(file) %>%
    mutate(año = as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", file)))
  data <- data|>
    rename(estado=ID_ENTIDAD,
           tema=TEMA,
           valor=VALOR)|>
    filter(DESCRIPCION_CATEGORIA %in% c("Derechos",
                                        "Impuestos",
                                        "Productos",
                                        "Aprovechamientos") |
             startsWith(DESCRIPCION_CATEGORIA, "Remu"))|>
    group_by(estado, año)|>
    summarise(
      ingresos = sum(valor[tema == "Ingresos"], na.rm = TRUE),
      egresos = sum(valor[tema == "Egresos"], na.rm = TRUE),
      diferencia = egresos/ingresos
    )
} 

all_data_mun<- map_dfr(file_names, load_and_process_file)|>
  group_by(año)|>
  summarise(ingresos=sum(ingresos),egresos=sum(egresos))|>
  mutate(diferencia = egresos/ingresos)|>
  select(año, diferencia)
########
poli<-read_rds("polimun.RDS")
muns<-read_rds("trabsmuns.RDS")|>
  mutate(estado=as.numeric(estado))
df<-poli|>
  left_join(muns,by=c("municipio","estado","año","nombre_estado"))|>
  left_join(pop,by=c("municipio","estado","año","nombre_estado"))|>
  mutate(pop=ceiling(pop))

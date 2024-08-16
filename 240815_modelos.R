library(readr)
library(dplyr)
library(plm)
library(car)
library(sf)
library(spdep)
library(splm)

df<-read_rds("labaseyalimpiaapocono.RDS")
df <- st_drop_geometry(df)
df <- df |>
  group_by(CVEGEO) |>
  mutate(
    treat = as.factor(ifelse(total_plantios > 0 & año >= 2017, 1, 0)),
    time = ifelse(año > 2017, 1, 0),
    treat2 = as.factor(ifelse(
      año > 2017 & any(año == 2017),
      if_else(total_plantios < total_plantios[año == 2017], 1, 0),
      0
    ))
  ) |>
  ungroup()
df <- pdata.frame(df, index = c("CVEGEO", "time"))

m0<-plm(log(poli_mun+1)~treat*time,data=df,model="within")
summary(m0)

m0.1<-plm(log(poli_mun+1)~treat*time+log(prim_income+1)+log(transfer+1)+factor(estado)+luminosity+log(pop+1)+total_plantios,data=df,model="within")
summary(m0.1)


m1<-plm(log(persomun+1)~treat*time,data=df,model="within")
summary(m1)

m1.1<-plm(log(persomun+1)~treat*time+log(prim_income+1)+log(transfer+1)+factor(estado)+luminosity+log(pop+1)+total_plantios,data=df,model="within")
summary(m1.1)



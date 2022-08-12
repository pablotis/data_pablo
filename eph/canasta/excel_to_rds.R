library(eph)
library(readxl)
library(tidyverse)
library(glue)

cba <- read_excel("eph/canasta/canastas_eph.xlsx",
                  sheet = "CBA")
ice <- read_excel("eph/canasta/canastas_eph.xlsx",
                  sheet = "ICE")
cbt <- read_excel("eph/canasta/canastas_eph.xlsx",
                  sheet = "CBT")


eph::canastas_reg_example

cba$tipo <- 'cba'
cbt$tipo  <- 'cbt'
ice$tipo  <- 'ice'

canastas <- bind_rows(cba, cbt, ice)

canastas <- canastas %>%
  pivot_longer(., GBA:Patagonia, names_to = "region", values_to = "valor") %>%
  pivot_wider(., names_from = tipo, values_from = valor) %>%
  mutate(trimestre = case_when(Mes %in% 1:3 ~ 1,
                               Mes %in% 4:6 ~ 2,
                               Mes %in% 7:9 ~ 3,
                               Mes %in% 10:12 ~ 4),
         periodo = paste(Anio,trimestre,sep = '.')) %>%
  group_by(region, periodo) %>%
  summarise(CBA = mean(cba),
            CBT = mean(cbt)) %>%
  left_join(eph::diccionario_regiones)

saveRDS(canastas, "eph/canasta/canastas.rds")




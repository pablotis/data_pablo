

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------------- INDIVIDUAL-----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(stringr)
library(tidyverse)



list.files("eph/individual/")

bases_en_repo <- data.frame(
  anio = stringr::str_sub(list.files("eph/individual/"), start = 17, 20),
  trim = stringr::str_sub(list.files("eph/individual/"), start = 22, 22)) %>% 
  mutate(
    periodo = paste0(anio, "_", trim),
    dplyr::across(everything(.), as.numeric))


base_por_bajar <- data.frame(
  anio = rep(2022:2040, each = 4),
  trim = rep(1:4, 2040-2022 + 1)) %>% 
  dplyr::mutate(
    periodo = paste0(anio, "_", trim),
    periodo_check = periodo %in% bases_en_repo$periodo) %>% 
  dplyr::filter(periodo_check == FALSE)


anio <- as.numeric(base_por_bajar[1, "anio"])
trimestre <- as.numeric(base_por_bajar[1, "trim"])

### Descargo la base en cuestión
url <- glue::glue("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_{trimestre}_Trim_{anio}_txt.zip")
temp <- tempfile()
download.file(url,temp)


lista_zip <- unzip(temp, list = TRUE)

base_ind <- read.table(
  unz(
    temp,
    lista_zip$Name[grepl('individual', lista_zip$Name)]
  ),
  sep = ";", header = T, dec = ",")


#unlink(temp)

base_ind <- base_ind %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename_all(toupper) %>% 
  dplyr::mutate(CH14 = as.character(CH14),
         PP3E_TOT = as.integer(PP3E_TOT),
         PP3F_TOT = as.integer(PP3F_TOT),
         PP04B_COD = as.character(PP04B_COD),
         PP04D_COD = as.character(PP04D_COD),
         PP11B_COD = as.character(PP11B_COD),
         PP11D_COD = as.character(PP11D_COD),
         DECOCUR   = as.character(DECOCUR),
         IDECOCUR  = as.character(IDECOCUR),
         RDECOCUR  = as.character(RDECOCUR),
         GDECOCUR  = as.character(GDECOCUR),
         PDECOCUR  = as.character(PDECOCUR),
         ADECOCUR  = as.character(ADECOCUR),
         DECINDR   = as.character(DECINDR),
         IDECINDR  = as.character(IDECINDR),
         RDECINDR  = as.character(RDECINDR),
         GDECINDR  = as.character(GDECINDR),
         PDECINDR  = as.character(PDECINDR),
         ADECINDR  = as.character(ADECINDR),
         DECIFR    = as.character(DECIFR),
         IDECIFR   = as.character(IDECIFR),
         RDECIFR   = as.character(RDECIFR),
         GDECIFR   = as.character(GDECIFR),
         PDECIFR   = as.character(PDECIFR),
         ADECIFR   = as.character(ADECIFR),
         DECCFR    = as.character(DECCFR),
         IDECCFR   = as.character(IDECCFR),
         RDECCFR   = as.character(RDECCFR),
         GDECCFR   = as.character(GDECCFR),
         PDECCFR   = as.character(PDECCFR),
         ADECCFR   = as.character(ADECCFR),
         IPCF      = as.numeric(IPCF))


saveRDS(base_ind, glue::glue("eph/individual/base_individual_{anio}T{trimestre}.RDS"))
#table(is.na(base$IPCF))



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------------- HOGAR-------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------------- INDIVIDUAL-----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(stringr)
library(tidyverse)

list.files("eph/hogar/")

bases_en_repo <- data.frame(
  anio = stringr::str_sub(list.files("eph/hogar/"), start = 12, 15),
  trim = stringr::str_sub(list.files("eph/hogar/"), start = 17, 17)) %>% 
  dplyr::mutate(
    periodo = paste0(anio, "_", trim),
    dplyr::across(tidyselect::everything(.), as.numeric))


base_por_bajar <- data.frame(
  anio = rep(2022:2040, each = 4),
  trim = rep(1:4, 2040-2022 + 1)) %>% 
  dplyr::mutate(
    periodo = paste0(anio, "_", trim),
    periodo_check = periodo %in% bases_en_repo$periodo) %>% 
  dplyr::filter(periodo_check == FALSE)


anio <- as.numeric(base_por_bajar[1, "anio"])
trimestre <- as.numeric(base_por_bajar[1, "trim"])

### Descargo la base en cuestión
url <- glue::glue("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_{trimestre}_Trim_{anio}_txt.zip")
temp <- tempfile()
download.file(url,temp)


lista_zip <- unzip(temp, list = TRUE)

base_hog <- read.table(
  unz(
    temp,
    lista_zip$Name[grepl('hogar', lista_zip$Name)]
  ),
  sep = ";", header = T, dec = ",")




base_hog <- base_hog %>% 
  as_tibble() %>% 
  rename_all(toupper) %>% 
  mutate(DECIFR = as.character(DECIFR),
         IDECIFR = as.character(IDECIFR),
         GDECIFR = as.character(GDECIFR),
         PDECIFR = as.character(PDECIFR),
         ADECIFR = as.character(ADECIFR),
         DECCFR  = as.character(DECCFR),
         IDECCFR = as.character(IDECCFR),
         RDECCFR = as.character(RDECCFR),
         GDECCFR = as.character(GDECCFR),
         PDECCFR = as.character(PDECCFR),
         ADECCFR = as.character(ADECCFR),
         RDECIFR = as.character(RDECIFR),
         IPCF    = as.numeric(as.character(IPCF)))


saveRDS(base_hog, glue::glue("eph/hogar/base_hogar_{anio}T{trimestre}.RDS"))




library(tidyverse)
library(glue)
library(fs)
library(progress)


bases_hogar <- dir_ls('eph/hogar/')
pb <- progress_bar$new(total = length(bases_hogar))

for (dir in bases_hogar) {
  pb$tick()
  df <- read_rds(dir)
  filas <- nrow(df)
  columnas <- ncol(df)
  if (filas<10000) {
    warning( glue('error en el archivo {dir}. Tiene {filas} filas'))
  }
  if (columnas<35) {
    warning( glue('error en el archivo {dir}. Tiene {columnas} columnas'))
  }
}
warnings()

bases_individual <- dir_ls('eph/individual/')
pb <- progress_bar$new(total = length(bases_individual))
for (dir in bases_individual) {
    pb$tick()
    df <- read_rds(dir)
    filas <- nrow(df)
    columnas <- ncol(df)
    if (filas<10000) {
      warning( glue('error en el archivo {dir}. Tiene {filas} filas'))
    }
    if (columnas<35) {
      warning( glue('error en el archivo {dir}. Tiene {columnas} columnas'))
    }
}

warnings()

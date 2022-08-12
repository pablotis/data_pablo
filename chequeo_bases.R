##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                       CHEQUEO DE ESTRUCTURA DE BASES - Año 2020          ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Librerias
library(tidyverse)
library(eph)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    A través de la función get_microdata                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................una por una...........................
b_2020_t1 <- get_microdata(2020, 1)
b_2021_t1 <- get_microdata(2021, 1)
b_2022_t1 <- get_microdata(2022, 1)


## Apilo en una sola
b_tot <- dplyr::bind_rows(b_2020_t1,
                                     b_2021_t1,
                                     b_2022_t1)

class(b_2020_t1$PP04B_COD)
class(b_2021_t1$PP04B_COD)
class(b_2022_t1$PP04B_COD)


#................Pruebo la serie entera para 2020................
b_2020_tot_eph <- get_microdata(year = 2020, trimester = 1:4) %>% 
  select(microdata) %>% 
  unnest(microdata)





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          a través de la base local                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

b_2017_t1 <- readRDS("eph/individual/base_individual_2017T1.RDS")
b_2018_t1 <- readRDS("eph/individual/base_individual_2018T1.RDS")
b_2019_t1 <- readRDS("eph/individual/base_individual_2019T1.RDS")
b_2020_t1 <- readRDS("eph/individual/base_individual_2020T1.RDS")
b_2021_t1 <- readRDS("eph/individual/base_individual_2021T1.RDS")
b_2022_t1 <- readRDS("eph/individual/base_individual_2022T1.RDS")


b_tot <- dplyr::bind_rows(
  b_2017_t1,
  b_2018_t1,
  b_2019_t1,
  b_2020_t1,
  b_2021_t1,
  b_2022_t1
  )




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                       CHEQUEO DE ESTRUCTURA DE BASES - Interanuales_t3   ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Librerias
library(tidyverse)
library(eph)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          a través de la base local                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

b_2016_t4 <- readRDS("eph/individual/base_individual_2016T2.RDS")
b_2017_t4 <- readRDS("eph/individual/base_individual_2017T2.RDS")
b_2018_t4 <- readRDS("eph/individual/base_individual_2018T2.RDS")
b_2019_t4 <- readRDS("eph/individual/base_individual_2019T2.RDS")
b_2020_t4 <- readRDS("eph/individual/base_individual_2020T2.RDS")
b_2021_t4 <- readRDS("eph/individual/base_individual_2021T2.RDS")


b_interanual_tot <- dplyr::bind_rows(
  b_2016_t4,
  b_2017_t4,
  b_2018_t4,
  b_2019_t4,
  b_2020_t4,
  b_2021_t4
)


class(b_2020_t4$PP04B_COD)





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    A través de la función get_microdata                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#..........................una por una...........................
b_2016_t4 <- get_microdata(2016, 4)
b_2017_t4 <- get_microdata(2017, 4)
b_2018_t4 <- get_microdata(2018, 4)
b_2019_t4 <- get_microdata(2019, 4)
b_2020_t4 <- get_microdata(2020, 4)
b_2021_t4 <- get_microdata(2021, 4)

## Apilo en una sola
b_interanual_dplyr <- dplyr::bind_rows(b_2016_t4, 
                                       b_2017_t4, 
                                       b_2018_t4, 
                                       b_2019_t4, 
                                       b_2020_t4, 
                                       b_2021_t4)


#................Pruebo la serie entera para 2020................
b_interanual_tot_eph <- get_microdata(year = 2016:2021, trimester = 4) %>% 
  select(microdata) %>% 
  unnest(microdata)


# fix base_individual_2000O1 base_individual_2000O2 -----------------------
fix_encoding <- function(df, originalEncoding = "latin1") {
  numCols <- ncol(df)
  df <- data.frame(df)
  for (col in 1:numCols)
  {
    if(class(df[, col]) == "character"){
      Encoding(df[, col]) <- originalEncoding
    }
    
    if(class(df[, col]) == "factor"){
      Encoding(levels(df[, col])) <- originalEncoding
    }
  }
  return(as_tibble(df))
}
test_miscoding <- function(df){levels(df$P57)[str_detect(levels(df$P57),'AGRIMENSOR')]}

base_individual_2000O1 <-  readRDS('eph/individual/base_individual_2000O1.RDS')

base_individual_2000O1b <- fix_encoding(base_individual_2000O1,originalEncoding = 'CP1252')

x <-  stringi::stri_enc_toascii(levels(base_individual_2000O1$P57))
x[str_detect(x,'AGRIMENSOR')]

test_miscoding(base_individual_2000O1b)

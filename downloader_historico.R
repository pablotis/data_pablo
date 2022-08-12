library(tidyverse)
library(purrr)
library(glue)
library(haven)
library(eph)

### 30/10/2019 Paso bases a tibble

## bases puntuales hogar ----

bases_hogar <- get_microdata(year = 1996:2003, wave = c(1,2),type = 'hogar')

bases_hogar <- bases_hogar %>% 
  filter(!(year==2003 & wave ==2))

bases_hogar <- bases_hogar %>% 
  mutate(filename = glue('eph/hogar/base_hogar_{year}O{wave}.RDS'),
         microdata = map(microdata, as.tibble))

walk2(bases_hogar$microdata,bases_hogar$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})

## bases puntuales individual ----

bases <- get_microdata(year = 1996:2003, wave = c(1,2),type = 'individual')

bases <- bases %>% 
  filter(!(year==2003 & wave ==2)) %>% 
  mutate(filename = glue('eph/individual/base_individual_{year}O{wave}.RDS'),
         microdata = map(microdata, as.tibble))

# arreglo una variable que tenia diferentes tipos
bases <- bases %>% 
  mutate(microdata = map(microdata, 
                         function(x){x %>% mutate(P13AUS = as.character(P13AUS))}))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})


## bases continuas 2003:2007 hogar ----

bases <- get_microdata(year = 2003:2007, trimester = 1:4,type = 'hogar')

bases <- bases %>% 
  filter(!(year==2003 & trimester%in%c(1,2)),
         !(year==2007 & trimester%in%c(3,4))) %>% 
  mutate(filename = glue('eph/hogar/base_hogar_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         test = file.exists(filename))

# test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})


## bases continuas 2008:2016 hogar ----

bases <- get_microdata(year = 2008:2016, trimester = 1:4,type = 'hogar')

bases <- bases %>% 
  filter(!(year==2015 & trimester%in%3:4),
         !(year==2016 & trimester==1)) %>% 
  mutate(filename = glue('eph/hogar/base_hogar_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases%>% 
  mutate(microdata = map(microdata, 
                         function(x){x %>% mutate(DECIFR = as.character(DECIFR),
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
                                                  RDECIFR = as.character(RDECIFR))
                         }))

#test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})

## bases continuas 2003:2007 individual ----

bases <- get_microdata(year = 2003:2007, trimester = 1:4,type = 'individual')

bases <- bases %>% 
  filter(!(year==2003 & trimester%in%c(1,2)),
         !(year==2007 & trimester%in%c(3,4))) %>% 
  mutate(filename = glue('eph/individual/base_individual_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         test = file.exists(filename))

bases <- bases %>% 
  mutate(microdata = map(microdata, 
                         function(x){
                           if(!'ADECCFR' %in% names(x)) {x$ADECCFR <- NA_character_}
                           if(!'ADECIFR' %in% names(x)) {x$ADECIFR <- NA_character_}
                           if(!'ADECINDR' %in% names(x)) {x$ADECINDR <- NA_character_}
                           if(!'ADECOCUR' %in% names(x)) {x$ADECOCUR <- NA_character_}
                           if(!'CH14' %in% names(x)) {x$CH14 <- NA_character_}
                           if(!'CH05' %in% names(x)) {x$CH05 <- NA_character_}
                           if(!'CH15' %in% names(x)) {x$CH15 <- NA_character_}
                           if(!'CH15_COD' %in% names(x)) {x$CH15_COD <- NA_character_}
                           if(!'CH16' %in% names(x)) {x$CH16 <- NA_character_}
                           if(!'CH16_COD' %in% names(x)) {x$CH16_COD <- NA_character_}
                           if(!'DECCFR' %in% names(x)) {x$DECCFR <- NA_character_}
                           if(!'DECIFR' %in% names(x)) {x$DECIFR <- NA_character_}
                           if(!'DECINDR' %in% names(x)) {x$DECINDR <- NA_character_}
                           if(!'DECOCUR' %in% names(x)) {x$DECOCUR <- NA_character_}
                           if(!'GDECCFR' %in% names(x)) {x$GDECCFR <- NA_character_}
                           if(!'GDECIFR' %in% names(x)) {x$GDECIFR <- NA_character_}
                           if(!'GDECINDR' %in% names(x)) {x$GDECINDR <- NA_character_}
                           if(!'GDECOCUR' %in% names(x)) {x$GDECOCUR <- NA_character_}
                           if(!'IDECCFR' %in% names(x)) {x$IDECCFR <- NA_character_}
                           if(!'IDECIFR' %in% names(x)) {x$IDECIFR <- NA_character_}
                           if(!'IDECINDR' %in% names(x)) {x$IDECINDR <- NA_character_}
                           if(!'IDECOCUR' %in% names(x)) {x$IDECOCUR <- NA_character_}
                           if(!'ITF' %in% names(x)) {x$ITF <- NA_character_}
                           if(!'PDECCFR' %in% names(x)) {x$PDECCFR <- NA_character_}
                           if(!'PDECIFR' %in% names(x)) {x$PDECIFR <- NA_character_}
                           if(!'PDECINDR' %in% names(x)) {x$PDECINDR <- NA_character_}
                           if(!'PDECOCUR' %in% names(x)) {x$PDECOCUR <- NA_character_}
                           if(!'PP02C1' %in% names(x)) {x$PP02C1 <- NA_character_}
                           if(!'PP02C2' %in% names(x)) {x$PP02C2 <- NA_character_}
                           if(!'PP02C3' %in% names(x)) {x$PP02C3 <- NA_character_}
                           if(!'PP02C4' %in% names(x)) {x$PP02C4 <- NA_character_}
                           if(!'PP02C5' %in% names(x)) {x$PP02C5 <- NA_character_}
                           if(!'PP02C6' %in% names(x)) {x$PP02C6 <- NA_character_}
                           if(!'PP02C7' %in% names(x)) {x$PP02C7 <- NA_character_}
                           if(!'PP02C8' %in% names(x)) {x$PP02C8 <- NA_character_}
                           if(!'PP02E' %in% names(x)) {x$PP02E <- NA_character_}
                           if(!'PP3E_TOT' %in% names(x)) {x$PP3E_TOT <- NA_character_}
                           if(!'PP3F_TOT' %in% names(x)) {x$PP3F_TOT <- NA_character_}
                           if(!'PP04B_COD' %in% names(x)) {x$PP04B_COD <- NA_character_}
                           if(!'PP04D_COD' %in% names(x)) {x$PP04D_COD <- NA_character_}
                           if(!'PP05E' %in% names(x)) {x$PP05E <- NA_character_}
                           if(!'PP07A' %in% names(x)) {x$PP07A <- NA_character_}
                           if(!'PP11B_COD' %in% names(x)) {x$PP11B_COD <- NA_character_}
                           if(!'PP11D_COD' %in% names(x)) {x$PP11D_COD <- NA_character_}
                           if(!'RDECCFR' %in% names(x)) {x$RDECCFR <- NA_character_}
                           if(!'RDECIFR' %in% names(x)) {x$RDECIFR <- NA_character_}
                           if(!'RDECINDR' %in% names(x)) {x$RDECINDR <- NA_character_}
                           if(!'RDECOCUR' %in% names(x)) {x$RDECOCUR <- NA_character_}
                           if(!'T_VI' %in% names(x)) {x$T_VI <- NA_character_}
                           if(!'TOT_P12' %in% names(x)) {x$TOT_P12 <- NA_character_}
                           if(!'V10_M' %in% names(x)) {x$V10_M <- NA_character_}
                           if(!'V11_M' %in% names(x)) {x$V11_M <- NA_character_}
                           if(!'V12_M' %in% names(x)) {x$V12_M <- NA_character_}
                           if(!'V18_M' %in% names(x)) {x$V18_M <- NA_character_}
                           if(!'V19_AM' %in% names(x)) {x$V19_AM <- NA_character_}
                           if(!'V2_M' %in% names(x)) {x$V2_M <- NA_character_}
                           if(!'V21_M' %in% names(x)) {x$V21_M <- NA_character_}
                           if(!'V3_M' %in% names(x)) {x$V3_M <- NA_character_}
                           if(!'V4_M' %in% names(x)) {x$V4_M <- NA_character_}
                           if(!'V5_M' %in% names(x)) {x$V5_M <- NA_character_}
                           if(!'V8_M' %in% names(x)) {x$V8_M <- NA_character_}
                           if(!'V9_M' %in% names(x)) {x$V9_M <- NA_character_}
                           
                           x %>% mutate(ADECCFR = as.character(ADECCFR),
                                        ADECIFR = as.character(ADECIFR),
                                        ADECINDR = as.character(ADECINDR),
                                        ADECOCUR = as.character(ADECOCUR),
                                        CH05 = as.factor(CH05),
                                        CH14 = as.character(CH14),
                                        CH14 = as.character(CH14),
                                        CH15 = as.integer(CH15),
                                        CH15_COD = as.character(CH15_COD),
                                        CH16 = as.integer(CH16),
                                        CH16_COD = as.character(CH16_COD),
                                        DECCFR = as.character(DECCFR),
                                        DECIFR = as.character(DECIFR),
                                        DECINDR = as.character(DECINDR),
                                        DECOCUR = as.character(DECOCUR),
                                        GDECCFR = as.character(GDECCFR),
                                        GDECIFR = as.character(GDECIFR),
                                        GDECINDR = as.character(GDECINDR),
                                        GDECOCUR = as.character(GDECOCUR),
                                        IDECCFR = as.character(IDECCFR),
                                        IDECIFR = as.character(IDECIFR),
                                        IDECINDR = as.character(IDECINDR),
                                        IDECOCUR = as.character(IDECOCUR),
                                        ITF = as.integer(ITF),
                                        PDECCFR = as.character(PDECCFR),
                                        PDECIFR = as.character(PDECIFR),
                                        PDECINDR = as.character(PDECINDR),
                                        PDECOCUR = as.character(PDECOCUR),
                                        PP02C1 = as.integer(PP02C1),
                                        PP02C2 = as.integer(PP02C2),
                                        PP02C3 = as.integer(PP02C3),
                                        PP02C4 = as.integer(PP02C4),
                                        PP02C5 = as.integer(PP02C5),
                                        PP02C6 = as.integer(PP02C6),
                                        PP02C7 = as.integer(PP02C7),
                                        PP02C8 = as.integer(PP02C8),
                                        PP02E = as.integer(PP02E),
                                        PP04B_COD = as.character(PP04B_COD),
                                        PP04D_COD = as.character(PP04D_COD),
                                        PP05E = as.integer(PP05E),
                                        PP07A = as.integer(PP07A),
                                        PP11B_COD = as.character(PP11B_COD),
                                        PP11D_COD = as.character(PP11D_COD),
                                        PP3E_TOT = as.integer(PP3E_TOT),
                                        PP3F_TOT = as.integer(PP3F_TOT),
                                        RDECCFR = as.character(RDECCFR),
                                        RDECIFR = as.character(RDECIFR),
                                        RDECINDR = as.character(RDECINDR),
                                        RDECOCUR = as.character(RDECOCUR),
                                        T_VI = as.integer(T_VI),
                                        TOT_P12 = as.integer(TOT_P12),
                                        V10_M = as.integer(V10_M),
                                        V11_M = as.integer(V11_M),
                                        V12_M = as.integer(V12_M),
                                        V18_M = as.integer(V18_M),
                                        V19_AM = as.integer(V19_AM),
                                        V2_M = as.integer(V2_M),
                                        V21_M = as.integer(V21_M),
                                        V3_M = as.integer(V3_M),
                                        V4_M = as.integer(V4_M),
                                        V5_M = as.integer(V5_M),
                                        V8_M = as.integer(V8_M),
                                        V9_M = as.integer(V9_M)
                           )
                         }))

#test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})


## bases continuas 2008:2015 individual ----

bases <- get_microdata(year = 2008:2015, trimester = 1:4,type = 'individual')

bases <- bases %>% 
  filter(!(year==2015 & trimester%in%3:4),
         !(year==2016 & trimester==1)) %>% 
  mutate(filename = glue('eph/individual/base_individual_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases %>% 
  mutate(microdata = map(microdata, 
                         function(x){
                           if (!'PP04B_COD'%in% names(x)) {x$PP04B_COD <- NA_character_}
                           if (!'PP09A_ESP'%in% names(x)) {x$PP09A_ESP <- NA_character_}
                           if (!'PP09C_ESP'%in% names(x)) {x$PP09C_ESP <- NA_character_}
                           if (!'PP11B_COD'%in% names(x)) {x$PP11B_COD <- NA_character_}
                           if(!'CH05' %in% names(x)) {x$CH05 <- NA_character_}
                           if(!'CH15_COD' %in% names(x)) {x$CH15_COD <- NA_character_}
                           if(!'CH16_COD' %in% names(x)) {x$CH16_COD <- NA_character_}
                           
                           x %>% mutate(CH05 = as.factor(CH05),
                                        CH15_COD = as.character(CH15_COD),
                                        CH16_COD = as.character(CH16_COD),
                                        #CODUSU = as.factor(CODUSU),
                                        #MAS_500 = as.factor(MAS_500),
                                        PP09A_ESP = as.factor(PP09A_ESP),
                                        PP09C_ESP = as.factor(PP09C_ESP)
                                        
                           )
                         }))

#test <-  bases %>% unnest(cols = c(microdata))
walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})


### Bases post 2016 ----

# 2016 - individual
bases <- get_microdata(year = 2016, trimester = 2:4, type = 'individual')

bases <- bases %>% 
  mutate(filename = glue('eph/individual/base_individual_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases %>% 
  mutate(microdata = map(microdata, 
                         function(x){
                           if (!'PP04B_COD'%in% names(x)) {x$PP04B_COD <- NA_character_}
                           if (!'PP11B_COD'%in% names(x)) {x$PP11B_COD <- NA_character_}
                           
                           x %>% mutate(CH14 = as.character(CH14),
                                        CH15_COD = as.integer(as.character(CH15_COD)),
                                        CH16_COD = as.integer(as.character(CH16_COD)),
                                        PP04B_COD = as.character(PP04B_COD),
                                        PP04D_COD = as.character(PP04D_COD),
                                        PP11B_COD = as.character(PP11B_COD),
                                        PP11D_COD = as.character(PP11D_COD),
                                        IPCF      = as.numeric(as.character(IPCF)),
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
                                        ADECCFR   = as.character(ADECCFR)
                                        
                           )
                         }))

#test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})



## Hogar 2019
bases <- get_microdata(year = 2019, trimester = 1:2,type = 'hogar')

bases <- bases %>% 
  mutate(filename = glue('eph/hogar/base_hogar_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases%>% 
  mutate(microdata = map(microdata, 
                         function(x){x %>% mutate(DECIFR = as.character(DECIFR),
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
                                                  RDECIFR = as.character(RDECIFR))
                         }))

#test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})

### individual
bases <- get_microdata(year = 2019, trimester = 1:2,type = 'individual')

bases <- bases %>% 
  mutate(filename = glue('eph/individual/base_individual_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as.tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases %>% 
  mutate(microdata = map(microdata, 
                         function(x){
                           if (!'PP04B_COD'%in% names(x)) { x$PP04B_COD <- NA_character_}
                           if (!'PP11B_COD'%in% names(x)) {x$PP11B_COD <- NA_character_}
                           x %>% mutate(CH14 = as.character(CH14),
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
                                        IDECINDR   = as.character(IDECINDR),
                                        RDECINDR   = as.character(RDECINDR),
                                        GDECINDR   = as.character(GDECINDR),
                                        PDECINDR   = as.character(PDECINDR),
                                        ADECINDR   = as.character(ADECINDR),
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
                                        ADECCFR   = as.character(ADECCFR)
                                        
                           )
                         }))

#test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})






########################### 2020 ----
### individual
bases <- get_microdata(year = 2020, trimester = 1:4, type = 'individual')

bases <- bases %>% 
  mutate(filename = glue('eph/individual/base_individual_{year}T{trimester}.RDS'),
         microdata = map(microdata, zap_labels),
         microdata = map(microdata, as_tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases %>% 
  mutate(microdata = map(microdata, 
                         function(x){
                           if (!'PP04B_COD'%in% names(x)) {x$PP04B_COD <- NA_character_}
                           if (!'PP11B_COD'%in% names(x)) {x$PP11B_COD <- NA_character_}
                           x %>% mutate(CH14 = as.character(CH14),
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
                                        ADECCFR   = as.character(ADECCFR)
                           )
                         }))

#test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})





################# HOGAR - 2020 en adelante
# Definir año y trimestre
bases <- get_microdata(year = 2020:2020, trimester = 1,type = 'hogar')

bases <- bases %>% 
  mutate(filename = glue('eph/hogar/base_hogar_{year}T{trimester}.RDS'),
         microdata = map(microdata,zap_labels),
         microdata = map(microdata, as_tibble),
         microdata = map(microdata, 
                         function(x){ names(x) <- toupper(names(x))
                         return(x)}),
         test = file.exists(filename))

# arreglo una variable que tenia diferentes tipos

bases <- bases%>% 
  mutate(microdata = map(microdata, 
                         function(x){x %>% mutate(DECIFR = as.character(DECIFR),
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
                         }))

#test <-  bases %>% unnest(cols = c(microdata))

walk2(bases$microdata,bases$filename,
      function(.x,.y){saveRDS(object = .x,file = .y)})





########################### 2020-t4 en adelante (ojo con el separador de decimales, pasó a ser ","----


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Tabla de bases descargadas  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

base <- data.frame(anio = as.numeric(substring(list.files("eph/individual/"), 17, 20)),
                   trim = as.numeric(substring(list.files("eph/individual/"), 22, 22)))


anio <- max(as.numeric(substring(list.files("eph/individual/"), 17, 20)))

list.files("eph/individual/")

### Defino fecha fecha de la base a subir
anio <- 2021
trimestre <- 4

### Descargo la base en cuestión
url <- glue::glue("https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_{trimestre}_Trim_{anio}_txt.zip")
temp <- tempfile()
download.file(url,temp)


lista_zip <- unzip(temp, list = TRUE)

base <- read.table(
  unz(
    temp,
    lista_zip$Name[grepl('individual', lista_zip$Name)]
  ),
  sep = ";", header = T, dec = ",")



#unlink(temp)

base <- base %>% 
  as_tibble() %>% 
  rename_all(toupper) %>% 
  mutate(CH14 = as.character(CH14),
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


saveRDS(base, glue::glue("eph/individual/base_individual_{anio}T{trimestre}.RDS"))
#table(is.na(base$IPCF))


### Hogar
base_hog <- read.table(unz(temp, glue::glue("EPH_usu_{trimestre}to_Trim_{anio}_txt/usu_hogar_T{trimestre}{substr(anio,3,4)}.txt")), sep = ";", dec = ",", header = T)
  


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



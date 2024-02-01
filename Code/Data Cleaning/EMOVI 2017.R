# Libraries 
library(tidyverse)
library(haven)

# EMOVI 2017----
## Hogares
hog <- read_dta("./Data/EMOVI/ESRU-EMOVI 2017 Hogar.dta")

hog_ <- hog %>% mutate(
  ocupados  = ifelse(p15h == 1, 1, 0),
  hombres   = ifelse(p06h == 1, 1, 0)
) %>% group_by(folio) %>%
  summarise(
    tot_integ = n(),
    hombres   = sum(hombres),
    ocupados  = sum(ocupados, na.rm = T)
  )

## Household Head
hhd <- hog %>% filter(p07h == 1)
hhd <- hhd %>% transmute(
  folio, 
  
  # Social Security
  Ainst_1 = ifelse(p10_1h == 1 | (p09_1h == 1 & p10_1h == 4) |
                    (p09_2h == 1 & p10_1h == 4)  |
                    (p09_3h == 1 & p10_1h == 4), 1, 0),
  Ainst_2 = ifelse(p10_1h==2 | (p09_1h == 2 & p10_1h == 4) |
                    (p09_2h == 2 & p10_1h == 4)  | 
                    (p09_3h == 2 & p10_1h == 4), 1, 0),
  Ainst_3 = ifelse(p10_1h == 3 | (p09_1h == 3 & p10_1h == 4) |
                    (p09_2h == 3 & p10_1h == 4)  |
                    (p09_3h == 3 & p10_1h == 4), 1, 0),
  Ainst_4 = ifelse(p10_1h == 5 | (p09_1h == 4 & p10_1h == 4) |
                    (p09_2h == 4 & p10_1h == 4)  |
                    (p09_3h == 4 & p10_1h == 4), 1, 0),
  Ainst_5 = ifelse(p10_1h == 5 | p10_1h == 4, 1, 0),
  
  # Education 
  no_edu       = ifelse(p13h == 97, 1, 0),
  kinder       = ifelse(p13h == 1, 1, 0),
  primaria     = ifelse(p13h == 2, 1, 0),
  secundaria   = ifelse(p13h == 3 | p13h == 4, 1, 0),
  preparatoria = ifelse(p13h == 5 | p13h == 6, 1, 0),
  licenciatura = ifelse(p13h == 10 | p13h == 11, 1, 0),
  postgrado    = ifelse(p13h == 12, 1, 0),
  
  # gender
  bin_sex = ifelse(p06h == 2, 1, 0)
) 

## Merge 
df <- read_dta("./Data/EMOVI/ESRU-EMOVI 2017 Entrevistado.dta")
df <- df %>% full_join(hog_) %>% full_join(hhd)

df <- df %>% transmute(
  folio, 
  
  # Income
  ginc_1_2 = ifelse(p133 == 1 | p133 == 2, 1, 0),
  ginc_3 = ifelse(p133 == 3, 1, 0),
  ginc_4 = ifelse(p133 == 4, 1, 0),
  ginc_5 = ifelse(p133 == 5, 1, 0),
  ginc_6 = ifelse(p133 == 6, 1, 0),
  ginc_7 = ifelse(p133 == 7, 1, 0),
  
  # Assets
  bin_telefono     = ifelse(p126k == 2, 0, 1),
  bin_celular      = ifelse(p126l == 2, 0, 1),
  bin_tvpaga       = ifelse(p126j == 2, 0, 1),
  bin_conex_inte   = ifelse(p126m == 2, 0, 1),
  bin_agua         = ifelse(p125a == 2, 0, 1),
  bin_electricidad = ifelse(p126b == 2, 0, 1),
  coche            = p131,
  
  # Tenencia de la propiedad
  propiedad = ifelse(p123 == 1, 1, 0),
  
  # Piso
  piso_tierra  = ifelse(p120 == 1, 1, 0),
  piso_cemento = ifelse(p120 == 2, 1, 0),
  piso_otro    = ifelse(p120 == 3, 1, 0),
  
  # Demograficos
  bin_sex,
  share_h    = hombres/tot_integ,
  share_ocup = ocupados/tot_integ,
  hablaind   = ifelse(p39 == 1 | p39m == 1, 1, 0),
  
  # Seguridad Social
  inst = ifelse(Ainst_1 == 1, "IMSS", 
                ifelse(Ainst_2 == 1, "ISSSTE", 
                       ifelse(Ainst_3 == 1, "PEMEX", 
                              ifelse(Ainst_4 == 1, "IMSS Prospera", "Otro")
                       )
                )
  ),
  
  # Prospera y Adultos mayores
  prospera = ifelse(p130a == 1, 1, 0), 
  adultom  = ifelse(p130b == 1, 1, 0), 
  
  # Rural Urbano
  rururb     = ifelse(rururb == 1, 1, 0),
  tam_grande = ifelse(p24 == 1, 1, 0),
  tam_med    = ifelse(p24 == 2, 1, 0),
  tam_medpeq = ifelse(p24 == 3, 1, 0),
  tam_peq    = ifelse(p24 == 4 | p24 == 5, 1, 0),
  
  # Region
  ent    = substr(folio_ageb, 1, 2),
  mun    = substr(folio_ageb, 1, 5),
  
  region = ifelse(ent == "02" | ent == "08" | ent == "05" | ent == "19" |
                    ent == "28" | ent == "26", 1, 
                  ifelse(ent == "32" | ent == "25" | ent == "10" |
                           ent == "18" | ent == "03", 2, 
                         ifelse(ent == "16" | ent == "06" | ent == "14" |
                                  ent == "01" | ent == "24", 3,
                                ifelse(ent == 13 | ent == "15" | ent == "17" |
                                         ent == "29" | ent == "21" | ent == "11" |
                                         ent == "22", 4, 
                                       ifelse(ent == "09", 5, 6)
                                )
                         )
                  )
  ),
  
  # Educacion
  no_edu,       
  kinder,       
  primaria,     
  secundaria,   
  preparatoria, 
  licenciatura, 
  postgrado,  
  
  factor   = factor,
  upm      = upm
)

write.csv(df, "./Data/EMOVI17.csv")
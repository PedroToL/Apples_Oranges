# Libraries 
library(tidyverse)
library(foreign)

# ENIGH 2018 ----
## Viviendas 
viv <- read.dbf("./Data/ENIGH/2018/viviendas.dbf")

viv <- viv %>% select(
  folioviv,
  mat_pisos, cuart_dorm, num_cuarto, disp_agua, dotac_agua, disp_elect,
  excusado, tenencia,
  upm, factor
)

## Hograes
hog <- read.dbf("./Data/ENIGH/2018/hogares.dbf")

hog <- hog %>% select(
  telefono, celular, tv_paga, conex_inte, num_auto, num_van, num_pickup,
  num_tvd, num_dvd, num_tosta, num_micro, num_refri, num_estuf, num_lavad,
  num_aspir, folioviv, foliohog 
)

## Concentrado Hogar
conc <- read.dbf("./Data/ENIGH/2018/concentradohogar.dbf")

conc <- conc %>% select(
  foliohog, folioviv, tam_loc, factor, sexo_jefe, edad_jefe, educa_jefe, 
  tot_integ, hombres, ocupados, ing_cor, upm, ubica_geo, clase_hog, tam_loc,
  menores
)

## Población 
pob  <- read.dbf("./Data/ENIGH/2018/poblacion.dbf")

pob1 <- pob %>% filter(numren == "01") %>%
  select(
    folioviv, foliohog, hablaind, atemed, inst_1, inst_2, inst_3, inst_4, 
    inst_5, inst_6
  )

pob2 <- pob %>% mutate(
  pobemovi = ifelse(edad >= 25 & edad <= 64, 1, 0)
) %>% group_by(folioviv, foliohog) %>%
  summarise(
    pobemovi = sum(pobemovi)
  ) %>%
  mutate(
    pobemovi = ifelse(pobemovi > 0, 1, 0)
  )

## Ingresos 
ing <- read.dbf("./Data/ENIGH/2018/ingresos.dbf")

ing <- ing %>% mutate(
  prospera = ifelse(clave == "P042", 1, 0),
  adultom  = ifelse(clave == "P044", 1, 0)
) %>% group_by(folioviv, foliohog) %>%
  summarise(
    prospera = sum(prospera),
    adultom  = sum(adultom)   
  ) %>% mutate(
    prospera = ifelse(prospera > 0, 1, 0),
    adultom  = ifelse(adultom > 0, 1, 0)
  )

## Join 
df <- viv %>% full_join(hog, by = "folioviv") %>%
  full_join(conc, by = c("folioviv", "foliohog")) %>%
  full_join(pob1, by = c("folioviv", "foliohog")) %>%
  full_join(ing, by = c("folioviv", "foliohog")) %>%
  full_join(pob2, by = c("folioviv", "foliohog"))

df <- df %>% transmute(
  folio = paste0(folioviv, foliohog),
  
  # Ingresos
  ing_cor = ing_cor,
  ingc_pc = ing_cor/tot_integ,
  
  ginc_1_2 = ifelse(ing_cor >= 0 & ing_cor <= 2400, 1, 0),
  ginc_3   = ifelse(ing_cor >= 2401 & ing_cor <= 4800, 1, 0),
  ginc_4   = ifelse(ing_cor >= 4801 & ing_cor <= 7200, 1, 0),
  ginc_5   = ifelse(ing_cor >= 7201 & ing_cor <= 12000, 1, 0),
  ginc_6   = ifelse(ing_cor >= 12001 & ing_cor <= 24000, 1, 0),
  ginc_7   = ifelse(ing_cor > 24000, 1, 0),
  
  lincome = log(ingc_pc),
  
  # Assets
  bin_telefono     = ifelse(telefono   == "1", 1, 0),
  bin_celular      = ifelse(celular    == "1", 1, 0),
  bin_tvpaga       = ifelse(tv_paga    == "1", 1, 0),
  bin_conex_inte   = ifelse(conex_inte == "1", 1, 0),
  bin_agua         = ifelse(disp_agua  == "1", 1, 0),
  bin_electricidad = ifelse(disp_elect == "1", 1, 0),
  coche            = ifelse(num_auto > 0 | num_van > 0  |
                              num_pickup > 0, 1, 0),
  
  # Tenencia de la propiedad
  propiedad = ifelse(tenencia == "3" | tenencia == "4", 1, 0),
  
  # Piso
  piso_tierra  = ifelse(mat_pisos == "1", 1, 0),
  piso_cemento = ifelse(mat_pisos == "2", 1, 0),
  piso_otro    = ifelse(mat_pisos == "3", 1, 0),
  
  # Tipo de hogar
  hoga_uni  = ifelse(clase_hog == "1", 1, 0),
  hoga_nuc  = ifelse(clase_hog == "2", 1, 0),
  hoga_amp  = ifelse(clase_hog == "3", 1, 0),
  hoga_comp = ifelse(clase_hog == "4", 1, 0),
  hoga_corr = ifelse(clase_hog == "5", 1, 0),
  
  # Demograficos
  bin_sex    = ifelse(sexo_jefe == "1", 1, 0),
  share_h    = hombres/tot_integ,
  share_ocup = ocupados/tot_integ,
  hablaind = ifelse(hablaind == 2, 0, hablaind),
  
  # Seguridad Social
  Ainst_2 = ifelse(inst_3 == 3, 2, inst_2),
  Ainst_3 = inst_4,
  Ainst_4 = inst_5,
  Ainst_5 = inst_6,
  
  inst = ifelse(!is.na(inst_1), "IMSS", 
                ifelse(!is.na(Ainst_2), "ISSSTE", 
                       ifelse(!is.na(Ainst_3), "PEMEX", 
                              ifelse(!is.na(Ainst_4), "IMSS Prospera", "Otro")
                              )
                       )
                ),
  
  # Prospera y Adultos mayores
  prospera = ifelse(!is.na(prospera), prospera, 0), 
  adultom  = ifelse(!is.na(adultom), adultom, 0), 
  
  # Rural Urbano
  rururb     = ifelse(tam_loc == "4", 1, 0),
  tam_grande = ifelse(tam_loc == "1", 1, 0),
  tam_med    = ifelse(tam_loc == "2", 1, 0),
  tam_medpeq = ifelse(tam_loc == "3", 1, 0),
  tam_peq    = ifelse(tam_loc == "4", 1, 0),
  
  # Region
  ent    = substr(ubica_geo, 1, 2),
  mun    = ubica_geo,
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
  no_edu       = ifelse(educa_jefe == "01", 1, 0),
  kinder       = ifelse(educa_jefe == "02" | educa_jefe == "03", 1, 0),
  primaria     = ifelse(educa_jefe == "04" | educa_jefe == "05", 1, 0),
  secundaria   = ifelse(educa_jefe == "06" | educa_jefe == "07", 1, 0),
  preparatoria = ifelse(educa_jefe == "08" | educa_jefe == "09", 1, 0),
  licenciatura = ifelse(educa_jefe == "10", 1, 0),
  postgrado    = ifelse(educa_jefe == "11", 1, 0),
  
  # survey Design
  pobemovi = pobemovi,
  factor   = factor.x,
  upm      = upm.x
  
) %>% filter(pobemovi == 1)

write.csv(df, "./Data/ENIGH18.csv")

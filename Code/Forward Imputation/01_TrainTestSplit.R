# Libraries ----
library(tidyverse)
library(fastDummies)

# Data ----
enigh18 = read_csv("./Data/ENIGH18.csv") %>%
    filter(ing_cor > 0) %>%
    mutate(
      folio = paste0(folio, "-")
    )
enigh16 = read_csv("./Data/ENIGH16.csv") %>%
    filter(ing_cor > 0) %>%
    filter(ingc_pc < 4000000) %>%
    mutate(
      folio = paste0(folio, "-")
    )
emovi17 = read_csv("./Data/EMOVI17.csv") %>%
    mutate(
      folio = paste0(folio, "-")
    )

# Transformaciones ----
lpob_18_r = 1145.5   # Linea de pobreza Rural Julio 2018
lpob_18_u = 1521.44  # Linea de pobreza Urbana Julio 2018

lpob_16_r = 1015.44  # Linea de pobreza Rural Julio 2016
lpob_16_u = 1348.81  # Linea de pobreza Urbana Julio 2016

lpob_17_r = 1120.08  # Linea de pobreza Rural Julio 2017
lpob_17_u = 1471.60  # Linea de pobreza Urbana Julio 2017

enigh18 = enigh18 %>% mutate(
  r_ingc_pc = ifelse(rururb == 1, ingc_pc/lpob_18_r, ingc_pc/lpob_18_u),
  y         = log(r_ingc_pc)
)

write.csv(enigh18, "./Data/ENIGH18.csv", row.names = FALSE)

enigh16 = enigh16 %>% mutate(
  r_ingc_pc = ifelse(rururb == 1, ingc_pc/lpob_16_r, ingc_pc/lpob_16_u),
  y         = log(r_ingc_pc)
)

write.csv(enigh16, "./Data/ENIGH16.csv", row.names = FALSE)

## Dummies
enigh18 = dummy_cols(enigh18, select_columns = 'ent')
enigh18 = dummy_cols(enigh18, select_columns = 'inst')
enigh16 = dummy_cols(enigh16, select_columns = 'ent')
enigh16 = dummy_cols(enigh16, select_columns = 'inst')
emovi17 = dummy_cols(emovi17, select_columns = 'ent')
emovi17 = dummy_cols(emovi17, select_columns = 'inst')

# Train-Test Splitting 
train = enigh16 %>% select(
  y,
  starts_with("ginc"),
  starts_with("bin_"), coche, 
  propiedad, 
  starts_with("piso"),
  starts_with("share"),
  starts_with("inst_"),
  hablaind, 
  prospera, 
  adultom,
  starts_with("tam"), 
  no_edu, kinder, primaria, secundaria, preparatoria, licenciatura, 
  postgrado,
  starts_with("ent_"),
  starts_with("region_"),
  folio
) 

write.csv(train, "./Data/Forward/train.csv", row.names = FALSE)

test = enigh18 %>% select(
  y,
  starts_with("ginc"),
  starts_with("bin_"), coche, 
  propiedad, 
  starts_with("piso"),
  starts_with("share"),
  starts_with("inst_"),
  hablaind, 
  prospera, 
  adultom,
  starts_with("tam"), 
  no_edu, kinder, primaria, secundaria, preparatoria, licenciatura, 
  postgrado,
  starts_with("ent_"),
  starts_with("region_"),
  folio
)

write.csv(test, "./Data/Forward/test.csv", row.names = FALSE)

emovi <- emovi17 %>% select(
  starts_with("ginc"),
  starts_with("bin_"), coche, 
  propiedad, 
  starts_with("piso"),
  starts_with("share"),
  starts_with("inst_"),
  hablaind, 
  prospera, 
  adultom,
  starts_with("tam"), 
  no_edu, kinder, primaria, secundaria, preparatoria, licenciatura, 
  postgrado,
  starts_with("ent_"),
  starts_with("region_"),
  folio, 
  -inst_NA)

write.csv(emovi, "./Data/Forward/emovi.csv", row.names = FALSE)
# Libraries ----
library(tidyverse)
library(haven)
library(fastDummies)
library(psych)

# Data ----
emovi <- read_dta("./Data/EMOVI/ESRU-EMOVI 2017 Entrevistado.dta")

individual <- emovi %>% transmute(
  # Assets and Services in the household
  plumbing          = p125a,
  stove             = p126a,
  electricity       = p125b,
  tv                = p126e,
  fridge            = p126c,
  washing_machine   = p126b,
  landline          = p126k,
  dvd_bluray        = p126h,
  microwave         = p126d,
  cable_tv          = p126j,
  internet          = p126m,
  cellphone         = p126i,
  computer          = p126o,
  
  # Assets from Residents 
  other_housing     = p129a,
  other_land        = p129e,
  automobile        = ifelse(p131 > 1, 1, 0),
  bank_account      = p128c,
  credit_card       = p128d,
  
  # Agricultural Activities
  premises          = p129b,
  working_parcels   = p129c,
  working_machinery = p126r,
  working_animals   = p126p,
  livestock         = p126q
)

individual[individual == 2]   <- 0  # Replace 2 = 0
individual[individual == 8]   <- NA # Replace 9 = NA
individual[is.na(individual)] <- 0  # Replace NA = 0

# PCA ----
poly <- polychoric(individual)

pca  <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)

loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings.csv")

if (pca$rotation[1, 1] < 0) {
  emovi$index <- predict(pca, newdata = individual)[, 1]*-1
} else {
  emovi$index <- predict(pca, newdata = individual)[, 1]
}

reg <- lm(index~poly(p05, 2), data = emovi)

emovi$index_age <- predict(reg) # Get life-cycle Bias

emovi$index <- (emovi$index - emovi$index_age)   # Correct life-cycle Bias

emovi$ent <- ifelse(as.double(emovi$p23) < 32, emovi$p23, "Other")

df <- emovi %>% transmute(
    folio, 
    index,
    sex = ifelse(p06 == 2, 1, 0),
    ethnicity = ifelse(p39 == 1 | p39m == 1, 1, 0),
        birth_region = ifelse(ent == "02" | ent == "08" | ent == "05" | ent == "19" |
                    ent == "28" | ent == "26", 1, 
                  ifelse(ent == "32" | ent == "25" | ent == "10" |
                           ent == "18" | ent == "03", 2, 
                         ifelse(ent == "16" | ent == "06" | ent == "14" |
                                  ent == "01" | ent == "24", 3,
                                ifelse(ent == 13 | ent == "15" | ent == "17" |
                                         ent == "29" | ent == "21" | ent == "11" |
                                         ent == "22", 4, 
                                       ifelse(ent == "09", 5, 
                                              ifelse(ent == "Other", NA, 6)
                                              )
                                )
                         )
                  )
    ),
    father_educ = ifelse(p42 == 2 | p42m == 8, 0,       # Years of schooling father 
                        ifelse(p43 == 1 | p43 == 98, 0,
                               ifelse(p43 == 2, p44,
                                      ifelse(p43 == 3 | p43 == 4 | p43 == 7, p44 + 6,
                                             ifelse(p43 == 5 | p43 == 6 | p43 == 8, p44 + 9,
                                                    ifelse(p43 == 9 | p43 == 10 | p43 == 11, p44 + 12,
                                                           ifelse(p43 == 12, p44 + 16 , 0)
                                                    )
                                             )
                                      )
                               )
                        )
    ),
    mother_educ = ifelse(p42m == 2 | p42m == 8, 0,      # Years of schooling mother 
                        ifelse(p43m == 1 | p43m == 98, 0,
                               ifelse(p43m == 2, p44m,
                                      ifelse(p43m == 3 | p43m == 4 | p43m == 7, p44m + 6,
                                             ifelse(p43m == 5 | p43m == 6 | p43m == 8, p44m + 9,
                                                    ifelse(p43m == 9 | p43m == 10 | p43m == 11, p44m + 12,
                                                           ifelse(p43m == 12, p44m + 16 , 0)
                                                    )
                                             )
                                      )
                               )
                        )
    ),
    father_ocup = substr(SINCO1, 1, 2),
  father_ocup = ifelse(father_ocup == 99, father_ocup, paste0(0, substr(father_ocup, 1, 1))),
  father_ocup = ifelse(p46 == 3, "NT", father_ocup),
  father_ocup = factor(father_ocup)
)

df$father_educ <- replace(df$father_educ, is.na(df$father_educ), 0)
df$mother_educ <- replace(df$mother_educ, is.na(df$mother_educ), 0)

df$father_ocup <- replace(df$father_ocup, is.na(df$father_ocup), "NT")

write.csv(df, "./Data/Example/circumstances.csv")

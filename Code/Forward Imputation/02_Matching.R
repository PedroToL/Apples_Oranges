# Libraries ----
library(MatchIt)
library(tidyverse)
library(dineq)
library(fastDummies)

# Data ----
emovi = read_csv("./Data/Forward/emovi.csv")
emovi$D <- TRUE
emovi = emovi %>%
    dplyr::select(-inst_NA)

enigh = read_csv("./Data/Forward/test.csv")
enigh = enigh %>%
    mutate(D = FALSE) %>%
    dplyr::select(colnames(emovi))

enigh = enigh[sample(nrow(enigh), 0.25*nrow(enigh)), ]

df <- rbind(emovi, enigh)
df[is.na(df)] <- 0

# Matching
formula <- paste(colnames(emovi), collapse =  " + ")
formula <- paste("D", formula, sep = " ~ ")

gc()

set.seed(123)
muestra <- matchit(D~., data = df, method = "nearest")
muestra <- match.data(
  muestra,
  group             = "control",
  distance          = "distance",
  weights           = "weights",
  subclass          = "subclass",
  include.s.weights = TRUE,
  data              = NULL,
  drop.unmatched    = TRUE
)

df <- muestra %>% select(-c(D, distance, weights, subclass))
df <- df %>% left_join(enigh, relationship = "many-to-many") %>% 
  select(y,
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
         rururb, 
         ent,
         region
         )

write.csv(df, "./Data/Forward/emovi_replica.csv")

lpob_18_r = 1145.5   # Linea de pobreza Rural Julio 2018
lpob_18_u = 1521.44  # Linea de pobreza Urbana Julio 2018

## Original 
df <- df %>% mutate(
  y = ifelse(rururb == 1, exp(y)*lpob_18_r, exp(y)*lpob_18_u)
)

gini <- NULL
set.seed(123)
for (i in 1:1000){
  df_ <- sample_n(df, 17500, replace = T)
  gini[i] <- gini.wtd(df_$y)
}
LB <- mean(gini) - 1.96*sd(gini)
UB <- mean(gini) + 1.96*sd(gini)
print(paste0("[", round(LB,4), "; ", round(mean(gini), 4), "; ", round(UB,4), "]"))

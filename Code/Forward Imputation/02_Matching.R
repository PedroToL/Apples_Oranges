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

df <- rbind(emovi, enigh)
df[is.na(df)] <- 0
df <- df[sample(nrow(df)), ]

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


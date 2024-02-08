# Libraries ----
library(tidyverse)

# Functions ----
correction <- function(data){
    if (per == "FWD") {
        lpob_r = lpob_18_r
        lpob_u = lpob_18_u
    } else if (per == "BWD") {
        lpob_r = lpob_16_r
        lpob_u = lpob_16_u
    }
 # Linea de pobreza Urbana Julio 2018
    data <- data %>% group_by(ent) %>%
        mutate(
            rank = ntile(y_hat, 100)
        ) %>% ungroup()
    
    data <- data %>% full_join(ratios, by = c("ent", "rank"))

    data <- data %>% select(y_hat, ent, rank, ratio, rururb) %>% 
        na.omit()

    data <- data %>% mutate(
        y_hat = exp(y_hat)*ratio,
        y_hat = ifelse(rururb == 1, y_hat*lpob_r, y_hat*lpob_u)
    )

    return(data)
}

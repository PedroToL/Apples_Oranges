# Libraries ----
library(tidyverse)
library(dineq)

# Data ----
target = read_csv("./Data/EMOVI17.csv")
y_hat  = read_csv("./Data/Forward/OLS_emovi.csv")
c      = read_csv("./Data/Example/circumstances.csv")

target <- target %>% select(region, rururb)
c <- c %>% select(index, sex, ethnicity, birth_region,
                  father_educ, mother_educ, father_ocup)

target$y_hat <- y_hat$`0`
target <- cbind(target, c) 
target <- target %>% filter(y_hat < 1000)

target <- target %>% group_by(region) %>%
  mutate(
    rank = ntile(y_hat, 100)
)

ratios <- read_csv("./Data/Example/ratios.csv")
target <- target %>% full_join(ratios) 

lpob_17_r = 1120.08  # Linea de pobreza Rural Julio 2017
lpob_17_u = 1471.60  # Linea de pobreza Urbana Julio 2017

target <- target %>% mutate(
  y = ifelse(rururb == 1, exp(y_hat)*lpob_17_r, exp(y_hat)*lpob_17_u)
)

gini.wtd(target$y) # 0.351

# Correction ----
target <- target %>% mutate(
  ratio = ratio,
  y_hat_BC_WC = exp(y_hat)*ratio,
  y_hat_BC_WC = ifelse(rururb == 1, y_hat_BC_WC*lpob_17_r, y_hat_BC_WC*lpob_17_u)
)

gini.wtd(target$y_hat_BC_WC) # 0.482

# Distribution ----
ggplot(target) + aes(x = log(y_hat_BC_WC)) + 
    geom_density(aes(fill = "A"), alpha = 0.75) +
    xlab("log HHI") +
    ylab("Density") +
    labs(fill = "") + 
    ggthemes::theme_clean() +
  theme(
    axis.line.y       = element_blank(),
    legend.background = element_blank(),
    plot.background   = element_blank(),
    axis.text = element_text(size = 15), 
    axis.title = element_text(size = 18),
    legend.position = "none"
  )

ggsave(
    filename = "./Figures/Example/Density.png",
    width = 3000,
    height = 1500,
    units = "px"
)

write.csv(target, "./Data/Example/final.csv")

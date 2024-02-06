# Libraries ----
library(tidyverse)
library(dineq)

# Data ----
target = read_csv("./Data/Forward/emovi_replica.csv")
y_hat  = read_csv("./Data/Forward/OLS_emovi.csv")

target$y_hat <- y_hat$`0`

target <- target %>% group_by(ent) %>%
  mutate(
    rank = ntile(y_hat, 100)
) %>% ungroup() %>%
  filter(y_hat < 1000)

ratios <- read_csv("./Data/Forward/ratios.csv")
target <- target %>% full_join(ratios)

lpob_18_r = 1145.5   # Linea de pobreza Rural Julio 2018
lpob_18_u = 1521.44  # Linea de pobreza Urbana Julio 2018

target$ingc_pc <- ifelse(target$rururb == 1, exp(target$y)*lpob_18_r, exp(target$y)*lpob_18_u)

gini.wtd(target$ingc_pc) # 0.545

# Original ----
target <- target %>% mutate(
  y_hat_0 = ifelse(rururb == 1, exp(y_hat)*lpob_18_r, exp(y_hat)*lpob_18_u)
)
target$SE_0 <- (target$ingc_pc - target$y_hat_0)^2 
sqrt(mean(target$SE_0, na.rm = T)) # 28,552

gini.wtd(target$y_hat_0) # 0.357

# Between Cluster ----
target <- target %>% mutate(
  y_hat_BC = exp(y_hat)*ratio_BC,
  y_hat_BC = ifelse(rururb == 1, y_hat_BC*lpob_18_r, y_hat_BC*lpob_18_u)
)

target$SE_BC <- (target$ingc_pc - target$y_hat_BC)^2
sqrt(mean(target$SE_BC, na.rm = T)) # 27,783

gini.wtd(target$y_hat_BC) # 0.363

# Within Cluster ----
target <- target %>% mutate(
  y_hat_WC = exp(y_hat)*ratio_WC,
  y_hat_WC = ifelse(rururb == 1, y_hat_WC*lpob_18_r, y_hat_WC*lpob_18_u)
)

target$SE_WC <- (target$ingc_pc - target$y_hat_WC)^2
sqrt(mean(target$SE_WC, na.rm = T)) # 39,688
 
gini.wtd(target$y_hat_WC) # 0.553

# Mean ----
target <- target %>% mutate(
  ratio = ratio,
  y_hat_BC_WC = exp(y_hat)*ratio,
  y_hat_BC_WC = ifelse(rururb == 1, y_hat_BC_WC*lpob_18_r, y_hat_BC_WC*lpob_18_u)
)

target$SE_BC_WC <- (target$ingc_pc - target$y_hat_BC_WC)^2
sqrt(mean(target$SE_BC_WC, na.rm = T)) # 23.608

gini.wtd(target$y_hat_BC_WC) # 0.477

# Errors ----
target$y_hat_1 <- log(exp(target$y_hat)*target$ratio)

ggplot(target) + 
  geom_density(aes(x = log(ingc_pc), 
                   fill = "Original"), alpha = 0.5) +
  geom_density(aes(x = log(y_hat_0), 
                   fill = "Prediction"), alpha = 0.5) +
  geom_density(aes(x = log(y_hat_BC_WC),
                   fill = "Correction"), alpha = 0.5) +
  ylab("Density") +
  xlab("Log HHI") +
  labs(
    fill = ""
  ) +
  ggthemes::theme_clean() +
  theme(
    axis.line.y     = element_blank(),
    axis.line.x     = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.position = "top"
  )

ggsave(
    filename = "./Figures/Results/Forward/Densities_Target_Robust_XGB.png",
    width = 3000,
    height = 1500,
    units = "px"
)

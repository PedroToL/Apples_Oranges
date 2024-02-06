# Libraries ----
library(tidyverse)
library(dineq)

# Data ----
target = read_csv("./Data/ENIGH18.csv")
y_hat  = read_csv("./Data/Forward/XGB_test.csv")

target <- target %>% left_join(y_hat, by = "folio")
target$y_hat <- target$`0`

target <- target %>% group_by(ent) %>%
  mutate(
    rank = ntile(y_hat, 100)
)

ratios <- read_csv("./Data/Forward/ratios.csv")
ratios$ent <- as.double(ratios$ent)
target <- target %>% full_join(ratios)

gini.wtd(target$ingc_pc) # 0.463

lpob_18_r = 1145.5   # Linea de pobreza Rural Julio 2018
lpob_18_u = 1521.44  # Linea de pobreza Urbana Julio 2018

# Original ----
target <- target %>% mutate(
  y_hat_0 = ifelse(rururb == 1, exp(y_hat)*lpob_18_r, exp(y_hat)*lpob_18_u)
)
target$SE_0 <- (target$ingc_pc - target$y_hat_0)^2 
sqrt(mean(target$SE_0, na.rm = T)) # 19,452

gini.wtd(target$y_hat_0) # 0.35

# Between Cluster ----
target <- target %>% mutate(
  y_hat_BC = exp(y_hat)*ratio_BC,
  y_hat_BC = ifelse(rururb == 1, y_hat_BC*lpob_18_r, y_hat_BC*lpob_18_u)
)

target$SE_BC <- (target$ingc_pc - target$y_hat_BC)^2
sqrt(mean(target$SE_BC, na.rm = T)) # 19,149

gini.wtd(target$y_hat_BC) # 0.36

# Within Cluster ----
target <- target %>% mutate(
  y_hat_WC = exp(y_hat)*ratio_WC,
  y_hat_WC = ifelse(rururb == 1, y_hat_WC*lpob_18_r, y_hat_WC*lpob_18_u)
)

target$SE_WC <- (target$ingc_pc - target$y_hat_WC)^2
sqrt(mean(target$SE_WC, na.rm = T)) # 29,807
 
gini.wtd(target$y_hat_WC) # 0.55

# Mean ----
target <- target %>% mutate(
  ratio = ratio,
  y_hat_BC_WC = exp(y_hat)*ratio,
  y_hat_BC_WC = ifelse(rururb == 1, y_hat_BC_WC*lpob_18_r, y_hat_BC_WC*lpob_18_u)
)

target$SE_BC_WC <- (target$ingc_pc - target$y_hat_BC_WC)^2
sqrt(mean(target$SE_BC_WC, na.rm = T)) # 23.161

gini.wtd(target$y_hat_BC_WC) # 0.468

# Errors ----
target$y_hat_1 <- log(exp(target$y_hat)*target$ratio)

target$error <- target$y - target$y_hat
target$error1 <- target$y - target$y_hat_1

ggplot(target) + aes(error) + 
  geom_histogram(
    aes(y = ..density..),
    fill    = "#112446", 
    color   = "white", 
    alpha   = 0.75,
    bins    = 100,
    density = T
    ) +
  xlim(c(-2, 2)) +
  geom_density() +
  xlab("") +
  ylab("Density") +
  ggthemes::theme_clean() +
  theme(
    axis.line.y     = element_blank(),
    plot.background = element_blank()
  )

ggsave(
    filename = "./Figures/Results/Forward/Error_Target_Original_XGB.png",
    width = 3000,
    height = 1500,
    units = "px"
)

ggplot(target) + aes(error1) + 
  geom_histogram(
    aes(y = ..density..),
    fill    = "#112446", 
    color   = "white", 
    alpha   = 0.75,
    bins    = 100,
    density = T
    ) +
  xlim(c(-2, 2)) +
  geom_density() +
  xlab("") +
  ylab("Density") +
  ggthemes::theme_clean() +
  theme(
    axis.line.y     = element_blank(),
    plot.background = element_blank()
  )

ggsave(
    filename = "./Figures/Results/Forward/Error_Correction_Test_XGB.png",
    width = 3000,
    height = 1500,
    units = "px"
)

ggplot(target) + 
  geom_density(aes(x = log(ingc_pc), 
                   fill = "Original"), alpha = 0.5) +
  geom_density(aes(x = log(y_hat_0), 
                   fill = "Prediction"), alpha = 0.5) +
  geom_density(aes(x = log(y_hat_BC_WC),
                   fill = "Correction"), alpha = 0.5,
                   adjust = 1.5) +
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
    filename = "./Figures/Results/Forward/Densities_Target_XGB.png",
    width = 3000,
    height = 1500,
    units = "px"
)

ggplot(target) + aes(y = y) + 
  geom_point(aes(x = y_hat, color = "Original"),
              alpha = 0.25) +
  geom_point(aes(x = y_hat_1, color = "Corrected"),
              alpha = 0.25) +
  geom_smooth(aes(x = y_hat), color = "blue",
              se = F, method = "lm") +
  geom_smooth(aes(x = y_hat_1), color = "red",
              se = F, method = "lm") +
  xlab("Predicted") +
  ylab("Original") + 
  labs(color = "") +
  ggthemes::theme_clean() +
  theme(
    axis.line.y = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(), 
    legend.position = "top"
  )

ggsave(
    filename = "./Figures/Results/Forward/Correlation_Test_XGB.png",
    width = 3000,
    height = 1500,
    units = "px"
)

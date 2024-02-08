# Libraries ----
library(tidyverse)
library(dineq)

# Data ----
target = read_csv("./Data/ENIGH16.csv")
y_hat  = read_csv("./Data/Backward/OLS_test.csv")

target <- target %>% left_join(y_hat, by = "folio")
target$y_hat <- target$`0`

target <- target %>% group_by(ent) %>%
  mutate(
    rank = ntile(y_hat, 100)
) %>% ungroup() %>%
  filter(y_hat < 1000)

target$ent <- as.double(target$ent)

ratios <- read_csv("./Data/Backward/ratios.csv")
target <- target %>% full_join(ratios)

gini.wtd(target$ingc_pc) # 0.472

lpob_16_r = 1015.44  # Linea de pobreza Rural Julio 2016
lpob_16_u = 1348.81  # Linea de pobreza Urbana Julio 2016

# Original ----
target <- target %>% mutate(
  y_hat_0 = ifelse(rururb == 1, exp(y_hat)*lpob_16_r, exp(y_hat)*lpob_16_u)
)
target$SE_0 <- (target$ingc_pc - target$y_hat_0)^2 
sqrt(mean(target$SE_0, na.rm = T)) # 21,250

gini.wtd(target$y_hat_0) # 0.337

# Between Cluster ----
target <- target %>% mutate(
  y_hat_BC = exp(y_hat)*ratio_BC,
  y_hat_BC = ifelse(rururb == 1, y_hat_BC*lpob_16_r, y_hat_BC*lpob_16_u)
)

target$SE_BC <- (target$ingc_pc - target$y_hat_BC)^2
sqrt(mean(target$SE_BC, na.rm = T)) # 20,935

gini.wtd(target$y_hat_BC) # 0.341

# Within Cluster ----
target <- target %>% mutate(
  y_hat_WC = exp(y_hat)*ratio_WC,
  y_hat_WC = ifelse(rururb == 1, y_hat_WC*lpob_16_r, y_hat_WC*lpob_16_u)
)

target$SE_WC <- (target$ingc_pc - target$y_hat_WC)^2
sqrt(mean(target$SE_WC, na.rm = T)) # 28,773
 
gini.wtd(target$y_hat_WC) # 0.551

# Mean ----
target <- target %>% mutate(
  ratio = ratio,
  y_hat_BC_WC = exp(y_hat)*ratio,
  y_hat_BC_WC = ifelse(rururb == 1, y_hat_BC_WC*lpob_16_r, y_hat_BC_WC*lpob_16_u)
)

target$SE_BC_WC <- (target$ingc_pc - target$y_hat_BC_WC)^2
sqrt(mean(target$SE_BC_WC, na.rm = T)) # 23.608

gini.wtd(target$y_hat_BC_WC) # 0.467

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
    plot.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15)
  )

ggsave(
    filename = "./Figures/Results/Backward/Error_Target_Original.png",
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
    plot.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15)
  )

ggsave(
    filename = "./Figures/Results/Backward/Error_Correction_Test.png",
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
    legend.position = "top",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15)
  )

ggsave(
    filename = "./Figures/Results/Backward/Densities_Target.png",
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
    legend.position = "top",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15)
  )

ggsave(
    filename = "./Figures/Results/Backward/Correlation_Test.png",
    width = 3000,
    height = 1500,
    units = "px"
)

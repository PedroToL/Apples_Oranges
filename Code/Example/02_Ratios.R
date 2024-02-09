# Libraries ----
library(tidyverse)
library(dineq)

# Data ----
source = read_csv("./Data/ENIGH16.csv")
y_hat  = read_csv("./Data/Forward/OLS_train.csv")

source       <- source %>% left_join(y_hat, by = "folio")
source$y_hat <- source$`0`
source$error <- source$y - source$y_hat

MLmetrics::R2_Score(source$y_hat, source$y) # 0.503

# Ratios ----
source <- source %>% group_by(region) %>%
  mutate(
    rank = ntile(exp(y), 100)
  ) %>% ungroup()

set.seed(123)
for (i in 1:100){
  df <- sample_n(source, nrow(source), replace = T)
  ratio_BC_ <- df %>% group_by(region) %>%
    summarise(
      income     = mean(exp(y)), 
      income_hat = mean(exp(y_hat)), 
      ratio_BC   = income/income_hat
    ) %>% ungroup() %>%
    select(
      region, ratio_BC
    )
  
  ratio_WC_ <- df %>% group_by(region, rank) %>%
    summarise(
      rank       = mean(rank), 
      income     = mean(exp(y)), 
      income_hat = mean(exp(y_hat)), 
      ratio_WC   = income/income_hat, 
      income     = y
    ) %>% ungroup() %>%
    select(
      region, rank, ratio_WC, income
    )
  
  if (i == 1) {
    ratio_BC <- ratio_BC_
    ratio_WC <- ratio_WC_
  } else{
    ratio_BC <- rbind(ratio_BC, ratio_BC_)
    ratio_WC <- rbind(ratio_WC, ratio_WC_)
  }
  
  gc()
}

gc()

ratio_BC <- ratio_BC %>% group_by(region) %>% 
  summarise(
    sd_BC    = sd(ratio_BC),
    ratio_BC = mean(ratio_BC)
  ) %>% ungroup()

ratio_WC <- ratio_WC %>% group_by(region, rank) %>% 
  summarise(
    sd_WC    = sd(ratio_WC),
    ratio_WC = mean(ratio_WC),
    mean_    = mean(income), 
    sd       = sd(income)
  ) %>% ungroup()

ratio  <- ratio_WC %>% left_join(ratio_BC)

gc()

lpob_16_r = 1015.44  # Linea de pobreza Rural Julio 2016
lpob_16_u = 1348.81  # Linea de pobreza Urbana Julio 2016

# Alpha Selection ----
MSE  <- NULL
diff <- NULL
alpha <- NULL

j = 1
for(i in seq(0.01, 1, 0.01)) {
  alpha[j] = i
  ratio_ = ratio %>% mutate(
    ratio = (i*ratio_BC) + ((1-i)*ratio_WC)
  )
  
  source_ <- source %>% full_join(ratio_) %>% mutate(
    y_hat = log(exp(y_hat)*ratio),
    SE    = (y - y_hat)^2,
    y_hat_ = ifelse(rururb == 1, exp(y_hat)*lpob_16_r, exp(y_hat)*lpob_16_u)
  )
  
  MSE[j] = sqrt(mean(source_$SE)) 
  
  gini = gini.wtd(source_$ingc_pc)
  gini_ = gini.wtd(source_$y_hat_)
  
  diff[j] = abs(gini - gini_)
  
  
  MSE[j] <- mean(MSE)
  diff[j] <- mean(diff)
  
  j = j + 1
}

Z = (MSE + diff)/2

alpha = (alpha[which.min(MSE)] + alpha[which.min(diff)]) / 2 + sd(alpha) #0.43

ratio  <- ratio_WC %>% left_join(ratio_BC) %>%
  mutate(
    ratio = alpha*ratio_BC + (1-alpha)*ratio_WC,
    alpha
  )

write.csv(ratio, "./Data/Example/ratios.csv", row.names = FALSE)

# Corrections ----
source <- source %>% full_join(ratio) 

source$y_hat_1 <- log(exp(source$y_hat)*source$ratio)

source$error1 <- source$y - source$y_hat_1

ggplot(source) + aes(x = error1) + 
  geom_histogram(aes(y = ..density..), fill = "#112446",
                 color = "white", alpha = 0.75, bins = 100, density = T) +
  geom_density() +
  xlim(-2, 2) +
  xlab("") +
  ylab("Density") +
  ggthemes::theme_clean() +
  theme(
    axis.line.y     = element_blank(),
    plot.background = element_blank(),
    axis.text = element_text(size = 15), 
    axis.title = element_text(size = 18)
  )

ggsave(
    filename = "./Figures/Results/Example/Error_Correction_Train.png",
    width = 3000,
    height = 1500,
    units = "px"
)

ggplot(source) + aes(y = y) + 
  geom_point( aes(x = y_hat, color = "Original"),
              alpha = 0.25) +
  geom_point( aes(x = y_hat_1, color = "Corrected"),
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
    axis.line.y     = element_blank(),
    plot.background = element_blank(),
    axis.text = element_text(size = 15), 
    axis.title = element_text(size = 18), 
    legend.background = element_blank(),
    legend.position = "top"
  )

ggsave(
    filename = "./Figures/Results/Example/Correlation.png",
    width = 3000,
    height = 1500,
    units = "px"
)

MLmetrics::R2_Score(source$y_hat_1, source$y) # 0.68

source <- source %>% mutate(
    y_hat_1 = ifelse(rururb == 1, exp(y_hat_1)*lpob_16_r, exp(y_hat_1)*lpob_16_u),
    y_hat   = ifelse(rururb == 1, exp(y_hat)*lpob_16_r, exp(y_hat)*lpob_16_u)
  )

ggplot(source) + 
    geom_density(aes(x = log(ingc_pc), fill = "Original"),   alpha = 0.5) +
    geom_density(aes(x = log(y_hat),   fill = "Prediction"), alpha = 0.5) +
    geom_density(aes(x = log(y_hat_1), fill = "Correction"), alpha = 0.5) +
    labs(fill = "") +
    xlab("log HHI") +
    ylab("Density") + 
    ggthemes::theme_clean() +
    theme(
    axis.line.y     = element_blank(),
    plot.background = element_blank(),
    axis.text = element_text(size = 15), 
    axis.title = element_text(size = 18), 
    legend.background = element_blank(),
    legend.position = "top"
  )

ggsave(
    filename = "./Figures/Results/Example/Densities.png",
    width = 3000,
    height = 1500,
    units = "px"
)

# Distribution of Ratios ----- 
ggplot(source) + aes(ratio_BC) + 
  geom_histogram(fill  = "#112446",
                 color = "white",
                 alpha = 0.75,
                 bins  = 15) +
  xlab("Between Clusters") +
  ylab("Frequency") +
  ggthemes::theme_clean() +
  theme(
    axis.line.y     = element_blank(),
    plot.background = element_blank(),
    axis.text = element_text(size = 15), 
    axis.title = element_text(size = 18), 
    legend.background = element_blank(),
    legend.position = "top"
  )

ggsave(
    filename = "./Figures/Results/Example/BC_ratios.png",
    width = 3000,
    height = 1500,
    units = "px"
)

ggplot(source) + aes(ratio_WC) + 
  geom_histogram(fill  = "#112446",
                 color = "white",
                 alpha = 0.75,
                 bins  = 15) +
  xlab("Between Clusters") +
  ylab("Frequency") +
  ggthemes::theme_clean() +
  theme(
    axis.line.y     = element_blank(),
    plot.background = element_blank(),
    axis.text = element_text(size = 15), 
    axis.title = element_text(size = 18), 
    legend.background = element_blank(),
    legend.position = "top"
  )

ggsave(
    filename = "./Figures/Results/Example/WC_ratios.png",
    width = 3000,
    height = 1500,
    units = "px"
)

ggplot(source) + aes(ratio) + 
  geom_histogram(fill  = "#112446",
                 color = "white",
                 alpha = 0.75,
                 bins  = 15) +
  xlab("Between Clusters") +
  ylab("Frequency") +
  ggthemes::theme_clean() +
  theme(
    axis.line.y     = element_blank(),
    plot.background = element_blank(),
    axis.text = element_text(size = 15), 
    axis.title = element_text(size = 18), 
    legend.background = element_blank(),
    legend.position = "top"
  )

ggsave(
    filename = "./Figures/Results/Example/BCWC_ratios.png",
    width = 3000,
    height = 1500,
    units = "px"
)

gini.wtd(source$y_hat_1)
gini.wtd(source$y_hat)
gini.wtd(source$ingc_pc)

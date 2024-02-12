# Libraries -----
library(tidyverse)
library(mice)
library(dineq)

source("./Code/Robustness/AuxFunctions.R")

lpob_18_r = 1145.5   # Linea de pobreza Rural Julio 2018
lpob_18_u = 1521.44 

lpob_16_r = 1015.44  # Linea de pobreza Rural Julio 2016
lpob_16_u = 1348.81  # Linea de pobreza Urbana Julio 2016

# Data -----
target = read_csv("./Data/ENIGH18.csv")
y_hat  = read_csv("./Data/Forward/OLS_test.csv")

target <- target %>% left_join(y_hat, by = "folio")
target$y_hat <- target$`0`

ratios <- read_csv("./Data/Forward/ratios.csv")
ratios$ent <- as.double(ratios$ent)

# Prediction ----
results <- matrix(data = NA, nrow = 10, 101)
results[, 1] <- 1:10

per <- "FWD"

left <- quantile(target$y)[2]
right <- quantile(target$y)[4]
set.seed(123)
for (i in 1:9) {
    print(i)
    for (j in 1:100){
        r = sample(c(1,2,3,4), 1, prob = c(0.45, 0.15, 0.15, 0.15))

        if (r == 1) {
           target$weights = rep(1, nrow(target))
        } else if (r == 2) {
           target$weights = ifelse(target$y < left, 2, 1)
        } else if (r == 3) {
           target$weights = ifelse(target$y > right, 2, 1)
        } else {
           target$weights = ifelse(target$y <= left & target$y >= right, 2, 1)
        }

        data <- sample_frac(target, size = i/10, weight = target$weights)

        data <- correction(data)

        results[i, j + 1] = gini.wtd(data$y_hat)
        gc()
    }

    gc()
}

results_FWD <- data.frame(results) %>% pivot_longer(cols = -X1,
    names_to = "prop", values_to = "gini") %>% group_by(X1) %>%
    summarise(
        sd = sd(gini, na.rm = T),
        gini = mean(gini, na.rm = T), 
        lower = gini - 1.96*sd,
        upper = gini + 1.96*sd, 
        p = "FWD"
    )

target = read_csv("./Data/ENIGH16.csv")
y_hat  = read_csv("./Data/Backward/OLS_test.csv")

target$y_hat <- y_hat$`0`

ratios <- read_csv("./Data/Backward/ratios.csv")
ratios$ent <- as.double(ratios$ent)

target$ent <- as.double(target$ent)

# Prediction ----
results <- matrix(data = NA, nrow = 10, 101)
results[, 1] <- 1:10

per <- "BWD"
left <- quantile(target$y)[2]
right <- quantile(target$y)[4]

set.seed(123)
for (i in 1:9) {
    print(i)
    for (j in 1:100){
        r = sample(c(1,2,3,4), 1, prob = c(0.45, 0.15, 0.15, 0.15))

        if (r == 1) {
           target$weights = rep(1, nrow(target))
        } else if (r == 2) {
           target$weights = ifelse(target$y < left , 2, 1)
        } else if (r == 3) {
           target$weights = ifelse(target$y > right , 2, 1)
        } else {
           target$weights = ifelse(target$y <= left & target$y >= right , 2, 1)
        }

        data <- sample_frac(target, size = i/10, weight = target$weights)

        data <- correction(data)

        results[i, j + 1] = gini.wtd(data$y_hat)
        gc()
    }

    gc()
}

results_BWD <- data.frame(results) %>% pivot_longer(cols = -X1,
    names_to = "prop", values_to = "gini") %>% group_by(X1) %>%
    summarise(
        sd = sd(gini, na.rm = T),
        gini = mean(gini, na.rm = T), 
        lower = gini - 1.96*sd,
        upper = gini + 1.96*sd, 
        p = "BWD"
    )

results <- rbind(results_FWD, results_BWD) 

write.csv(results, "./Results/simulations.csv")

ggplot(results %>% na.omit()) + aes(x = X1, y = gini) +
    geom_hline(yintercept = 0.465, color = "red", alpha = 0.5) +
    geom_hline(yintercept = 0.472, color = "blue", alpha = 0.5) +
    geom_point(aes(color = p),size = 3) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = p), alpha = 0.25) +
    scale_x_continuous(breaks = 1:9, labels = seq(10, 90, 10)) +
    ylim(c(0.44, 0.48)) +
    xlab("%") +
    ylab("Gini") +
    labs(fill = "", color = "") +
    scale_color_hue(labels = c("Backward", "Forward")) +
    scale_fill_hue(labels = c("Backward", "Forward")) +
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
    filename = "./Figures/Results/Robustness/Simulations.png",
    width  = 3000,
    height = 2000,
    units  = "px"
)

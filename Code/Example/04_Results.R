# Libraries ----
library(tidyverse)
library(dineq)

# Data ----

df <- read_csv("./Data/Example/final.csv")

df <- df %>% transmute(
    index  = (index - min(index))/(max(index) - min(index)), 
    income_0 = as.double(y),
    income = as.double(y_hat_BC_WC),
    sex = factor(sex),
    birth_region = factor(birth_region),
    ethnicity = factor(ethnicity),
    father_educ,
    mother_educ, 
    father_ocup = factor(father_ocup)
) %>% na.omit()

ggplot(df) + aes(x = index, y = log(income_0), color = "A") +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", aes(color = "B"),size = 1) +
    xlab("Asset Index") +
    ylab("log HHI (Original)") +
    scale_color_hue(direction = -1) +
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
    filename = "./Figures/Example/Correlation_0.png",
    width = 3000,
    height = 1500,
    units = "px"
)

ggplot(df) + aes(x = index, y = log(income), color = "A") +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", aes(color = "B"),size = 1) +
    xlab("Asset Index") +
    ylab("log HHI (Corrected)") +
    scale_color_hue(direction = -1) +
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
    filename = "./Figures/Example/Correlation.png",
    width = 3000,
    height = 1500,
    units = "px"
)

ggplot(df) + aes(x = log(income_0), y = log(income), color = "A") +
    geom_line(aes(x = log(income)), color = "darkgrey") +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", aes(color = "B"),size = 1) +
    xlab("log HHI (Original)") +
    ylab("log HHI (Corrected)") +
    scale_color_hue(direction = -1) +
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
    filename = "./Figures/Example/Correlation_HHI.png",
    width = 3000,
    height = 1500,
    units = "px"
)

cor(df$income, df$index)
cor(df$income_0, df$index)
cor(df$income_0, df$income)

# IOp ----
IOp_W <- NULL
Absolute_W <- NULL
IOp_HHI <- NULL
Absolute_HHI <- NULL
IOp_HHI_0 <- NULL
Absolute_HHI_0 <- NULL

set.seed(123)
for (i in 1:200){
    df_ <- sample_frac(df, size = 1, replace = T)

    w <- lm(index~.-income-income_0, data = df_)
    y_hat <- predict(w)

    Absolute_W[i] <- gini.wtd(y_hat)
    IOp_W[i]   <- gini.wtd(y_hat)/gini.wtd(df$index)

    hhi <- lm(income~.-index-income_0, data = df_)
    y_hat <- predict(hhi)

    Absolute_HHI[i] <-  gini.wtd(y_hat)
    IOp_HHI[i] <- gini.wtd(y_hat)/gini.wtd(df$income)

    hhi_0 <- lm(income_0~.-index-income, data = df_)
    y_hat <- predict(hhi_0)

    Absolute_HHI_0[i] <-  gini.wtd(y_hat)
    IOp_HHI_0[i] <- gini.wtd(y_hat)/gini.wtd(df$income_0)
}

reg_W <- lm(index~.-income-income_0, data = df)

lower_W <- mean(IOp_W) - 1.96*sd(IOp_W)/sqrt(200)
mean_W  <- mean(IOp_W)
upper_W <- mean(IOp_W) + 1.96*sd(IOp_W)/sqrt(200)

lower_W_A <- round(mean(Absolute_W) - 1.96*sd(Absolute_W)/sqrt(200),3)
mean_W_A  <- mean(Absolute_W)
upper_W_A <- mean(Absolute_W) + 1.96*sd(Absolute_W)/sqrt(200)

print(paste("[", round(lower_W,3), "; ", round(mean_W,3), "; ", round(upper_W,3), "]"))
print(paste("[", round(lower_W_A,3), "; ", round(mean_W_A,3), "; ", round(upper_W_A,3), "]"))

gini.wtd(df$index)

# Income
reg_HHI <- lm(income~.-index-income_0, data = df)

lower_HHI <- mean(IOp_HHI) - 1.96*sd(IOp_HHI)/sqrt(200)
mean_HHI  <- mean(IOp_HHI)
upper_HHI <- mean(IOp_HHI) + 1.96*sd(IOp_HHI)/sqrt(200)

lower_HHI_A <- mean(Absolute_HHI) - 1.96*sd(Absolute_HHI)/sqrt(200)
mean_HHI_A  <- mean(Absolute_HHI)
upper_HHI_A <- mean(Absolute_HHI) + 1.96*sd(Absolute_HHI)/sqrt(200)

print(paste("[", round(lower_HHI,3), "; ", round(mean_HHI,3), "; ", round(upper_HHI,3), "]"))
print(paste("[", round(lower_HHI_A,3), "; ", round(mean_HHI_A,3), "; ", round(upper_HHI_A,3), "]"))

gini.wtd(df$income)

reg_HHI_0 <- lm(income_0~.-index-income, data = df)

lower_HHI_0 <- mean(IOp_HHI_0) - 1.96*sd(IOp_HHI_0)/sqrt(200)
mean_HHI_0  <- mean(IOp_HHI_0)
upper_HHI_0 <- mean(IOp_HHI_0) + 1.96*sd(IOp_HHI_0)/sqrt(200)

lower_HHI_0_A <- mean(Absolute_HHI_0) - 1.96*sd(Absolute_HHI_0)/sqrt(200)
mean_HHI_0_A  <- mean(Absolute_HHI_0)
upper_HHI_0_A <- mean(Absolute_HHI_0) + 1.96*sd(Absolute_HHI_0)/sqrt(200)

print(paste("[", round(lower_HHI_0,3), "; ", round(mean_HHI_0,3), "; ", round(upper_HHI_0,3), "]"))
print(paste("[", round(lower_HHI_0_A,3), "; ", round(mean_HHI_0_A,3), "; ", round(upper_HHI_0_A,3), "]"))

gini.wtd(df$income_0)

stargazer::stargazer(reg_W, reg_HHI_0, reg_HHI)


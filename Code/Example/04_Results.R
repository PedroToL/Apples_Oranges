# 

df <- read_csv("./Data/Example/final.csv")

df <- df %>% transmute(
    index  = (index - min(index))/(max(index) - min(index)) , 
    income = as.double(y_hat_BC_WC),
    sex = factor(sex),
    birth_region = factor(birth_region),
    ethnicity = factor(ethnicity),
    father_educ,
    mother_educ, 
    father_ocup = factor(father_ocup)
) %>% na.omit()

ggplot(df) + aes(x = index, y = log(income), color = "A") +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", aes(color = "B"),size = 1) +
    xlab("Asses Index") +
    ylab("log HHI") +
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

cor(df$income, df$index)
# Wealth 
IOp_W <- NULL
Absolute_W <- NULL
IOp_HHI <- NULL
Absolute_HHI <- NULL

set.seed(123)
for (i in 1:200){
    df_ <- sample_frac(df, size = 1, replace = T)

    w <- lm(index~.-income, data = df_)
    y_hat <- predict(w)

    Absolute_W[i] <- gini.wtd(y_hat)
    IOp_W[i]   <- gini.wtd(y_hat)/gini.wtd(df$index)

    hhi <- lm(income~.-index, data = df_)
    y_hat <- predict(hhi)

    Absolute_HHI[i] <-  gini.wtd(y_hat)
    IOp_HHI[i] <- gini.wtd(y_hat)/gini.wtd(df$income)
}

reg_W <- lm(index~.-income, data = df)

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
reg_HHI <- lm(income~.-index, data = df)

lower_HHI <- mean(IOp_HHI) - 1.96*sd(IOp_HHI)/sqrt(200)
mean_HHI  <- mean(IOp_HHI)
upper_HHI <- mean(IOp_HHI) + 1.96*sd(IOp_HHI)/sqrt(200)

lower_HHI_A <- mean(Absolute_HHI) - 1.96*sd(Absolute_HHI)/sqrt(200)
mean_HHI_A  <- mean(Absolute_HHI)
upper_HHI_A <- mean(Absolute_HHI) + 1.96*sd(Absolute_HHI)/sqrt(200)

print(paste("[", round(lower_HHI,3), "; ", round(mean_HHI,3), "; ", round(upper_HHI,3), "]"))
print(paste("[", round(lower_HHI_A,3), "; ", round(mean_HHI_A,3), "; ", round(upper_HHI_A,3), "]"))

gini.wtd(df$income)

stargazer::stargazer(reg_W, reg_HHI)


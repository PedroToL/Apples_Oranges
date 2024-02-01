# Librerias ----
library(tidyverse)   # Limpieza, transformación y carga de datos
library(fastDummies) # Crear dummies
library(ggthemes)

# Datos---- 
enigh18 = read_csv("./Data/ENIGH18.csv") %>% filter(ing_cor > 0)
enigh16 = read_csv("./Data/ENIGH16.csv") %>% filter(ing_cor > 0) %>% filter(ingc_pc < 4000000)
emovi17 = read_csv("./Data/EMOVI17.csv")

enigh18 %>% select(lincome, ing_cor, ingc_pc) %>%                      
  mutate(
    year = 2018
  ) %>%
  rbind(.,
        mutate(
            select(enigh16, lincome, ing_cor, ingc_pc),
            year = 2016
        )
  ) %>% ggplot() + aes(x    = lincome,
                          fill = factor(year)) +
  geom_density(alpha = 0.5) +
  ylab("Density") +
  xlab("Per Capita Income (log)") +
  labs(fill = "Year") +
  scale_fill_manual(values = c("red", "blue")) +
  ggthemes::theme_clean() +
  theme(
    axis.line.y       = element_blank(),
    legend.position   = "top",
    legend.background = element_blank(),
    plot.background   = element_blank()
  )

ggsave(
    filename = "./Figures/Data/Density_Original.png",
    width = 3000,
    height = 1500,
    units = "px"
)

# Transformaciones ----
lpob_18_r = 1145.5   # Linea de pobreza Rural Julio 2018
lpob_18_u = 1521.44  # Linea de pobreza Urbana Julio 2018

lpob_16_r = 1015.44  # Linea de pobreza Rural Julio 2016
lpob_16_u = 1348.81  # Linea de pobreza Urbana Julio 2016

lpob_17_r = 1120.08  # Linea de pobreza Rural Julio 2017
lpob_17_u = 1471.60  # Linea de pobreza Urbana Julio 2017

enigh18 = enigh18 %>% mutate(
  r_ingc_pc = ifelse(rururb == 1, ingc_pc/lpob_18_r, ingc_pc/lpob_18_u),
  y         = log(r_ingc_pc)
)

enigh16 = enigh16 %>% mutate(
  r_ingc_pc = ifelse(rururb == 1, ingc_pc/lpob_16_r, ingc_pc/lpob_16_u),
  y         = log(r_ingc_pc)
)

enigh18 %>% select(y) %>% 
  mutate(
    year = 2018
  ) %>%
  rbind(.,
        mutate(
          select(enigh16, y),
          year = 2016)
  ) %>%
  ggplot() + aes(x = y, fill = factor(year)) +
  geom_density(alpha = 0.5) +
  ylab("Density") +
  xlab("Per Capita Income (log)") +
  labs(fill = "Year") +
  scale_fill_manual(values = c("red", "blue")) +
  ggthemes::theme_clean() +
  theme(
    axis.line.y = element_blank(),
    legend.position = "top",
    legend.background = element_blank(),
    plot.background = element_blank()
  )

ggsave(
    filename = "./Figures/Data/Density_Scaled.png",
    width = 3000,
    height = 1500,
    units = "px"
)

enigh16 %>% transmute(
    HHI       = r_ingc_pc,
    `log HHI` = y,
    `PL Urban` = lpob_16_u, 
    `PL Rural` = lpob_16_r,
    Survey = "ENIGH 2016"
) %>%
    rbind(., 
        enigh18 %>% transmute(
            HHI        = r_ingc_pc,
            `log HHI`  = y,
            `PL Urban` = lpob_18_u, 
            `PL Rural` = lpob_18_r,
            Survey     = "ENIGH 2018"
        )) %>%
    rbind(., 
        emovi17 %>% transmute(
            HHI        = NA,
            `log HHI`  = NA, 
            `PL Urban` = lpob_17_u, 
            `PL Rural` = lpob_17_r,
            Survey     = "EMOVI 2017"
        )) %>% 
        group_by(Survey) %>%
        summarise_all(mean, na.rm = T) %>%
        write.csv(., "./Results/Statistics.csv")

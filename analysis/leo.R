# ADB
# IAA
# 2024-03-12

# Leopard Seals
# Mensaje de Flor Soto describiendo el problema

# La idea era estudiar el efecto del sexo, la clase de edad (juvenil o
# adulto), la variación anual (años 2014, 2015, 2019 y 2020) y el índice
# de condicion corporal en la prevalencia y/o abundancia media.

# Los datos tienen el desafio que que hay muchos ceros, así que no sé si puede modelar..

# libraries ----
library(data.table)
library(tidyverse)
library(plotly)
library(lme4)
library(car)
library(performance)
library(modelbased)


# functions -----
source("R/compile_rmd.R")


# read data -----
leo <- read.csv("data/leopard.csv", sep = ";", dec = ",") %>%
  as.data.table()

# look at data structure ----
str(leo)
# 50 unique seals
unique(leo$Sp)

# transform weight to numeric
leo$weight_kg <- as.numeric(leo$weight_kg)

# transform sex to factor
leo$sex <- as.factor(leo$sex)

# transform age_class to factor
leo$age_class <- as.factor(leo$age_class)

# code presence
leo[, presence := ifelse(Lice == 0, 0, 1)]


# explore data -----
## largo - peso ----
# no estoy seguro que se puedan usar estos datos para estimar una relacion largo-peso
# para asi poder estimar la condicion corporal
# Opciones:
# 1. usar de todas maneras, pero tener en cuenta en las dicusiones
# 2. encontrar relaciones largo-peso publicadas

# Aqui, tomare la opcion 1, pero es facil cambiarla

leo <- leo %>% arrange(sex, SL_cm)
m.lw.m <- lm(log(weight_kg) ~ log(SL_cm), data = leo[sex == "M"])
m.lw.f <- lm(log(weight_kg) ~ log(SL_cm), data = leo[sex == "F"])

leo$pred_weight <- 0.0; leo$pred_weight_lci <- 0.0; leo$pred_weight_uci <- 0.0

leo[sex == "M" & !is.na(weight_kg), pred_weight := exp(modelbased::estimate_prediction(m.lw.m)$Predicted)]
leo[sex == "M" & !is.na(weight_kg), pred_weight_lci := exp(modelbased::estimate_prediction(m.lw.m)$CI_low)]
leo[sex == "M" & !is.na(weight_kg), pred_weight_uci := exp(modelbased::estimate_prediction(m.lw.m)$CI_high)]
leo[sex == "F" & !is.na(weight_kg), pred_weight := exp(modelbased::estimate_prediction(m.lw.f)$Predicted)]
leo[sex == "F" & !is.na(weight_kg), pred_weight_lci := exp(modelbased::estimate_prediction(m.lw.f)$CI_low)]
leo[sex == "F" & !is.na(weight_kg), pred_weight_uci := exp(modelbased::estimate_prediction(m.lw.f)$CI_high)]

leo[is.na(weight_kg), pred_weight := NA]

p.lw <- ggplot(data = leo, aes(x = SL_cm, y = weight_kg)) +
  geom_line(aes(y = pred_weight)) +
  geom_ribbon(aes(ymin = pred_weight_lci, ymax = pred_weight_uci), alpha = 0.2) +
  geom_point() +
  theme_bw() +
  facet_wrap(.~sex)

## calcular condicion -----
leo[, rel_cond := weight_kg/pred_weight]

p.cond <- ggplot(data = leo, aes(x = rel_cond, fill = sex, color = sex)) +
  theme_bw() +
  geom_density(alpha = 0.3) +
  xlab("Relative condition") +
  theme(legend.position = c(.05, .85),
        legend.background = element_rect(colour = NA, fill = NA),
        legend.title = element_blank())


## Lice abundance ----
p.lice.sex <- ggplot(leo, aes(x = sex, y = Lice)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)

p.lice.cond <- ggplot(leo, aes(x = rel_cond, y = Lice, color = sex)) +
  theme_bw() +
  geom_point(alpha = 0.8) +
  geom_smooth() +
  geom_vline(xintercept = 1, lty = 2)
ggplotly(p.lice.cond)

p.lice.age <- ggplot(leo, aes(x = age_class, y = Lice)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)

p.lice.year <- ggplot(leo, aes(x = year, y = Lice)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)
## Lice presence ----
p.lice.sex.presence <- ggplot(leo, aes(x = sex, y = presence)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)

p.lice.cond.presence <- ggplot(leo, aes(x = rel_cond, y = presence, color = sex)) +
  theme_bw() +
  geom_point(alpha = 0.8) +
  geom_smooth() +
  geom_vline(xintercept = 1, lty = 2)
ggplotly(p.lice.cond.presence)

p.lice.age.presence <- ggplot(leo, aes(x = age_class, y = presence)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)

p.lice.year.presence <- ggplot(leo, aes(x = year, y = presence)) +
  theme_bw() +
  geom_point(position = position_dodge2(width = 0.4), alpha = 0.5)


## data summaries -----
### sex ----
t.lice.presence.sex <- leo %>%
  group_by(sex) %>%
  reframe(n = n(),
          pres = sum(presence)) %>%
  mutate(prop = pres/n)

t.lice.abundance.sex <- leo %>%
  group_by(sex) %>%
  reframe(median_lice = median(Lice),
          mean_lice = mean(Lice),
          sd_lice = sd(Lice),
          CV_lice = 100 * sd_lice/mean_lice)
### age_class ----
t.lice.presence.age_class <- leo %>%
  group_by(age_class) %>%
  reframe(n = n(),
          pres = sum(presence)) %>%
  mutate(prop = pres/n)

t.lice.abundance.age_class <- leo %>%
  group_by(age_class) %>%
  reframe(median_lice = median(Lice),
          mean_lice = mean(Lice),
          sd_lice = sd(Lice),
          CV_lice = 100 * sd_lice/mean_lice)




# model -----
## presence ~ condition + age + year -----
# can't use sex as only one F with lice
m.lice.cond.age.year <- glmmTMB::glmmTMB(presence ~ rel_cond + age_class  + (1|year),
      data = leo,
      family = "binomial")

performance::check_model(m.lice.cond.age.year)

# remove random year effect
## presence ~ condition + age  -----
m.lice.cond.age <- glmmTMB::glmmTMB(presence ~ rel_cond + age_class ,
                           data = leo,
                           family = "binomial")

performance::check_model(m.lice.cond.age)
performance::check_convergence(m.lice.cond.age)
performance::check_singularity(m.lice.cond.age)
performance::model_performance(m.lice.cond.age)
car::Anova(m.lice.cond.age)
summary(m.lice.cond.age)


## presence ~ condition  -----
m.lice.cond <- glmmTMB::glmmTMB(presence ~ rel_cond  ,
                                    data = leo,
                                    family = "binomial")

performance::check_model(m.lice.cond)
performance::check_convergence(m.lice.cond)
performance::check_singularity(m.lice.cond)
performance::model_performance(m.lice.cond)
car::Anova(m.lice.cond)
summary(m.lice.cond)


## presence ~ age  -----
m.lice.age <- glmmTMB::glmmTMB(presence ~ age_class  ,
                               data = leo,
                               family = "binomial")

performance::check_model(m.lice.age)
performance::check_convergence(m.lice.age)
performance::check_singularity(m.lice.age)
performance::model_performance(m.lice.age)
car::Anova(m.lice.age)
summary(m.lice.age)

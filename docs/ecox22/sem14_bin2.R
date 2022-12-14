# Эконометрика, учеб. год 2022-23
# Обр. программа "Экономика", НИУ ВШЭ, г. Санкт-Петербург
# Семинар 14. Модели бинарного выбора (2)


# Подготовка к работе ==========================================================

## Загрузка пакетов ------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(margins)
library(stargazer)
library(broom)
library(pROC)

## Другие настройки ------------------------------------------------------------

### Установка рабочей директории
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Общая тема оформления для всех графиков
theme_set(theme_light(base_size = 12))

### Отлючение экспоненциальной записи чисел
options(scipen = 999)


# Загрузка и преобразование данных =============================================
default <- read_csv("default.csv")

default <- default %>% mutate(id = row_number(), 
                              EDUCATION = as.factor(EDUCATION))

old <- default %>% filter(!is.na(DEFAULT))
new <- default %>% filter(is.na(DEFAULT))

set.seed(101) 
train <- old %>% sample_frac(6/7)
test  <- anti_join(old, train, by = 'id')


# Разведывательный анализ ======================================================
default %>% glimpse()

default %>% summary()

default %>% select(DEFAULT, EDUCATION) %>% table()

default %>% 
  ggplot(aes(x = DTI, y = DEFAULT)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_smooth(method = "glm", se = FALSE, color = "red",
              method.args = list(family = "binomial"))



# Регрессионный анализ =========================================================

## Спецификации ----------------------------------------------------------------
spec0 <- DEFAULT ~ . - id - DEBT - INCOME
spec1 <- DEFAULT ~ . - id - DEBT - INCOME + I(AGE^2)
spec2 <- DEFAULT ~ . - id - DTI + I(AGE^2)
spec3 <- DEFAULT ~ . - id - DTI - INCOME - DEBT + I(AGE^2) + log(INCOME) + log(DEBT)

## Оценивание ------------------------------------------------------------------
glm1 <- glm(spec0, data = train, binomial(link = "probit"), x = TRUE)
glm2 <- glm(spec0, data = train, binomial(link = "logit"),  x = TRUE)

glm3 <- glm(spec1, data = train, binomial(link = "probit"), x = TRUE)
glm4 <- glm(spec1, data = train, binomial(link = "logit"),  x = TRUE)

glm5 <- glm(spec2, data = train, binomial(link = "probit"), x = TRUE)
glm6 <- glm(spec2, data = train, binomial(link = "logit"),  x = TRUE)

glm7 <- glm(spec3, data = train, binomial(link = "probit"), x = TRUE)
glm8 <- glm(spec3, data = train, binomial(link = "logit"),  x = TRUE)

stargazer(glm1, glm3, glm5, glm7,
          glm2, glm4, glm6, glm8, 
          type = "text")

## Проверка гипотез ------------------------------------------------------------
linearHypothesis(glm7, "ADDRESS = 0", test = "Chisq")
linearHypothesis(glm7, c("ADDRESS = 0", "EMPLOY = 0"), test = "Chisq")


## Предельные эффекты ----------------------------------------------------------
glm7 %>% margins(at = list(EDUCATION = c("0", "1"))) %>% summary()

me <- glm7 %>% margins_summary() %>% filter(factor != "id", factor != "DTI")

me %>% 
  ggplot() +
  geom_point(aes(factor, AME)) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45))

dydx(train, glm7, "EDUCATION")

glm7 %>% marginal_effects() %>% summary()


## Отношение шансов ------------------------------------------------------------
exp(coef(glm8))

glm8 %>%
  tidy() %>%
  mutate(or = exp(estimate),  
         var_diag = diag(vcov(glm8)),
         or_se = sqrt(or^2 * var_diag),
         lower_ci = or - qnorm(0.975) * or_se, 
         upper_ci = or + qnorm(0.975) * or_se) 

or_se2 <- exp(coef(glm2)) * sqrt(diag(vcov(glm2)))
or_se4 <- exp(coef(glm4)) * sqrt(diag(vcov(glm4)))
or_se6 <- exp(coef(glm6)) * sqrt(diag(vcov(glm6)))
or_se8 <- exp(coef(glm8)) * sqrt(diag(vcov(glm8)))

stargazer(glm2, glm4, glm6, glm8, 
          se = list(or_se2, or_se4, or_se6, or_se8),
          apply.coef = exp,
          type = "text")


# Предсказание =================================================================

## Расчет предсказанной вероятности и предсказанных значений -------------------
predict(glm7, newdata = test, type = 'response')

treshold <- 0.5

test <- test %>% mutate(def_prob = predict(glm7, newdata = test, type = 'response'),
                        def_hat = as.numeric(def_prob > treshold))

## Оценка качества предсказаний ------------------------------------------------
test %>% select(DEFAULT, def_hat) %>% table() # confusion matrix

test %>% summarise(sens = sum(def_hat == 1 & def_hat == DEFAULT) / sum(DEFAULT == 1), 
                   spec = sum(def_hat == 0 & def_hat == DEFAULT) / sum(DEFAULT == 0))

roc <- test %>% roc(DEFAULT, def_prob)
roc %>% glimpse()

roc %>%  
  ggroc(legacy.axes = T) + 
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed") + 
  coord_cartesian(xlim = c(-0.01, 1.01), ylim = c(-0.01, 1.01), expand = F)


# Эконометрика, учеб. год 2022-23
# Обр. программа "Экономика", НИУ ВШЭ, г. Санкт-Петербург
# Семинар 10. Оформление отчетов в R


# Подготовка к работе ==========================================================

## Загрузка пакетов ------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(sandwich)
library(lmtest)
library(car)
library(broom)
library(xtable)
library(ggpubr)
library(stargazer)
library(modelsummary)


## Собственные функции ---------------------------------------------------------
number <- function(x, na.rm = TRUE){return(sum(!is.na(x)))}

## Другие настройки ------------------------------------------------------------

### Установка рабочей директории
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()

### Общая тема оформления для всех графиков
theme_set(theme_classic(base_size = 12))

### Отлючение экспоненциальной записи чисел
#options(scipen = 999)


# Загрузка данных ==============================================================
MEPS <- read_csv("MEPS.csv") # Medical expenditure panel survey

MEPS %>% glimpse()

# Описательные статистики ======================================================

## dplyr+ ----------------------------------------------------------------------
desc <- MEPS %>%
  select(totexp, posexp, suppins, phylim, actlim, totchr, age, female, income) %>%
  summarise(across(everything(), 
                   list(n = number, mean = mean, median = median, sd = sd, min = min, max = max), 
                   na.rm = TRUE)) %>% 
  pivot_longer(everything(), names_to = "name", values_to = "value") %>% 
  separate(name, c("variable", "statistic"), sep = "_") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  arrange(variable) %>% 
  select(variable, n, mean, median, sd, min, max)

desc

desc %>% xtable(caption = "Descriptive statistics", 
                label = "desc_stats")

desc %>% write_csv('desc_stats.csv')


MEPS %>%
  select(totexp, posexp, suppins, phylim, actlim, totchr, age, female, income) %>%
  group_by(suppins) %>%
  summarise(across(everything(), 
                   list(n = number, mean = mean, median = median, sd = sd, min = min, max = max), 
                   na.rm = TRUE)) %>% 
  pivot_longer(-suppins, names_to = "name", values_to = "value") %>% 
  separate(name, c("variable", "statistic"), sep = "_") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  arrange(variable) %>% 
  select(variable, suppins, n, mean, median, sd, min, max)


## stargazer -------------------------------------------------------------------
#MEPS %>% stargazer(type = "text")

mtcars %>% stargazer(type = "text")
mtcars %>% stargazer(type = "latex")
mtcars %>% stargazer(type = 'html', out = "desc_mtcars.html")

mtcars %>%
  stargazer(type = 'text', min.max = TRUE, mean.sd = TRUE, 
            nobs = TRUE, median = FALSE, iqr = FALSE,
            digits = 1, align = TRUE,
            out = "desc_mtcars.txt",
            title = "Descriptive statistics", 
            covariate.labels = c("Miles/(US)gallon","No. of cylinders","Displacement (cu.in.)",
                                 "Gross horsepower","Rear axle ratio","Weight (lb/1000)",
                                 "1/4 mile time","V/S","Transmission (0=auto, 1=manual)",
                                 "Number of forward gears","Number of carburetors"))

## modelsummary ----------------------------------------------------------------

#datasummary(All(mtcars) ~ mean + sd + min + max, data = mtcars)

datasummary(totexp + age + (Доход = income) + suppins ~ Mean + SD + Min + Max, 
            data = MEPS, 
            #output = "latex"
            #output = "table.tex"
            output = 'table.docx'
            )

datasummary(totexp + age + income ~ Factor(suppins) * (mean + sd), 
            data = MEPS,
            #output = 'table.docx'
            )

# table(MEPS$suppins, MEPS$private)
# 
# datasummary_crosstab(suppins ~ private, statistic = ~ N, data = MEPS)
# datasummary_crosstab(suppins ~ private, statistic = ~ Percent("col"), data = MEPS)
# 
# datasummary_crosstab(suppins ~ private, data = MEPS)

# Графики ======================================================================

## Посмотреть все сразу --------------------------------------------------------
MEPS %>% select(totexp, totchr, age, income) %>% ggpairs()

## Оформление и сохранение графиков --------------------------------------------
MEPS %>%
  ggplot(aes(income, ltotexp)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Годовой доход домохозяйства / 1000") + 
  ylab("Логарифм медицинских расходов") + 
  facet_grid(female ~ suppins)

ggsave("facetgrid.png")

s1 <- MEPS %>%
  ggplot(aes(income, ltotexp)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) + 
  xlab("Годовой доход домохозяйства / 1000") + 
  ylab("Логарифм медицинских расходов")

s1

ggsave("scatter_plot.png")


## Комбинация графиков ---------------------------------------------------------
s2 <- MEPS %>%
  ggplot(aes(educyr, ltotexp)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("Количество лет образования") + 
  ylab("Логарифм медицинских расходов")

s2

s1s2 <- ggarrange(s1, s2, ncol = 2) 
s1s2

s1s2 %>% ggexport(filename = "scatter_plots.png", width = 800, height = 400)


# Регрессионный анализ =========================================================

## Спецификации ----------------------------------------------------------------
spec0 <- ltotexp ~ 1 + phylim + actlim + totchr
spec1 <- ltotexp ~ 1 + phylim + actlim + totchr + suppins + age + female + income
spec2 <- ltotexp ~ 1 + phylim + actlim + totchr + suppins + age + female + income + I(income^2)
spec3 <- ltotexp ~ 1 + phylim + actlim + totchr + suppins + age + female + income + region + income:region


## Оценивание ------------------------------------------------------------------
reg0 <- lm(spec0, data = MEPS)
cov0 <- vcovHC(reg0, type = "HC0")

cov0
cov0*nrow(MEPS)

se0 <- sqrt(diag(cov0))

summary(reg0)

coeftest(reg0, df = Inf, vcov = cov0)

tidy(reg0)
tidy(reg0) %>% mutate(rob.se = sqrt(diag(cov0))[term]) 
tidy(reg0) %>% mutate(rob.se = sqrt(diag(cov0))[term]) %>% select(term, estimate, rob.se)

coeftest(reg0, df = Inf, vcov. = cov0) %>% tidy()


reg1 <- lm(spec1, data = MEPS)
cov1 <- vcovHC(reg1, type = "HC0")
se1 <- sqrt(diag(cov1))

reg2 <- lm(spec2, data = MEPS)
cov2 <- vcovHC(reg2, type = "HC0")
se2 <- sqrt(diag(cov2))

reg3 <- lm(spec3, data = MEPS)
cov3 <- vcovHC(reg3, type = "HC0")
se3 <- sqrt(diag(cov3))

## Сопоставление результатов ---------------------------------------------------
stargazer(reg0, reg1, reg2, reg3,
          se = list(se0, se1, se2, se3),
          #column.labels = c("A", "B", "C", "D"),
          colnames = FALSE,
          type = "text")

stargazer(reg0, reg1, reg2, reg3,
          se = list(se0, se1, se2, se3),
          type = "text",
          #type = "latex",
          #type = "html",
          #out = "results.html", 
          title = "Результаты оценивания",
          keep = c("phylim", "actlim", "totchr"),
          covariate.labels = c("Functional limitation", "Activity limitation", "Number of chronic problems"),
          keep.stat = "n",
          notes = c("В скобках даны робастные стандартные ошибки", 
                    "Все регрессии содержат контрольные переменные: ..."))

modelsummary(models = list(reg0, reg1, reg2, reg3),                       # список оцененных моделей
             vcov = list(cov1, cov1, cov2, cov3),                         # список ковариционных матриц для расчета стандартных ошибок
             statistic = "std.error",                                     # выводить стандартные ошибки
             stars = TRUE,                                                # звездочки для уровня значимости
             gof_omit = ".*",                                             # не выводить никаких показателей качества моделей
             notes = list("В скобках даны робастные стандартные ошибки"), # комментарий по поводу расчета стандартных ошибок
             title = "Результаты оценивания")

modelsummary(models = list(reg0, reg1, reg2, reg3),                       
             vcov = list(cov1, cov1, cov2, cov3), 
             #output = "latex",
             #output = "results.docx",
             coef_omit = "suppins|age|female|income|region",
             coef_map = c("phylim" = "Наличие функциональных ограничений", 
                          "actlim" = "Наличие ограничений активности", 
                          "totchr" = "Число хронических заболеваний"),
             statistic = "std.error",                                     
             stars = TRUE,                                                
             gof_omit = ".*",                                             
             notes = list("В скобках даны робастные стандартные ошибки",
                          "Все регрессии содержат контрольные переменные: ..."), # комментарий по поводу расчета стандартных ошибок
             title = "Результаты оценивания")

# Tests ========================================================================
linearHypothesis(reg0, 
                 c("phylim = 0", "actlim = 0", "totchr = 0"), 
                 test = "Chisq", 
                 white.adjust = "hc0")

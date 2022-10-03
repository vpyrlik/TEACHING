#  Open with Encoding Windows 1251   # 

######################################
#                                    #
#  О риске инфекционного заражения   #
#      в американских больницах      #
#                                    #
#         Владимир Пырлик            #
#           ЭКОНОМЕТРИКА             #
#                                    #
#             Неделя 4               #
#    Эмпирический пример по теме     #
# "Множественная линейная регрессия" #
#                                    #
######################################

# На основе данных из учебника
# Applied Linear Regression Models (McGraw Hill/Irwin Series:
# Operations and Decision Sciences) 4th Edition

##################################
#             ПАКЕТЫ             #
##################################
# установка пакетов, запускать один раз или для обновления
#install.packages(c("datasets,
#                   lmtest,
#                   stargazer"))

# подгружаем пакеты
library(sandwich)
library(lmtest)
library(stargazer)

##################################
#             ДАННЫЕ             #
##################################
# загружаем данные
setwd("~/ECOX21")
dta <- read.table("hospitaldata.txt",
                  header=TRUE)

##################################
#           РЕГРЕССИЯ            #
##################################

## работа "вручную"
# количество наблюдений
n <- nrow(dta)

# матрицы с данными
Y <- dta$InfctRsk
X <- cbind(intercept = rep(1,n),
           stay      = dta$Stay,
           age       = dta$Age,
           beds      = dta$Beds)

# компоненты для вычисления оценок
hatQxx  <- t(X) %*% X / n 
hatQxy  <- t(X) %*% Y / n
hatQxx1 <- solve(hatQxx)

# оценки МНК
hatbeta <- hatQxx1 %*% hatQxy

# вектор остатков
hatU    <- as.vector(Y - X%*%hatbeta) 

# не-робастная оценка
# матрицы ковариаций оценок 
hatV1   <- var(hatU)*hatQxx1 

# робастная (метод Уайта) оценка
# матрицы ковариаций оценок
hatVxu <- t(X) %*% diag(hatU^2) %*% X/n
hatV2  <- hatQxx1 %*% hatVxu %*% hatQxx1

# две версии стандартных ошибок
SE1 <- sqrt(diag(hatV1)/n) # не-робастные SE
SE2 <- sqrt(diag(hatV2)/n) # робастные SE (White's)

# "таблица" с результатами
cbind(hatbeta, SE1, SE2)

## работа встроенными средствами
# оценка МНК, функция lm()
m1 <- lm(InfctRsk ~ 1 + Stay + Age + Beds,
         data=dta)

# оценка стандартных ошибок,
# робастная формула
cf1 <- coeftest(m1, df=Inf, vcov=vcovHC, type="HC0")

# вывод результатов в таблицу
stargazer(m1,cf1,type='text')

##################################
#      ПРОВЕРКА ГИПОТЕЗЫ         #
##################################
t1  <- cf1[4,1]/cf1[4,2]
Pv1 <- 1-pnorm(t1) #1-сторонняя!!!
        

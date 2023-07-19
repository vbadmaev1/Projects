setwd("C:/Users/79081/Desktop/Матстат проект")

install.packages("readxl")
install.packages('EnvStats')
library(readxl)
library(EnvStats)

Ural <- read_excel("Урал.xlsx", sheet=1, skip=0)
Income <- Ural$Доходы

# 1 задание: среднее значение дохода и его дисперсия
mean =  mean(Income)
var =  var(Income)

# 2 задание: гистограмма дохода
hist(Income, main = 'Гистограмма дохода', breaks = log2(length(Income)), xlab = 'Доход', freq = FALSE)

# 3 Задание: Коэффициенты асимметрии и эксцесса
install.packages("moments")
library(moments)

skewness = skewness(Income) # коэффициент асимметрии (сдвиг концентрации плотности относительно стандартной нормальной)
kurtosis = kurtosis(Income) # коэффициент эксцесса (показывает меру остроту пика)

# 4 Задание: гипотеза о том, что доход распределен нормально
shapiro.test(Income)
ks.test(Income, 'pnorm', mean = mean(Income), sd = sd(Income))

# 5 Задание: корреляция дохода и посевной площади
install.packages("corrplot")
library(corrplot)
square <- Ural$Площадь
cor(square, Income)
cor.test(square, Income)
q <- c(square, Income)
x <- matrix(q, ncol = 2)
corrplot(cor(x))


#Задание 6: Проверить гипотезу о равенстве дисперсий дохода и расхода
cost <- Ural$Расходы
var.test(Income, cost)

#Задание 7: Построить доверительный интервал для средней стоимости скота в предположении, что стоимость скота распределена показательно
#Построим точный доверительный интервал с 5% уровнем значимости
livestock <- Ural$`стоимость скота`
srednee = mean(livestock)
n = length(livestock)
qleft <- qchisq(p=0.025, df=2*n) #левосторонний хи-квадарта
qright <- qchisq(p=1-0.025, df=2*n) #правосторонняя критич точка
rightborder = (2*n*srednee)/qleft
leftborder = (2*n*srednee)/qright
leftborder
rightborder
#Построим ассимптотический доверительный интервал
leftborder1 = srednee/(1 + qnorm(p = 0.025, lower.tail = FALSE)/sqrt(n))
rightborder1 = srednee/(1 - qnorm(p = 0.025, lower.tail = FALSE)/sqrt(n))
leftborder1
rightborder1


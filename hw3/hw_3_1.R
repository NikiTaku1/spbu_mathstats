data <- read.table("C:/Users/Zver/Desktop/uni/матстат/R projects/hw3/babyboom.txt", header = FALSE)

names(data) <- c("Time", "Sex", "Weight", "Minutes")

girls <- subset(data, Sex == 1)$Weight
boys  <- subset(data, Sex == 2)$Weight

# Проверка нормальности
shapiro.test(girls) # p < 0.05, распределение не нормальное
shapiro.test(boys) # p > 0.05, распределение нормальное

# Вилкоксон-тест среднего
wilcox.test(girls, boys)

# F-тест дисперсий
var.test(girls, boys)

# Вывод:
# Для среднего: p-value = 0.3519 > 0.05
# Значимого различия нет, гипотеза принимается
# Для дисперсии: p-value = 0.07526 > 0.05
# Значимого различия нет, гипотеза принимается
if(!require(nortest)) install.packages("nortest")
library(nortest)
if(!require(goftest)) install.packages("goftest")
library(goftest)

# ---- ЧТЕНИЕ ДАННЫХ ----
babyboom <- read.table("C:/Users/Zver/Desktop/uni/матстат/R projects/hw2/babyboom.txt", quote="\"", comment.char="")
colnames(babyboom) <- c("Time24", "Sex", "BirthWeight", "MinutesAfterMidnight")

# Проверка нормальности веса младенцев
weights <- babyboom$BirthWeight
weights_girls <- babyboom$BirthWeight[babyboom$Sex == 1]
weights_boys  <- babyboom$BirthWeight[babyboom$Sex == 2]

# Точечные оценки параметров нормального распределения
mu_hat <- mean(weights)
sigma_hat <- sd(weights)
cat("Вес всех детей:\n")
cat("Среднее:", mu_hat, "\n")
cat("Стандартное отклонение:", sigma_hat, "\n")

mu_hat_g <- mean(weights_girls)
sigma_hat_g <- sd(weights_girls)
cat("Вес девочек:\n")
cat("Среднее:", mu_hat_g, "\n")
cat("Стандартное отклонение:", sigma_hat_g, "\n")

mu_hat_b <- mean(weights_boys)
sigma_hat_b <- sd(weights_boys)
cat("Вес всех детей:\n")
cat("Среднее:", mu_hat_b, "\n")
cat("Стандартное отклонение:", sigma_hat_b, "\n")

# Доверительные интервалы для среднего и SD
n <- length(weights)
n_g <- length(weights_girls)
n_b <- length(weights_boys)
alpha <- 0.05

# Доверительный интервал для среднего
t_crit <- qt(1 - alpha/2, df = n-1)
CI_mean <- c(mu_hat - t_crit*sigma_hat/sqrt(n),
             mu_hat + t_crit*sigma_hat/sqrt(n))

# Доверительный интервал для SD
CI_sd <- c(sqrt((n-1)*sigma_hat^2/qchisq(1-alpha/2, df=n-1)),
           sqrt((n-1)*sigma_hat^2/qchisq(alpha/2, df=n-1)))

cat("Доверительный интервал для среднего веса:", CI_mean, "\n")
cat("Доверительный интервал для SD веса:", CI_sd, "\n")

# Доверительный интервал для среднего (девочки)
t_crit_g <- qt(1 - alpha/2, df = n_g-1)
CI_mean_g <- c(mu_hat_g - t_crit_g*sigma_hat_g/sqrt(n_g),
             mu_hat_g + t_crit_g*sigma_hat_g/sqrt(n_g))

# Доверительный интервал для SD
CI_sd_g <- c(sqrt((n_g-1)*sigma_hat_g^2/qchisq(1-alpha/2, df=n_g-1)),
           sqrt((n_g-1)*sigma_hat_g^2/qchisq(alpha/2, df=n_g-1)))

cat("Доверительный интервал для среднего веса:", CI_mean_g, "\n")
cat("Доверительный интервал для SD веса:", CI_sd_g, "\n")

# Доверительный интервал для среднего (мальчики)
t_crit_b <- qt(1 - alpha/2, df = n_b-1)
CI_mean_b <- c(mu_hat_b - t_crit_b*sigma_hat_b/sqrt(n_b),
               mu_hat_b + t_crit_b*sigma_hat_b/sqrt(n_b))

# Доверительный интервал для SD
CI_sd_b <- c(sqrt((n_b-1)*sigma_hat_b^2/qchisq(1-alpha/2, df=n_b-1)),
             sqrt((n_b-1)*sigma_hat_b^2/qchisq(alpha/2, df=n_b-1)))

cat("Доверительный интервал для среднего веса:", CI_mean_b, "\n")
cat("Доверительный интервал для SD веса:", CI_sd_b, "\n")





cat("Тест нормальности для всех детей:\n")
# Шапиро–Уилка
shapiro_all <- shapiro.test(weights)
print(shapiro_all)

# Тест нормальности для девочек
cat("\nТесты нормальности для девочек:\n")
shapiro_girls <- shapiro.test(weights_girls)
print(shapiro_girls)

# Тест нормальности для мальчиков
cat("\nТесты нормальности для мальчиков:\n")
shapiro_boys <- shapiro.test(weights_boys)
print(shapiro_boys)

# Q-Q plot для визуальной проверки
par(mfrow = c(1,3))  # Три графика в одном ряду
qqnorm(weights, main = "Q-Q Plot: Все дети")
qqline(weights, col = "red", lwd = 2)

qqnorm(weights_girls, main = "Q-Q Plot: Девочки")
qqline(weights_girls, col = "blue", lwd = 2)

qqnorm(weights_boys, main = "Q-Q Plot: Мальчики")
qqline(weights_boys, col = "green", lwd = 2)

# Вывод:
# Все дети: не нормальное, Девочки: не нормальное, Мальчики: нормальное




# Проверка экспоненциального распределения времени между рождениями

# Интервалы между рождениями
birth_times <- sort(babyboom$MinutesAfterMidnight)
intervals <- diff(birth_times)

# Точечная оценка λ для экспоненциального распределения
lambda_hat <- 1/mean(intervals)
cat("Оценка λ для экспоненциального распределения интервалов:", lambda_hat, "\n")

# Тест Андерсона–Дарлинга на экспоненциальное распределение
ad_exp <- ad.test(intervals, null="pexp", rate=lambda_hat)
print(ad_exp)

# Визуальная проверка
par(mfrow = c(1,1))
hist(intervals, breaks=30, probability=TRUE,
     main="Гистограмма интервалов между рождениями",
     xlab="Интервал (минуты)")
curve(dexp(x, rate=lambda_hat), add=TRUE, col="red", lwd=2)
legend("topright", legend="Экспоненциальная плотность", col="red", lwd=2)

# Вывод:
# Распределение экспоненциальное




# Проверка распределения Пуассона количества рождений по часам

babyboom$Hour <- floor(babyboom$MinutesAfterMidnight / 60)
births_per_hour <- table(babyboom$Hour)

# Точечная оценка λ для Пуассона
lambda_poisson <- mean(births_per_hour)
cat("\nОценка λ для распределения Пуассона по часам:", lambda_poisson, "\n")

# Тест хи-квадрат для соответствия распределению Пуассона
observed_counts <- as.numeric(births_per_hour)
expected_probs <- dpois(0:(length(observed_counts)-1), lambda=lambda_poisson)
chisq.test(x = observed_counts, p = expected_probs, rescale.p = TRUE)

# Визуализация сравнения с пуассоновской кривой
# Эмпирическая гистограмма
barplot(births_per_hour, names.arg = names(births_per_hour),
        col = "lightblue", main = "Число рождений по часам vs Пуассон",
        xlab = "Час", ylab = "Количество рождений")

# Теоретические пуассоновские значения
expected_counts <- expected_probs * sum(births_per_hour)
lines(1:length(expected_counts), expected_counts, col="red", lwd=2)

legend("topright", legend=c("Эмпирические данные", "Пуассон"), 
       col=c("lightblue","red"), lwd=c(NA,2), pch=c(15,19))

# Вывод:
# Распределение не Пуассона
# Загрузка данных
dataset <- read.csv("C:/Users/Zver/Desktop/uni/матстат/R projects/hw2/dataset.csv")

alpha <- 0.05

# Проверка нормальности
Height <- dataset$Height
n <- length(Height)
mu_hat <- mean(Height)
sigma_hat <- sd(Height)
cat("\nHeight (вся выборка):\n")
cat("Среднее:", mu_hat, "SD:", sigma_hat, "\n")

# Shapiro-Wilk тест
shapiro_result <- shapiro.test(Height)
print(shapiro_result)

# Q-Q plot
qqnorm(Height, main="Q-Q Plot Height (вся выборка)")
qqline(Height, col="blue", lwd=2)

# Доверительные интервалы
t_crit <- qt(1-alpha/2, df=n-1)
CI_mean <- c(mu_hat - t_crit*sigma_hat/sqrt(n), mu_hat + t_crit*sigma_hat/sqrt(n))
CI_sd <- c(sqrt((n-1)*sigma_hat^2/qchisq(1-alpha/2, df=n-1)),
           sqrt((n-1)*sigma_hat^2/qchisq(alpha/2, df=n-1)))
cat("CI для среднего:", CI_mean, "\n")
cat("CI для SD:", CI_sd, "\n")


# Height по группам Gender
for(g in unique(dataset$Gender)){
  h <- dataset$Height[dataset$Gender == g]
  n_g <- length(h)
  mu_hat <- mean(h)
  sigma_hat <- sd(h)
  cat("\nGender =", g, "\n")
  cat("Среднее:", mu_hat, "SD:", sigma_hat, "\n")
  
  # Shapiro-Wilk тест
  print(shapiro.test(h))
  
  # Q-Q plot
  qqnorm(h, main=paste("Q-Q Plot Height - Gender", g))
  qqline(h, col="blue", lwd=2)
  
  # Доверительные интервалы
  t_crit <- qt(1-alpha/2, df=n_g-1)
  CI_mean <- c(mu_hat - t_crit*sigma_hat/sqrt(n_g), mu_hat + t_crit*sigma_hat/sqrt(n_g))
  CI_sd <- c(sqrt((n_g-1)*sigma_hat^2/qchisq(1-alpha/2, df=n_g-1)),
             sqrt((n_g-1)*sigma_hat^2/qchisq(alpha/2, df=n_g-1)))
  cat("CI для среднего:", CI_mean, "\n")
  cat("CI для SD:", CI_sd, "\n")
}


# Проверка экспоненциального распределения
Time <- dataset$TimeBetweenVisits
lambda_hat <- 1/mean(Time)
cat("\nЭкспоненциальное распределение - оценка λ:", lambda_hat, "\n")

# KS-тест
ks_result <- ks.test(Time, "pexp", rate=lambda_hat)
print(ks_result)

# Гистограмма с кривой
hist(Time, breaks=30, probability=TRUE, main="TimeBetweenVisits", xlab="Days", col="lightblue")
curve(dexp(x, rate=lambda_hat), add=TRUE, col="red", lwd=2)
legend("topright", legend=c("Эмпирические данные", "Экспоненциальное распределение"), fill=c("lightblue", NA), border=c("white", NA), lty=c(NA,1), col=c("black","red"), lwd=2)


# Проверка распределения Пуассона
Steps <- dataset$DailySteps
lambda_pois <- mean(Steps)
cat("\nПуассоновское распределение - оценка λ:", lambda_pois, "\n")

# Преобразуем дискретные значения в «псевдо-непрерывные» с помощью jitter
Steps_jitter <- Steps + runif(length(Steps), 0, 1)

# KS-тест против теоретического пуассоновского распределения
ks_result <- ks.test(Steps_jitter, "ppois", lambda=lambda_pois)
print(ks_result)

# Визуализация сравнения с пуассоновской кривой
barplot(observed_counts, names.arg=names(observed_counts),
        col="lightblue", main="DailySteps vs Пуассон",
        xlab="DailySteps", ylab="Количество наблюдений")
expected_counts <- expected_probs * sum(observed_counts)
lines(1:length(expected_counts), expected_counts, col="red", lwd=2)
legend("topright", legend=c("Эмпирические данные", "Пуассон"), col=c("lightblue","red"), lwd=c(NA,2), pch=c(15,19))

# Вывод: Распределения по росту (общие и по полу) - нормальные
# Распределение по времени визитов - экспоненциальное
# Распределение шагов - Пуассона
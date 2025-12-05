# Загрузка данных
euroweight <- read.table("C:/Users/Zver/Desktop/uni/матстат/R projects/hw2/euroweight.txt", header = FALSE)
euroweight <- euroweight[, -1]

# Переименовываем столбцы
colnames(euroweight) <- c("weight", "batch")

weights <- euroweight$weight
batch <- euroweight$batch


# Нормальность для всех монет

mu_hat <- mean(weights)
sigma_hat <- sd(weights)
cat("Все монеты:\n")
cat("Среднее:", mu_hat, "\n")
cat("Стандартное отклонение:", sigma_hat, "\n")

# KS-тест
ks.test(weights, "pnorm", mean = mu_hat, sd = sigma_hat)

# Q-Q plot
qqnorm(weights, main="Q-Q Plot: Все монеты")
qqline(weights, col="red", lwd=2)

# Доверительные интервалы
n <- length(weights)
alpha <- 0.05
t_crit <- qt(1-alpha/2, df=n-1)
CI_mean <- c(mu_hat - t_crit*sigma_hat/sqrt(n),
             mu_hat + t_crit*sigma_hat/sqrt(n))
CI_sd <- c(sqrt((n-1)*sigma_hat^2/qchisq(1-alpha/2, df=n-1)),
           sqrt((n-1)*sigma_hat^2/qchisq(alpha/2, df=n-1)))

cat("Доверительный интервал для среднего веса:", CI_mean, "\n")
cat("Доверительный интервал для SD веса:", CI_sd, "\n")



# Проверка нормальности по пакетам
batches <- unique(batch)  # уникальные пакеты

for(b in batches){
  w_batch <- weights[batch == b]
  cat("\nПакет:", b, "\n")
  
  # Точечные оценки
  mu_hat_b <- mean(w_batch)
  sigma_hat_b <- sd(w_batch)
  cat("Среднее:", mu_hat_b, "\n")
  cat("Стандартное отклонение:", sigma_hat_b, "\n")
  
  # KS-тест на нормальность
  ks_result <- ks.test(w_batch, "pnorm", mean = mu_hat_b, sd = sigma_hat_b)
  print(ks_result)
  
  # Q-Q plot
  qqnorm(w_batch, main = paste("Q-Q Plot: Пакет", b))
  qqline(w_batch, col = "blue", lwd = 2)
  
  # Доверительные интервалы
  n_b <- length(w_batch)
  alpha <- 0.05
  t_crit_b <- qt(1 - alpha/2, df = n_b - 1)
  
  CI_mean_b <- c(mu_hat_b - t_crit_b * sigma_hat_b / sqrt(n_b),
                 mu_hat_b + t_crit_b * sigma_hat_b / sqrt(n_b))
  CI_sd_b <- c(sqrt((n_b - 1) * sigma_hat_b^2 / qchisq(1 - alpha/2, df = n_b - 1)),
               sqrt((n_b - 1) * sigma_hat_b^2 / qchisq(alpha/2, df = n_b - 1)))
  
  cat("Доверительный интервал для среднего веса:", CI_mean_b, "\n")
  cat("Доверительный интервал для SD веса:", CI_sd_b, "\n")
}


# Вывод:
# Все пакеты: нормальное
# Во всех пакетах по отдельности распределение так же нормальное

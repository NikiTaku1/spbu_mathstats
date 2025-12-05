# -----------------------------
# 1. Загрузка данных
# -----------------------------
iris_data <- read.csv("C:/Users/Zver/Desktop/uni/матстат/R projects/hw2/iris.txt", header = FALSE)
colnames(iris_data) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Class")
iris_data$Class <- as.factor(iris_data$Class)

# Уровень значимости
alpha <- 0.05

# -----------------------------
# 2. Проверка нормальности Sepal.Length по классам с Shapiro-Wilk
# -----------------------------
# Получаем уникальные значения классов
classes <- unique(iris_data$Class)

for(cl in classes){
  sepal <- iris_data$Sepal.Length[iris_data$Class == cl]
  n <- length(sepal)
  
  cat("\nКласс:", cl, "\n")
  
  # Точечные оценки
  mu_hat <- mean(sepal)
  sigma_hat <- sd(sepal)
  cat("Среднее:", mu_hat, "\n")
  cat("Стандартное отклонение:", sigma_hat, "\n")
  
  # Shapiro-Wilk тест
  sw <- shapiro.test(sepal)
  print(sw)
  
  # Q-Q plot
  qqnorm(sepal, main = paste("Q-Q Plot Sepal.Length -", cl))
  qqline(sepal, col = "blue", lwd = 2)
  
  # Доверительные интервалы
  alpha <- 0.05
  t_crit <- qt(1 - alpha/2, df = n - 1)
  CI_mean <- c(mu_hat - t_crit * sigma_hat / sqrt(n),
               mu_hat + t_crit * sigma_hat / sqrt(n))
  CI_sd <- c(sqrt((n - 1) * sigma_hat^2 / qchisq(1 - alpha/2, df = n - 1)),
             sqrt((n - 1) * sigma_hat^2 / qchisq(alpha/2, df = n - 1)))
  
  cat("Доверительный интервал для среднего:", CI_mean, "\n")
  cat("Доверительный интервал для SD:", CI_sd, "\n")
}


# Вывод: по всем классам нормальное распределение
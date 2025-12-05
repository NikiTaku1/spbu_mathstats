# Чтение данных из Excel, начиная с 3-й строки
if(!require(readxl)) install.packages("readxl")
library(readxl)

surgery_data <- read_excel("C:/Users/Zver/Desktop/uni/матстат/R projects/hw3/surgery.xlsx", skip = 2)

# Переименовываем столбцы
names(surgery_data) <- c("Before_V_Right", "Before_V_Left", "After_V_Right", "After_V_Left")

# Создаем бинарную переменную "успех операции"
# Успех = обе стороны после операции больше, чем до, пропуски игнорируем
surgery_data$success <- with(surgery_data,
                             ifelse(is.na(After_V_Right) | is.na(After_V_Left), 
                                    NA, 
                                    (After_V_Right > Before_V_Right) & (After_V_Left > Before_V_Left)))

# Убираем строки с NA в переменной success
surgery_clean <- surgery_data[!is.na(surgery_data$success), ]

# Считаем количество успехов и общее количество наблюдений
n_success <- sum(surgery_clean$success)
n_total <- nrow(surgery_clean)

cat("Количество успехов:", n_success, "\n")
cat("Количество наблюдений (без пропусков):", n_total, "\n")

# 1. Проверка гипотезы p0 = 0.7 через точный биномиальный тест
p0 <- 0.7
test_0.7 <- binom.test(n_success, n_total, p = p0, alternative = "greater")
cat("\nТочный биномиальный тест гипотезы p0 = 0.7\n")
print(test_0.7)

# 2. Проверка гипотезы p0 = 0.8 через точный биномиальный тест
p0 <- 0.8
test_0.8 <- binom.test(n_success, n_total, p = p0, alternative = "greater")
cat("\nТочный биномиальный тест гипотезы p0 = 0.8\n")
print(test_0.8)


# Вывод:
# Для 0.7 p < 0.05, значит гипотезу о равенстве не принимаем
# Для 0.8 p > 0.05, значит гипотезу о равенстве принимаем
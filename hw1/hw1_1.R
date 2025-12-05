airport <- read.table("airportdat1.txt", header = FALSE)
# ==========================
# 1. Гистограммы
# ==========================
par(mfrow = c(2,3)) # сетка для вывода
hist(airport$V3, main = "Scheduled departures",
     xlab = "Количество")
hist(airport$V4, main = "Performed departures",
     xlab = "Количество")
hist(airport$V5, main = "Enplaned passengers",
     xlab = "Количество")
hist(airport$V6, main = "Revenue tons of freight",
     xlab = "Тонны")
hist(airport$V7, main = "Revenue tons of mail",
     xlab = "Тонны")

# ==========================
# 2. Ящики с усами
# ==========================
par(mfrow = c(2,3))
boxplot(airport$V3, main = "Scheduled departures", horizontal = TRUE)
boxplot(airport$V4, main = "Performed departures", horizontal = TRUE)
boxplot(airport$V5, main = "Enplaned passengers", horizontal = TRUE)
boxplot(airport$V6, main = "Revenue tons of freight", horizontal = TRUE)
boxplot(airport$V7, main = "Revenue tons of mail", horizontal = TRUE)

#Усы идут до ближайших точек, не выходящих за 1.5 * IQR от квартилей. Точки за пределами — считаются выбросами и отображаются отдельно.
#Линия внутри - медиана.

# ==========================
# 3. Описательная статистика
# ==========================
summary_stats <- function(x) {
  c(
    mean = mean(x),
    variance = var(x),
    sd = sd(x),
    median = median(x),
    Q1 = quantile(x, 0.25),
    Q3 = quantile(x, 0.75)
  )
}

stats_V3 <- summary_stats(airport$V3)
stats_V4 <- summary_stats(airport$V4)
stats_V5 <- summary_stats(airport$V5)
stats_V6 <- summary_stats(airport$V6)
stats_V7 <- summary_stats(airport$V7)

stats_V3
stats_V4
stats_V5
stats_V6
stats_V7

# ==========================
# 4. Попарные коэффициенты корреляции
# ==========================
cor_matrix <- cor(airport[, c("V3","V4","V5","V6","V7")])
cor_matrix

# Для всех переменных:
# Гистограмма: пик в левой части
# Boxplot: медиана ближе к нижней(левой) границе ящика, верхний(правый) ус длиннее нижнего(левого), много выбросов сверху(справа).
# Соответственно имеем положительную ассиметрию

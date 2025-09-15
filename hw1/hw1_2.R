babyboom <- read.table("babyboom.dat.txt", header = FALSE)
# ==========================
# 1. Гистограммы
# ==========================
par(mfrow = c(1,2))
hist(babyboom$V3, main = "Birth weight in grams",
     xlab = "Вес")
hist(babyboom$V4, main = "Number of minutes after midnight of each birth",
     xlab = "Количество минут")

# ==========================
# 2. Ящики с усами
# ==========================
par(mfrow = c(1,2))
boxplot(babyboom$V3, main = "Birth weight in grams")
boxplot(babyboom$V4, main = "Number of minutes after midnight of each birth")

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

stats_V3 <- summary_stats(babyboom$V3)
stats_V4 <- summary_stats(babyboom$V4)

stats_V3
stats_V4


# ==========================
# 4. Попарные коэффициенты корреляции
# ==========================
cor_matrix <- cor(babyboom[, c("V3","V4")])
cor_matrix

# Гистограмма: пик ближе к правому краю
# Boxplot: медиана ближе к верхней границе ящика, нижний ус длиннее верхнего, несколько выбросов снизу.

# Соответственно имеем отрицательную ассиметрию
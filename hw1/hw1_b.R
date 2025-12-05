# --- Загрузка данных ---
df <- read.csv("dataset.csv")

# Просмотр структуры
str(df)


# --- Гистограммы (для каждого числового признака) ---
par(mfrow=c(1,3))

hist(df$Age, main="Age", xlab="Age")
hist(df$Salary, main="Salary", xlab="Salary")
hist(df$Experience, main="Experience", xlab="Experience")


# --- Ящики с усами ---
par(mfrow=c(1,3))

boxplot(df$Age, main="Age")
boxplot(df$Salary, main="Salary")
boxplot(df$Experience, main="Experience")

#Усы идут до ближайших точек, не выходящих за 1.5 * IQR от квартилей. Точки за пределами — считаются выбросами и отображаются отдельно.
#Линия внутри - медиана.

# --- Описательная статистика ---
summary(df[, c("Age","Salary","Experience")])

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
statsAge <- summary_stats(df$Age)
statsSalary <- summary_stats(df$Salary)
statsExp <- summary_stats(df$Experience)

statsAge
statsSalary
statsExp

library(e1071)
skewness(df$Salary)
skewness(df$Experience)

# --- Матрица корреляций ---
cor(df[, c("Age","Salary","Experience")])

# Age:
# Гистограмма: значения немного смещены вправо.
# Boxplot: два выброса, один сверху, один снизу, нижний ус чуть длиннее верхнего.
# Имеем распределение близкое к нормальному, с небольшой левосторонней ассиметрией

# Salary: 
# Гистограмма показывает небольшую левостороннюю ассиметрию
# Boxplot: верхний ус длиннее, выбросов нет
# Левосторонняя ассиметрия

# Experience:
# Гистограмма: значения немного смещены влево.
# Boxplot: по два выброса с обеих сторон, усы примерно равны
# Распределение близкое к нормальному, с небольшой правосторонней ассиметрией


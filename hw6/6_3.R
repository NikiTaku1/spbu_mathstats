library(readxl)

# --- 1. Загрузка данных ---
data <- read_excel("dataset3.xlsx")

# Преобразуем переменные в числовые, если нужно
data$Minutes <- as.numeric(data$Minutes)
data$Intensity <- as.numeric(data$Intensity)
data$Efficiency <- as.numeric(data$Efficiency)
data$Calories <- as.numeric(data$Calories)
data$WorkoutType <- factor(data$WorkoutType)

# --- 1. Построение моделей ---

# Линейная модель
lin_model <- lm(Minutes ~ Intensity + Efficiency + Calories + WorkoutType, data = data)
summary(lin_model)

#Очень хорошо объясняет данные (R² ≈ 0.975). 
#Значимые предикторы — Intensity и Calories. 
#Категориальная переменная WorkoutType и Efficiency незначимы. 

# Квадратичная модель
quad_model <- lm(Minutes ~ Intensity + I(Intensity^2) +
                   Efficiency + I(Efficiency^2) +
                   Calories + I(Calories^2) +
                   WorkoutType,
                 data = data)
summary(quad_model)

#Небольшое улучшение R² (≈ 0.979), но коэффициенты незначимы, 
#ошибка не уменьшается. 
#Добавление квадратичных членов не даёт существенного выигрыша.

# Кубическая модель
cubic_model <- lm(Minutes ~ poly(Intensity, 3, raw = TRUE) +
                    poly(Efficiency, 3, raw = TRUE) +
                    poly(Calories, 3, raw = TRUE) +
                    WorkoutType,
                  data = data)
summary(cubic_model)

#Наилучшее соответствие данным (R² ≈ 0.986). 
#Значимые нелинейные эффекты у Intensity и Calories. 
#Ошибка предсказания уменьшается, но модель сложнее.

# --- 2. Сравнение моделей по R² и AIC ---
model_comparison <- data.frame(
  Model = c("Linear", "Quadratic", "Cubic"),
  R_2 = c(summary(lin_model)$r.squared,
                summary(quad_model)$r.squared,
                summary(cubic_model)$r.squared),
  Adjusted_R_2 = c(summary(lin_model)$adj.r.squared,
                         summary(quad_model)$adj.r.squared,
                         summary(cubic_model)$adj.r.squared),
  AIC = c(AIC(lin_model), AIC(quad_model), AIC(cubic_model))
)

#По R² (и по скорректированному тоже): Кубическая > Квадратичная > Линейная.
#По AIC (лучше меньше): Кубическая модель имеет наименьшее AIC.
#Вывод: Если цель — максимальная точность предсказания, кубическая модель предпочтительнее,
#хотя линейная модель гораздо проще и не сильно хуже.

print(model_comparison)


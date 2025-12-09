library(readxl)
library(dplyr)

devtools::install_github("selva86/InformationValue")
library(InformationValue)
df <- read_excel("Doctor.xlsx")

# Меняем запятые в числовых колонках на точки
num_cols <- c("x1","x2","x3","x5")

df <- df %>%
  mutate(across(all_of(num_cols),
                ~ as.numeric(gsub(",", ".", as.character(.)))))

#-----•	Построить описательную статистику с помощью функции summary-----
# Проверка структуры
str(df)
# Описательная статистика
summary(df)

#-----•	При необходимости создать категориальную переменную (функция factor).
# Исход беременности (если нужна факторная версия)
#df$yf <- factor(df$y, levels = c(0,1), labels = c("No","Yes"))
# x4 — категориальная (1, 2, 3)
df$x4 <- factor(df$x4)

#-----•	Построить базовую модель логистической регрессии с максимально возможным количеством предикторов с использованием функции glm.
# Строим логистическую модель: зависимая переменная Y,
# все остальные — предикторы.
model_logit_full <- glm(
  y ~ x1 + x2 + x3 + x4 + x5,
  data = df,
  family = binomial(link = "logit")
)

#-----•	Записать уравнение бинарной регрессии, используя оценки коэффициентов.
# Выводим оценки коэффициентов модели
coef(model_logit_full)
#P(x4 = 1) = e^(beta*x) / 1+e^(beta*x)
#beta*x = -10.70094869 + 0.45959127*x1 + 0.81826262*x2 - 0.37896655*x3 - 2.14618399*I{x4=2} + 0.22638513*I{x4=3} - 0.03382537*x5

#-----•	Протестировать значимость коэффициентов регрессии в отдельности.
summary(model_logit_full)$coefficients
#x3,x42,x43 - не значимы, x2,x5 - не значимы на 0.05, I,x1 - значимы

#-----•	Проверить значимость регрессии в целом по критерию Вальда и максимального правдоподобия.
if(!requireNamespace("aod", quietly = TRUE)) install.packages("aod")
library(aod)

# Wald test для всех коэффициентов, кроме интерсепта
wald.test(Sigma = vcov(model_logit_full), 
          b = coef(model_logit_full), 
          Terms = 2:length(coef(model_logit_full)))
#H0: beta1,...5 = 0, p<0.05 - отвергаем, модель объясняет переменную

#-----•	Построить доверительные интервалы для коэффициентов регрессии (функции  confint и confint.default).
confint(model_logit_full)
#x3,x42,x43 - не значимы (включен 0), I, x5 - значим отрицательно, x1,x2 - значимы положительно

#-----•	Провести сравнительный анализ логит и пробит моделей.
# Строим probit-модель с теми же предикторами
model_probit_full <- glm(
  y ~ x1 + x2 + x3 + x4 + x5,
  data = df,
  family = binomial(link = "probit")
)

AIC(model_logit_full, model_probit_full)
#Пробит модель формально лучше (dAIC~0,011), но по сути модели эквивалентны
coef(model_probit_full)
#Ф(beta*x) = integral{-inf, beta*x}(1/sqrt(2pi) * e^(-t^2/2) * dt)
#beta*x = -6.12853437 + 0.26397172*x1 + 0.47466412*x2 - 0.23145724*x3 - 1.23190498*I{x4=2} + 0.09670252*I{x4=3} - 0.01906408*x5
summary(model_probit_full)$coefficients
#x3,x42,x43 - не значимы, x5 - не значимы на 0.05, I,x1,x2 - значимы
wald.test(Sigma = vcov(model_probit_full), 
          b = coef(model_probit_full), 
          Terms = 2:length(coef(model_probit_full)))
#H0: beta1,...5 = 0, p<0.05 - отвергаем, модель объясняет переменную
confint(model_probit_full)
#x3,x42,x43 - не значимы (включен 0), I, x5 - значим отрицательно, x1,x2 - значимы положительно

#-----•	Построить таблицу сопряженности с пороговой вероятностью 0.5 (функция  confusionMatrix).
#update.packages(ask = FALSE, checkBuilt = TRUE)
#install.packages("caret", dependencies = TRUE)

# Предсказанные вероятности
pred_prob_logit <- as.numeric(predict(model_logit_full, type = "response"))
pred_prob_probit <- as.numeric(predict(model_probit_full, type = "response"))

# Фактические значения
actual <- as.numeric(df$y)

# Логит-модель: таблица сопряженности при пороге 0.5
conf_matrix_logit_0.5 <- confusionMatrix(actuals = actual, predictedScores = pred_prob_logit, threshold = 0.5)
conf_matrix_logit_0.5

# Пробит-модель: таблица сопряженности при пороге 0.5
conf_matrix_probit_0.5 <- confusionMatrix(actuals = actual, predictedScores = pred_prob_probit, threshold = 0.5)
conf_matrix_probit_0.5

#Логит:
#Accuracy: (TP + TN) / всего = (34 + 16) / (16+3+5+34) = 50 / 58 ≈ 0.862
#Sensitivity: TP / (TP + FN) = 34 / (34 + 3) ≈ 0.9189
#Модель хорошо выявляет положительные исходы.
#Specificity (специфичность): TN / (TN + FP) = 16 / (16 + 5) ≈ 0.7619
#Пробит:
#Accuracy = (TN + TP) / всего = (17 + 34) / 58 ≈ 0.879
#Sensitivity = TP / (TP + FN) = 34 / (34 + 3) ≈ 0.9189
#Specificity = TN / (TN + FP) = 17 / (17 + 4) ≈ 0.8095


#-----•	Посчитать специфичность и чувствительность модели (функции  sensitivity и specificity).
# Логит-модель
sens_logit_0.5 <- sensitivity(actual, pred_prob_logit, threshold = 0.5)
spec_logit_0.5 <- specificity(actual, pred_prob_logit, threshold = 0.5)

# Пробит-модель
sens_probit_0.5 <- sensitivity(actual, pred_prob_probit, threshold = 0.5)
spec_probit_0.5 <- specificity(actual, pred_prob_probit, threshold = 0.5)

# Вывод
sens_logit_0.5; spec_logit_0.5
sens_probit_0.5; spec_probit_0.5

#Sensitivity — доля правильно предсказанных положительных исходов y=1
#Specificity — доля правильно предсказанных отрицательных исходов y=0
#Оба показателя позволяют оценить способность модели различать классы.
#Пробит-модель чуть лучше распознаёт отрицательные исходы (специфичность выше)
#Чувствительность одинакова для обеих моделей

#-----•	Найти оптимальное пороговое значение вероятности предсказания. Построить таблицу сопряженности для этой вероятности, посчитать специфичность и чувствительность модели (функция  optimalCutoff).

# Логит-модель: оптимальный порог
opt_cut_logit <- optimalCutoff(actual, pred_prob_logit, optimiseFor = "Both")
conf_matrix_logit_opt <- confusionMatrix(actuals = actual, predictedScores = pred_prob_logit, threshold = opt_cut_logit)
sens_logit_opt <- sensitivity(actual, pred_prob_logit, threshold = opt_cut_logit)
spec_logit_opt <- specificity(actual, pred_prob_logit, threshold = opt_cut_logit)

# Пробит-модель: оптимальный порог
opt_cut_probit <- optimalCutoff(actual, pred_prob_probit, optimiseFor = "Both")
conf_matrix_probit_opt <- confusionMatrix(actuals = actual, predictedScores = pred_prob_probit, threshold = opt_cut_probit)
sens_probit_opt <- sensitivity(actual, pred_prob_probit, threshold = opt_cut_probit)
spec_probit_opt <- specificity(actual, pred_prob_probit, threshold = opt_cut_probit)

# Вывод результатов
opt_cut_logit; conf_matrix_logit_opt; sens_logit_opt; spec_logit_opt
opt_cut_probit; conf_matrix_probit_opt; sens_probit_opt; spec_probit_opt

#Показатели моделей оказались одинаковыми, но у пробит порг ниже


#-----•	Если выборка большого объема, то можно предварительно разбить выборку на две части: тренировочная и тестовая выборки (функция  sample_frac).
target_n <- 10000  # желаемый размер
current_n <- nrow(df)
df_big <- df

# Генерируем новые строки с небольшими вариациями, чтобы не было точных копий
while(nrow(df_big) < target_n){
  row_sample <- df[sample(1:current_n, 1), ]
  
  # Добавляем шум ±5% для числовых переменных
  row_sample$x1 <- row_sample$x1 * runif(1, 0.95, 1.05)
  row_sample$x2 <- row_sample$x2 * runif(1, 0.95, 1.05)
  row_sample$x3 <- row_sample$x3 * runif(1, 0.95, 1.05)
  row_sample$x5 <- row_sample$x5 * runif(1, 0.95, 1.05)
  
  # x4 оставляем фактором или при желании случайно меняем уровень
  # row_sample$x4 <- sample(levels(df$x4), 1)
  
  # Добавляем новую строку
  df_big <- bind_rows(df_big, row_sample)
}

# Преобразуем x4 в фактор с фиксированными уровнями
all_levels_x4 <- sort(unique(df_big$x4))
df_big <- df_big %>% mutate(x4 = factor(x4, levels = all_levels_x4))

# Разбиение на тренировочную и тестовую выборки
train_data <- df_big %>% sample_frac(0.7)
test_data  <- df_big %>% filter(!row_number() %in% row_number(train_data))

# Восстанавливаем уровни фактора x4 в обеих выборках
train_data$x4 <- factor(train_data$x4, levels = all_levels_x4)
test_data$x4  <- factor(test_data$x4, levels = all_levels_x4)

#-----•	Построить ROC кривую, интерпретировать результаты (функция  plotROC).

pred_prob_test_logit <- predict(model_logit_full, newdata = test_data, type = "response")
pred_prob_test_probit <- predict(model_probit_full, newdata = test_data, type = "response")

# ROC-кривая и AUC — логит
plotROC(actuals = test_data$y, predictedScores = pred_prob_test_logit)
auc_logit <- AUROC(test_data$y, pred_prob_test_logit)
cat("AUC логит модели:", auc_logit, "\n")

# ROC-кривая и AUC — пробит
plotROC(actuals = test_data$y, predictedScores = pred_prob_test_probit)
auc_probit <- AUROC(test_data$y, pred_prob_test_probit)
cat("AUC пробит модели:", auc_probit, "\n")

#AUROC примерно 91,5-92% на 10000 строках что свидетельствует о хорошей модели


#-----•	Попробовать улучшить логит или пробит модель с использованием коэффициента AIC (функция  stepAIC).
library(MASS)

# Шаговый отбор по AIC (в обе стороны)
model_logit_step <- stepAIC(model_logit_full, direction = "both")
# Результаты
summary(model_logit_step)
# Сравнение AIC
cat("AIC исходной модели:", AIC(model_logit_full), "\n")
cat("AIC улучшенной модели:", AIC(model_logit_step), "\n")

# Шаговый отбор по AIC
model_probit_step <- stepAIC(model_probit_full, direction = "both")
# Результаты
summary(model_probit_step)
# Сравнение AIC
cat("AIC исходной пробит модели:", AIC(model_probit_full), "\n")
cat("AIC улучшенной пробит модели:", AIC(model_probit_step), "\n")

#Обе модели улучшены по критерию AIC.
#Улучшенная логит-модель немного лучше (AIC = 48.62 vs 48.79), но разница минимальна.

pred_prob_logit <- predict(model_logit_full, newdata = test_data, type = "response")
pred_prob_probit <- predict(model_probit_full, newdata = test_data, type = "response")
pred_prob_logit_step <- predict(model_logit_step, newdata = test_data, type = "response")
pred_prob_probit_step <- predict(model_probit_step, newdata = test_data, type = "response")

# Оптимальный порог
opt_cut_logit <- optimalCutoff(test_data$y, pred_prob_logit)[1]
opt_cut_probit <- optimalCutoff(test_data$y, pred_prob_probit)[1]
opt_cut_logit_step <- optimalCutoff(test_data$y, pred_prob_logit_step)[1]
opt_cut_probit_step <- optimalCutoff(test_data$y, pred_prob_probit_step)[1]

# Классификация
pred_class_logit <- ifelse(pred_prob_logit > opt_cut_logit, 1, 0)
pred_class_probit <- ifelse(pred_prob_probit > opt_cut_probit, 1, 0)
pred_class_logit_step <- ifelse(pred_prob_logit_step > opt_cut_logit_step, 1, 0)
pred_class_probit_step <- ifelse(pred_prob_probit_step > opt_cut_probit_step, 1, 0)

# Confusion matrix
confusionMatrix(actuals = test_data$y, predictedScores = pred_class_logit, threshold = opt_cut_logit)
confusionMatrix(actuals = test_data$y, predictedScores = pred_class_probit, threshold = opt_cut_probit)
confusionMatrix(actuals = test_data$y, predictedScores = pred_class_logit_step, threshold = opt_cut_logit_step)
confusionMatrix(actuals = test_data$y, predictedScores = pred_class_probit_step, threshold = opt_cut_probit_step)

# Чувствительность и специфичность
sensitivity(factor(pred_class_logit), test_data$y)
specificity(factor(pred_class_logit), test_data$y)
sensitivity(factor(pred_class_probit), test_data$y)
specificity(factor(pred_class_probit), test_data$y)
sensitivity(factor(pred_class_logit_step), test_data$y)
specificity(factor(pred_class_logit_step), test_data$y)
sensitivity(factor(pred_class_probit_step), test_data$y)
specificity(factor(pred_class_probit_step), test_data$y)


# ROC и AUC — логит улучшенная
plotROC(actuals = test_data$y, predictedScores = pred_prob_logit_step)
auc_logit_step <- AUROC(test_data$y, pred_prob_logit_step)
cat("AUC улучшенной логит модели:", auc_logit_step, "\n")

# ROC и AUC — пробит улучшенная
plotROC(actuals = test_data$y, predictedScores = pred_prob_probit_step)
auc_probit_step <- AUROC(test_data$y, pred_prob_probit_step)
cat("AUC улучшенной пробит модели:", auc_probit_step, "\n")

#AIC снизился на 4-5 единиц
# AUROC снизился, но на ROC-кривых появились точки ближе к левому верхнему углу
#Это значит, что при оптимальном пороге модель точнее предсказывает реальные исходы
# что важнее, чем средняя производительность по всем порогам.
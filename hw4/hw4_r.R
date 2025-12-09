# Установка и подключение пакетов
#install.packages("GGally")
library(GGally)
library(ggplot2)

# Загрузка данных
library(readxl)
data <- read_excel("dataset.xls")

# -----a.	Провести корреляционный анализ имеющихся данных-----
# ggpairs — матрица scatterplot с гистограммами
ggpairs(data[, c("Investment", "Term", "RiskLevel", "Rate", "Guarantee")])
# Каждая диагональная ячейка показывает гистограмму переменной.
# Верхняя и нижняя треугольники — scatterplots между каждой парой переменных.
# Если точки на scatterplot показывают чёткую линейную зависимость, это сигнал о сильной корреляции.

# ggcorr — корреляционная матрица с визуализацией
ggcorr(data[, c("Investment", "Term", "RiskLevel", "Rate", "Guarantee")],
       method = c("everything", "pearson"),
       label = TRUE)
# Показывает коэффициенты корреляции Пирсона между всеми парами переменных.

#-----b.	Построить базовую модель линейной регрессии-----
base_model <- lm(Investment ~ Term + RiskLevel + Rate + Guarantee, data = data)

#-----c. Вывести результаты анализа базовой модели-----
summary(base_model)
#Остатки показывают, насколько предсказанные значения Investment отличаются от реальных.
#Медиана близка к нулю (-1960), что хорошо.
#Разброс остатков большой, особенно Max = 35817, что может указывать на выбросы.

#Residual standard error: 7659 -> средняя ошибка предсказания около 7.6 тыс.
#Multiple R-squared: 0.4026 -> модель объясняет ~40% вариации цены.
#Adjusted R-squared: 0.3996 -> почти то же, учитывая число переменных.

#-----d.	Записать уравнение линейной регрессии.-----
#Investment = 2799.86−0.164*Term+3007.80*RiskLevel+455.73*Rate+6076.05*Guarantee

#-----e.	Проверить значимость каждого отдельного коэффициента с помощью  T-test-----
#из summary():
#Term: коэффициент отрицательный (-0.164), значим (***), чем больше пробег, тем ниже цена.
#RiskLevel: положительный (3007.8), значим (***), больше цилиндров -> выше цена.
#Rate: коэффициент положительный, но не значим (p = 0.597).
#Guarantee: сильная положительная связь, значим (***).
#Intercept: незначим на уровне 0.05 (p = 0.070).

#-----f.	Проверить значимость построенного уравнения регрессии с помощью F-test-----
#F-statistic: 134.6 on 4 and 799 DF,  p-value: < 2.2e-16
#Пояснение: H0: coeff 1-4 = 0, p<0.05: отвергаем, значит модель значима

#-----g.	Построить график рассеяния и уравнения регрессии-----
# Добавляем предсказанные значения модели
data$Predicted <- predict(base_model)

ggplot(data, aes(x = Predicted, y = Investment)) +
  geom_point(color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Фактические vs Предсказанные значения Investment",
       x = "Predicted Investment",
       y = "Actual Investment") +
  theme_minimal()

#-----h. Построить доверительные интервалы для коэффициентов регрессии-----
confint(base_model)
# Интервалы показывают точность оценки коэффициентов: чем уже интервал, тем надёжнее оценка.
# Если интервал включает 0, переменная не значима.

#-----i.	В случае подозрения на наличие выбросов, проверить так называемые важные наблюдения, 
#-----которые значительно влияют на построение модели.
# Получаем меры влияния
infl <- influence.measures(base_model)
# Преобразуем в data.frame для удобства
infl_df <- as.data.frame(infl$is.inf)
# Выбираем наблюдения, у которых * есть в cook.d или hat
# В столбцах TRUE/FALSE хранится, превышает ли мера порог
# cook.d = столбец "cook.d", hat = столбец "hat"
influential_points <- which(infl_df$cook.d | infl_df$hat)
# Выводим номера наблюдений
influential_points


#-----j.	Используя функцию Step или StepAIC, постараться улучшить модель------
# Установка и подключение пакета
#install.packages("MASS")
library(MASS)

# Шаговый отбор по AIC
step_model <- stepAIC(base_model, direction = "both")

#-----k.	В случае получения в предыдущем пункте модели,-----------
#-----отличной от базовой, повторить пп. c-i для новой модели.-----
summary(step_model) #c-f
data$Predicted <- predict(step_model)
#Investment=2332.52−0.164*Term+3350.15*RiskLevel+6126.56*Guarantee
# График фактические vs предсказанные
ggplot(data, aes(x = Predicted, y = Investment)) +
  geom_point(color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Фактические vs Предсказанные значения (новая модель)",
       x = "Predicted Investment", y = "Actual Investment") +
  theme_minimal() #g
confint(step_model) #h
infls <- influence.measures(step_model)
infls_df <- as.data.frame(infls$is.inf)
influential_points <- which(infls_df$cook.d | infls_df$hat)
influential_points #i

#-----l.	Построить графики: scatterplot, "Residuals vs Fitted",------
#-----"Normal Q-Q" , "Residuals vs Leverage" и дать интерпретации-----
# Построение стандартных диагностических графиков для lm
plot(step_model)
# Residuals vs Fitted: модель хорошо предсказывает значения до 20т,
# немного недооценивает до 30к и начинает переоценивать после 30т
# Q-Q: модель хорошо предсказывает средние значения, но не справляется
# с наименьшими и особенно наибольшими значениями
# Residuals vs Leverage: значений слева/справа сверху нет, важные значения: 151, 152, 153

#-----m.	Проверить модель на наличие выбросов-----
#install.packages("car")
library(car)
outlier_test <- outlierTest(step_model)
outlier_test
# Значения 151-156 - выбросы

#-----n.	Проверить модель на гетероскедастичность-----
#install.packages("lmtest")
library(lmtest)
# Тест Бройша-Пагана
bptest(step_model)
# H0: дисперсия остатков постоянна (гомоскедастичность). p<0.05 - отвергаем, гетероскедастичность.

#-----o.	Проверить остатки модели на автокорреляцию-----
# Тест Дарбина-Уотсона
dwtest(step_model)
#H0: нет автокорреляции остатков. H1: существует положительная автокорреляция.
#DW = 0.2201 << 2 -> сильная положительная автокорреляция остатков.
#p-value < 0.05 -> нулевая гипотеза отвергается.

#-----p.	Проверить остатки  модели на нормальность распределения-----
#install.packages("olsrr")
library(olsrr)
# Проверка нормальности остатков
ols_test_normality(step_model)
#Все p-value = 0.0000 < 0.05 -> нулевая гипотеза нормальности остатков отвергается.

#-----q.	Проверить модель на мультиколлинеарность данных-----
# Проверка мультиколлинеарности
vif(step_model)
#Значения меньше 5, независимые переменные практически не коррелируют между собой.

#-----r.	Попробовать применить трансформацию Box-Cox зависимой переменной-----
bc <- boxcox(step_model)
# bc возвращает список с x = lambda и y = log-likelihood
lambda <- bc$x       # все проверенные λ
logLik <- bc$y       # соответствующие log-likelihood
# Находим индекс максимального log-likelihood
idx_max <- which.max(logLik)
# Оптимальное λ
lambda_opt <- lambda[idx_max]

# Создаём трансформированную зависимую переменную
data$Investment_tf <- (data$Investment^(lambda_opt) - 1)/(lambda_opt)

# Новая линейная модель
boxcox_model <- lm(Investment_tf ~ Term + RiskLevel + Guarantee, data = data)

#-----s.	В случае получения новой модели в предыдущем пункте проанализировать новую модель.-----
summary(base_model) #c
#Multiple R-squared = 0.4688 -> модель объясняет ~ 47% дисперсии Investment_tf (на 7% лучше).
#Investment_tf=1.592−1.487e-8*Term+2.661e−4*RiskLevel+6.762e−4*Guarantee (d)
#Все коэффициенты значимы (p < 0.001), даже Term, который ранее был менее значимым.(e)
#F = 235.3, p-value < 2.2e-16 -> модель значимо объясняет зависимую переменную.(f)
data$Predicted_tf <- predict(boxcox_model)

ggplot(data, aes(x = Predicted_tf, y = Investment_tf)) +
  geom_point(color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Фактические vs Предсказанные значения Investment_tf (Box-Cox)",
       x = "Predicted Investment_tf",
       y = "Actual Investment_tf") +
  theme_minimal() #g
confint(base_model) #h

infl_tf <- influence.measures(boxcox_model)
infl_tf_df <- as.data.frame(infl_tf$is.inf)
influential_points_tf <- which(infl_tf_df$cook.d | infl_tf_df$hat)
influential_points_tf #i

plot(boxcox_model) #l
outlier_test_tf <- outlierTest(boxcox_model)
outlier_test_tf #m

bptest(boxcox_model) #n
dwtest(boxcox_model) #o
ols_test_normality(boxcox_model) #p
vif(step_model) #q

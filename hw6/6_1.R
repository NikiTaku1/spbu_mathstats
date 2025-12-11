library(readxl)
library(lmridge)
library(car)

run_ridge_analysis <- function(file_path = "dataset1.xlsx") {
  
  # -----------------------------
  # Загрузка данных
  # -----------------------------
  df <- suppressMessages(read_excel(file_path))
  df <- na.omit(df[c("Measure","Age","Weight","Activity")])
  df$ActivityLevel <- as.factor(df$Activity)
  
  names(df) <- c("X1","X2","X3","X4")  # X4 — категориальный фактор
  
  # -----------------------------
  # 1) Ridge-регрессия + выбор k
  # -----------------------------
  k_grid <- seq(0,1,length.out=201)
  ridge_all <- lmridge(X1 ~ X2 + X3 + X4, data=df, K=k_grid)
  
  cv.plot(ridge_all)
  #На оси X — значения k, на оси Y — CV-ошибка
  bias.plot(ridge_all)
  #На оси X — значения k, на оси Y - ошибка
  #Var - Дисперсия оценок коэффициентов
  #Bias^2 - Квадрат смещения (отклонение от OLS)
  #MSE=Var+Bias^2 - Общая ошибка предсказания
  
  # Выбираем k вручную из графиков
  k_cv <- 0.015
  k_bias <- 0.045
  
  ridge_cv <- lmridge(X1 ~ X2 + X3 + X4, data=df, K=k_cv)
  ridge_bias <- lmridge(X1 ~ X2 + X3 + X4, data=df, K=k_bias)
  
  X <- model.matrix(X1 ~ X2 + X3 + X4, data=df)
  rmse <- function(a,b) sqrt(mean((a-b)^2))
  
  rmse_cv <- rmse(df$X1, X %*% coef(ridge_cv))
  rmse_bias <- rmse(df$X1, X %*% coef(ridge_bias))
  
  if (rmse_cv <= rmse_bias) {
    ridge_final <- ridge_cv
    k_final <- k_cv
    best_type <- "CV"
  } else {
    ridge_final <- ridge_bias
    k_final <- k_bias
    best_type <- "Bias"
  }
  
  cat("Best model:",best_type,"\n")
  
  # -----------------------------
  # 2) Проверка VIF (OLS)
  # -----------------------------
  #vif(ridge_final)
  lm_mod <- lm(X1 ~ X2 + X3 + X4, data=df)
  vif_vals <- vif(lm_mod)
  
  #X2 (Age) и X3 (Height/Weight) имеют очень высокий VIF > 10, 
  #что свидетельствует о сильной мультиколлинеарности между ними.
  #Это означает, что признаки X2 и X3 сильно линейно зависимы, 
  #что делает коэффициенты в OLS нестабильными.
  #Категориальная переменная X4 имеет VIF = 1.62 → проблем нет.
  # -----------------------------
  # 3) Значимость уравнения
  # -----------------------------
  lm_summary <- summary(lm_mod)
  fstat <- lm_summary$fstatistic
  f_pvalue <- pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
  
  #Оценка модели:
  #R² = 0.9797
  #Adj R² = 0.9783
  #Модель объясняет ≈ 98% вариации зависимой переменной Measure
  #F-тест модели:
  #p-value = 4.71e-38
  #Значение намного меньше 0.05 → модель в целом статистически значима.
  
  # -----------------------------
  # 4) Сравнение моделей
  # -----------------------------
  coef_ridge_final <- coef(ridge_final)
  pred_ridge <- as.numeric(X %*% coef_ridge_final)
  pred_lm <- predict(lm_mod)
  
  rmse_lm <- rmse(df$X1, pred_lm)
  rmse_ridge <- rmse(df$X1, pred_ridge)
  
  coef_table <- data.frame(
    term = names(coef(lm_mod)),
    OLS = as.numeric(coef(lm_mod)),
    Ridge_Final = as.numeric(coef_ridge_final[names(coef(lm_mod))])
  )
  
  #Ridge уменьшает величину коэффициента X2 — признак мультиколлинеарности.
  #Коэффициент X3 также слегка сдвигается.
  #X4 сжался почти к нулю, что соответствует его статистической незначимости в OLS.
  #Разница RMSE минимальна (0.0037)
  #По точности предсказания модели практически одинаковы.
  #Ridge немного проигрывает OLS по RMSE, но обеспечивает более устойчивые коэффициенты при сильной мультиколлинеарности.
  
  # -----------------------------
  # Вывод
  # -----------------------------
  cat("1) Выбран k =", k_final, "(", best_type, ")\n\n")
  cat("2) VIF:\n"); print(vif_vals); cat("\n")
  cat("3) OLS summary:\n"); print(lm_summary)
  cat("   F-stat p-value =", f_pvalue, "\n\n")
  cat("4) Коэффициенты и RMSE:\n")
  print(coef_table)
  cat(sprintf("\nRMSE OLS = %.4f; RMSE Ridge = %.4f\n", rmse_lm, rmse_ridge))
  cat(sprintf("RMSE CV-opt = %.4f; RMSE Bias-opt = %.4f\n", rmse_cv, rmse_bias))
}
run_ridge_analysis("dataset1.xlsx")

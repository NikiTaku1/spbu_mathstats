# Чтение данных и переименование столбцов
iris_data <- read.csv("C:/Users/Zver/Desktop/uni/матстат/R projects/hw3/iris.txt", header = FALSE)
names(iris_data) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "class")

features <- c("sepal_length", "sepal_width", "petal_length", "petal_width")
classes <- unique(iris_data$class)

if(!require(car)) install.packages("car")
if(!require(dplyr)) install.packages("dplyr")
library(car)
library(dplyr)

# Функция для проверки нормальности всех групп
is_normal <- function(feature, data, classes){
  for(cls in classes){
    if(shapiro.test(data[data$class == cls, feature])$p.value < 0.05){
      return(FALSE)
    }
  }
  return(TRUE)
}

cat("=== Анализ характеристик ===\n")

for(f in features){
  cat("\n--- Feature:", f, "---\n")
  
  normal_all <- is_normal(f, iris_data, classes)
  
  # 1. Сравнение распределений
  cat("\nСравнение распределений:\n")
  kruskal_test <- kruskal.test(as.formula(paste(f, "~ class")), data = iris_data)
  cat("Kruskal-Wallis (все классы) p-value =", kruskal_test$p.value, "\n")
  
  cat("Попарные Wilcoxon тесты (распределения):\n")
  for(i in 1:(length(classes)-1)){
    for(j in (i+1):length(classes)){
      grp1 <- iris_data[iris_data$class == classes[i], f]
      grp2 <- iris_data[iris_data$class == classes[j], f]
      wilcox_test <- wilcox.test(grp1, grp2)
      cat(classes[i], "vs", classes[j], "- p-value =", wilcox_test$p.value, "\n")
    }
  }
  
  # 2. Сравнение средних
  cat("\nСравнение средних:\n")
  if(normal_all){
    # ANOVA для всех классов
    aov_test <- aov(as.formula(paste(f, "~ class")), data = iris_data)
    p_val <- summary(aov_test)[[1]][["Pr(>F)"]][1]
    cat("ANOVA (все классы) p-value =", p_val, "\n")
    
    # Попарные t-тесты
    cat("Попарные t-тесты:\n")
    for(i in 1:(length(classes)-1)){
      for(j in (i+1):length(classes)){
        grp1 <- iris_data[iris_data$class == classes[i], f]
        grp2 <- iris_data[iris_data$class == classes[j], f]
        t_test <- t.test(grp1, grp2, var.equal = TRUE)
        cat(classes[i], "vs", classes[j], "- p-value =", t_test$p.value, "\n")
      }
    }
  } else {
    # Для ненормальных данных используем Kruskal-Wallis как общий тест для медиан/средних
    kruskal_test_mean <- kruskal.test(as.formula(paste(f, "~ class")), data = iris_data)
    cat("Общее сравнение средних p-value =", kruskal_test_mean$p.value, "\n")
    # Попарные Wilcoxon для средних (непараметрический аналог)
    cat("Попарные Wilcoxon тесты (для сравнения средних, данные ненормальные):\n")
    for(i in 1:(length(classes)-1)){
      for(j in (i+1):length(classes)){
        grp1 <- iris_data[iris_data$class == classes[i], f]
        grp2 <- iris_data[iris_data$class == classes[j], f]
        wilcox_test <- wilcox.test(grp1, grp2)
        cat(classes[i], "vs", classes[j], "- p-value =", wilcox_test$p.value, "\n")
      }
    }
  }
  
  # 3. Сравнение дисперсий
  cat("\nСравнение дисперсий:\n")
  levene_test <- leveneTest(as.formula(paste(f, "~ class")), data = iris_data)
  cat("Levene's test (все классы) p-value =", levene_test$`Pr(>F)`[1], "\n")
  
  cat("Попарные F-тесты:\n")
  for(i in 1:(length(classes)-1)){
    for(j in (i+1):length(classes)){
      grp1 <- iris_data[iris_data$class == classes[i], f]
      grp2 <- iris_data[iris_data$class == classes[j], f]
      var_test <- var.test(grp1, grp2)
      cat(classes[i], "vs", classes[j], "- p-value =", var_test$p.value, "\n")
    }
  }
}




# Вывод (по результату программы):
# Распределения нормальные кроме petal_width
# 1) sepal_length:
#    Распределения: не равны в общем, не равны попарно
#    Средние: не равны в общем, не равны попарно
#    Дисперсии: не равны в общем, не равны попарно кроме Iris-versicolor vs Iris-virginica
# 2) sepal_width:
#    Распределения: не равны в общем, не равны попарно
#    Средние: не равны в общем, не равны попарно
#    Дисперсии: равны в общем, равны попарно  
# 3) petal_length
#    Распределения: не равны в общем, не равны попарно
#    Средние: не равны в общем, не равны попарно
#    Дисперсии: не равны в общем, не равны попарно кроме Iris-versicolor vs Iris-virginica
# 4) petal_width
#    Распределения: не равны в общем, не равны попарно
#    Средние: не равны в общем, не равны попарно
#    Дисперсии: не равны в общем, не равны попарно


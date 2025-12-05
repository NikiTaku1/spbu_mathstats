# Загрузка данных
library(dplyr)
library(car)

df <- read.csv("C:/Users/Zver/Desktop/uni/матстат/R projects/hw3/dataset.csv")

# Преобразуем Success в логический тип
df$Success <- as.logical(df$Success)

groups <- unique(df$TreatmentGroup)
features <- c("WeightBefore", "BPBefore", "SugarBefore", "WeightAfter", "BPAfter", "SugarAfter")

# Функция проверки нормальности
is_normal <- function(x) {
  if(length(x) < 3) return(FALSE)
  shapiro.test(x)$p.value > 0.05
}

# --------------------------
# 1. Сравнение средних и дисперсий между группами
# --------------------------
for(f in features){
  cat("\n=== Feature:", f, "===\n")
  
  # Проверка нормальности по группам
  normal_all <- all(sapply(groups, function(g) is_normal(df[df$TreatmentGroup==g, f])))
  
  cat("Нормальность всех групп:", normal_all, "\n")
  
  if(length(groups) == 2){
    g1 <- df[df$TreatmentGroup==groups[1], f]
    g2 <- df[df$TreatmentGroup==groups[2], f]
    
    # Средние
    if(normal_all){
      t_res <- t.test(g1, g2, var.equal = TRUE)
      cat("t-test p-value =", t_res$p.value, "\n")
    } else {
      wilcox_res <- wilcox.test(g1, g2)
      cat("Wilcoxon p-value =", wilcox_res$p.value, "\n")
    }
    
    # Дисперсии
    f_res <- var.test(g1, g2)
    cat("F-test p-value =", f_res$p.value, "\n")
    
  } else {
    # Более двух групп
    formula <- as.formula(paste(f, "~ TreatmentGroup"))
    
    # Средние
    if(normal_all){
      aov_res <- aov(formula, data=df)
      cat("ANOVA p-value =", summary(aov_res)[[1]][["Pr(>F)"]][1], "\n")
    } else {
      kruskal_res <- kruskal.test(formula, data=df)
      cat("Kruskal-Wallis p-value =", kruskal_res$p.value, "\n")
    }
    
    # Дисперсии
    levene_res <- leveneTest(formula, data=df)
    cat("Levene's test p-value =", levene_res$`Pr(>F)`[1], "\n")
  }
}

# --------------------------
# 2. Сравнение распределений
# --------------------------
for(f in features){
  formula <- as.formula(paste(f, "~ TreatmentGroup"))
  kruskal_res <- kruskal.test(formula, data=df)
  cat("\nFeature:", f, "- распределения между группами (Kruskal-Wallis) p-value =", kruskal_res$p.value, "\n")
  
  # Попарные сравнения
  combs <- combn(groups, 2)
  for(i in 1:ncol(combs)){
    g1 <- df[df$TreatmentGroup==combs[1,i], f]
    g2 <- df[df$TreatmentGroup==combs[2,i], f]
    wilcox_res <- wilcox.test(g1, g2)
    cat("Попарное сравнение:", combs[1,i], "vs", combs[2,i], "- p-value =", wilcox_res$p.value, "\n")
  }
}

# --------------------------
# 3. Проверка успеха события
# --------------------------
success <- df$Success
n_success <- sum(success, na.rm = TRUE)
n_total <- sum(!is.na(success))

cat("\nКоличество успехов:", n_success, "\n")
cat("Количество наблюдений:", n_total, "\n")

# Проверка гипотезы p0 = 0.7
p0 <- 0.7
binom_res_0.7 <- binom.test(n_success, n_total, p = p0, alternative = "greater")
cat("\nТочный биномиальный тест для порога 0.7:\n")
print(binom_res_0.7)

# Проверка гипотезы p0 = 0.9
p0 <- 0.9
binom_res_0.8 <- binom.test(n_success, n_total, p = p0, alternative = "greater")
cat("\nТочный биномиальный тест для порога 0.8:\n")
print(binom_res_0.8)

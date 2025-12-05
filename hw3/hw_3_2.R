eur <- read.table("C:/Users/Zver/Desktop/uni/матстат/R projects/hw3/euroweight.txt", header = FALSE)
# Переименуем для удобства
names(eur) <- c("id", "weight", "batch")
# Удаляем ID
eur$id <- NULL

# Общая проверка нормальности для всей выборки
shapiro_all <- shapiro.test(eur$weight)
shapiro_all

# Проверка нормальности по каждому пакету
batches <- unique(eur$batch)

for (b in batches) {
  cat("Пакет", b, ":\n")
  print(shapiro.test(eur$weight[eur$batch == b]))
  cat("\n")
}
# Есть не нормальные распределения, используем Крускала и Вилкоксона

# Критерий Крускала–Уоллиса
kruskal.test(weight ~ as.factor(batch), data = eur)

# Попарный тест Вилкоксона
pairwise.wilcox.test(eur$weight, eur$batch, p.adjust.method = "bonferroni")

# Вывод:
# Среди весов всех пакетов как минимум один отличается (p < 0.05)

#    1 2 3 4 5 6 7 (y - различие есть, n - различия нет)     
#  2 n - - - - - -      
#  3 n y - - - - -      
#  4 y n y - - - -      
#  5 y n y n - - -      
#  6 n n n y y - -      
#  7 n n y n n n -      
#  8 n n n y y n n

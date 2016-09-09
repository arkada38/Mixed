# Метод к-го ближайшего соседа (knn)
# Простейшая валидация

sales <- read.table("Datasets/DISCRIM.txt", header = T, sep = ";")

head(sales)
tail(sales)

set.seed(1234)

# Выбираем случайным образом треть данных (без повтора)
test.num <- sample(1:nrow(sales), nrow(sales) / 3, replace = F)

# Тестовая выборка
test <- sales[test.num, 2:5]

# Обучающая выборка
train <- sales[-test.num, 2:5]

# Код класса для обучающей выборки
cl <- sales[-test.num, 6]

# Распознаем класс объектов из тестовой выборки
library(class)
zzz <- knn(train, test, cl, k = 3)

# Проверяем соответствие истинного и распознанного классов
table(zzz, sales[test.num, 6])



# Как определять значение k?
# Кросс-валидация - простейший вариант
a <- vector()
for (i in 1:15)
{
  zzz <- knn(train, test, cl, k = i)
  a[i] <- sum(zzz != sales[test.num, 6])
}
a
# [1] 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1
# k = 5 - Оптимальнее, так как всего одна ошибка

# Спрогнозируем уровень продаж на следующие данные - 66, 25, 40, 17
knn(train, c(66, 25, 40, 17), cl, k = 5)
# [1] 2
# Levels: 1 2 3

# Лучше данные стандартизировать
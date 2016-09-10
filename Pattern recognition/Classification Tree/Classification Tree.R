# Classification Tree
# Высоко интерпретируемый алгоритм

credit.01 <- read.csv2("Datasets/Credit.csv")

head(credit.01)
dim(credit.01)
# [1] 323   5

names(credit.01)
# [1] "credit"  "class"   "income"  "age"     "cr_cart"

library(rpart)
# maxdepth - количество слоев у дерева
# minbucket - минимальный размер потомка
# minsplit - МИНИМАЛЬНОЕ КОЛИЧЕСТВО НАБЛЮДЕНИЙ В УЗЛЕ РОДИТЕЛЯ
credit.01.res <- rpart(credit ~ class + income + age + cr_cart,
                             data = credit.01, method = "class",
                       control = rpart.control(minsplit = 10, minbucket = 5, maxdepth = 6))
credit.01.res

# Графика: дерево классификации
# 1 вариант: некрасивое дерево
plot(credit.01.res)
text(credit.01.res, use.n = T)

# 2 вариант: красивое дерево: много вариантов
# Необходима библиотека  rpart.plot
library(rpart.plot)
rpart.plot(credit.01.res)

# Красиво, но неинформативно
rpart.plot(credit.01.res, type = 0)
rpart.plot(credit.01.res, type = 1)
rpart.plot(credit.01.res, type = 2)
rpart.plot(credit.01.res, type = 3)
rpart.plot(credit.01.res, type = 4)

# Красиво, и информативно
rpart.plot(credit.01.res, type = 2, extra = 1)
extra.val <- 109
rpart.plot(credit.01.res, type = 2, extra = extra.val)

# credit.01.res и print(credit.01.res) выдают один и тот же результат.
# Но во втором случае имеются дополнительные возможности управления выводом.
# Смотри описание опций. Например, в команде print можно сократить число знаков после запятой.
print(credit.01.res, digits = 2)
 
summary(credit.01.res)
predict(credit.01.res, credit.01[ , -1])[ , 2]
table(credit.01[ , 1], predict(credit.01.res, credit.01[ , -1],  type="class"))
#     0   1
# 0 167   1
# 1  34 121



# Все переменные измерены в номинальной шкале
credit.02 <- data.frame(as.factor(credit.01[ , 1]), as.factor(credit.01[ ,2]),
                        as.factor(credit.01[ , 3]), as.factor(credit.01[ ,4]),
                        as.factor(credit.01[ , 5]))
names(credit.02) <- names(credit.01)

# Зададим уровни факторов
levels(credit.02[,1]) <- c( "Низкий", "Высокий")
levels(credit.02[,2]) <- c( "Management", "Professional", "Clerical", "Skilled Manual", "Unskilled")
levels(credit.02[,3]) <- c( "Еженедельно", "Ежемесячно")
levels(credit.02[,4]) <- c( "Молод (< 25)", "Средний(25-35)", "Пожилой( > 35)")
levels(credit.02[,5]) <- c( "Нет", "Да")

library(rpart)
credit.02.res <- rpart(credit ~ class + income + age + cr_cart,
                             data = credit.02, method = "class",
                             control = rpart.control(minsplit = 10, minbucket = 5, maxdepth = 6))

library(rpart.plot)
rpart.plot(credit.02.res)

# Линейная регрессия
# http://www.algorithmist.ru/2011/04/linear-regression-with-examples-in-r.html#more

# Читаем данные из файла
data <- read.table('diamond.dat')
names(data) <- c('Weight', 'Price')

summary(data)

# Строим линейную регрессию
fit <- lm(data$Price ~ data$Weight )
# Печатаем результат
summary(fit)

# Price = -259.63 + 3721.02 * Weight
# Модель описывает данные на 97.83%


plot(data$Price ~ data$Weight,
     main = "Price ~ Weight",
     sub = "Price = -259.63 + 3721.02 * Weight",
     xlab = "Вес бриллианта в каратах  (1 карат = 0.2 г.)", 
     ylab = "Цена кольца в сингапурских долларах")
abline(fit, col = 2)





library(party)

data <- readingSkills
data$result <- as.numeric(readingSkills$nativeSpeaker) - 1

# Print some records from data
print(head(data))

# Строим линейную регрессию
fit <- lm(data$result ~ data$age + data$shoeSize + data$score )

summary(fit)

# Result = -0.816931 -0.644451 * age -0.006149 * shoeSize + 0.162203 * score
# Модель описывает данные на 97.07%
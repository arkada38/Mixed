# Collatz conjecture
# Берём любое натуральное число n
# Если оно чётное, то делим его на 2, а если нечётное, то умножаем на 3 и прибавляем 1 (получаем 3n + 1)
# Над полученным числом выполняем те же самые действия, и так далее
# Гипотеза Коллатца заключается в том, что какое бы начальное число n мы ни взяли, рано или поздно мы получим единицу

# Факты:
# - Условие выполнится при достижении натурального числа n любого 2**x, n стремится к 2**x
# - Делить на 2 приходится чаще, чем умножать на 3 и прибавлять единицу
# - n стремится к 2**x, x кратен двум
# - n стремится к 4*2**x

# Функция для расчета ряда Коллатца
Collatz.conjecture <- function(N) {
  Collatz <- list()
  Collatz$steps <- 0
  Collatz$row <- N
  
  Collatz$two.in.degrees.n.up <- 2**ceiling(log2(N))
  Collatz$two.in.degrees.n.down <- 2**floor(log2(N))
  
  Collatz$divided.by.2 <- 0
  Collatz$multipled.by.3.plus.1 <- 0
  Collatz$first.two.in.degrees.n <- 1

  while (N != 1 | Collatz$steps > 1000) {
    # N стремится к 2**n
    if (Collatz$first.two.in.degrees.n == 1 & 2**ceiling(log2(N)) == N)
      Collatz$first.two.in.degrees.n <- log2(N)
    
    if (N %% 2 == 0) {
      N <- N / 2
      Collatz$divided.by.2 <- Collatz$divided.by.2 + 1
    }
    else {
      N <- 3 * N + 1
      Collatz$multipled.by.3.plus.1 <- Collatz$multipled.by.3.plus.1 + 1
    }
    
    Collatz$steps <- Collatz$steps + 1
    Collatz$row <- c(Collatz$row, N)
    
    Collatz$two.in.degrees.n.up <- c(Collatz$two.in.degrees.n.up, 2**ceiling(log2(N)))
    Collatz$two.in.degrees.n.down <- c(Collatz$two.in.degrees.n.down, 2**floor(log2(N)))
  }
  
  if (Collatz$steps > 1000)
    Collatz$result <- "Task has stoped. Too many steps (>1000)"
  else
    Collatz$result <- paste("Task completed by ", Collatz$steps, " steps")
  
  return (Collatz)
}

Collatz <- Collatz.conjecture(235)
Collatz <- data.frame(Collatz$row, Collatz$two.in.degrees.n,
                      Collatz$two.in.degrees.n.up, Collatz$two.in.degrees.n.down)
names(Collatz) <- c("row", "two.in.degrees.n", "two.in.degrees.n.up", "two.in.degrees.n.down")

library(ggplot2)
ggplot(Collatz, aes(seq(1, length(row)))) +
  geom_line(aes(y = row), colour = "black") +
  geom_line(aes(y = two.in.degrees.n), colour = "blue") +
  geom_line(aes(y = two.in.degrees.n.up), colour = "red") +
  geom_line(aes(y = two.in.degrees.n.down), colour = "green")

plot(Collatz$row, type = "l")
lines(Collatz$two.in.degrees.n.up, col = "red")
lines(Collatz$two.in.degrees.n.down, col = "green")




# Таблица для N от 1 до 100 000
# Условие выполнится при достижении N любого 2**n
# Поэтому особое внимание будем уделять столбцу со значениями n
df <- data.frame()
for (N in c(2:100000)) {
  Collatz <- Collatz.conjecture(N)
  df <- rbind(df, c(N, Collatz$steps, Collatz$divided.by.2, Collatz$multipled.by.3.plus.1, Collatz$first.two.in.degrees.n))
  rm(Collatz, N)
}
names(df) <- c("N","steps","divided.by.2","multipled.by.3.plus.1", "first.two.in.degrees.n")

# Посчитаем сколько наблюдений приходится на каждое n
y <- vector()
for (i in c(0:max(df$first.two.in.degrees.n))) {
  y <- c(y, sum(df$first.two.in.degrees.n == i))
}

# Наблюдения с n
# Вычленим только те n, которые встечаются более одного раза (ключевые),
# так как каждое 2**n является целым числом
z <- data.frame(c(0:(length(y) - 1)), y)
z <- subset(z, y > 1)
names(z) <- c("n", "q")
# Интересно то, что ключевые n кратны двум и выражаются следующей формулой
# n1 = 2+n0





plot(df$steps)


n <- 3
for (i in c(1:2)) {
  n <- 3 * n + 1
}
for (j in c(1:5)) {
  n <- n / 2
}
n
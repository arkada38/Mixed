# Collatz conjecture
# Берём любое натуральное число n
# Если оно чётное, то делим его на 2, а если нечётное, то умножаем на 3 и прибавляем 1 (получаем 3n + 1)
# Над полученным числом выполняем те же самые действия, и так далее
# Гипотеза Коллатца заключается в том, что какое бы начальное число n мы ни взяли, рано или поздно мы получим единицу

# Факты:
# - Условие выполнится при достижении натурального числа n любого 2**x, n стремится к 2**x
# - Делить на 2 приходится чаще, чем умножать на 3 и прибавлять единицу
# - n стремится к 2**x, x кратен двум или
# - n стремится к 2**(2*x)



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


# Таблица для N от 1 до 10 000
# Условие выполнится при достижении N любого 2**n
# Поэтому особое внимание будем уделять столбцу со значениями n
df <- data.frame()
for (N in c(2:10000)) {
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




Collatz <- Collatz.conjecture(3)
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





# Гипотеза
# Любое N будет равно 2**(2*n) при N = 3*N+1
N <- 18
while (log2(N) %% 2 != 0) {
  # В таком виде гипотезу проверить сложно, так как числа становятся слишком большими (компьютер не справляется)
  while (N %% 2 == 0) {
    N <- N / 2
  }
  N <- 3 * N + 1
  print(N)
}


a <- vector()
for (i in c(1:10)) {
  N <- i
  while (log2(N) %% 2 != 0) {
    while (N %% 2 == 0)
      N <- N / 2
    N <- 3 * N + 1
    print(N)
  }
  a <- append(a, N)
}

b <- vector()
for (i in c(0:max(a))) {
  b <- append(b, sum(a == i))
}
b <- b[b > 0]



# А что если делить на 2 только когда N = 2**x
# Так как N стремится к 1 => N стремится к 2**0
# Если делить на 2 только когда N = 2**x, то x стремится к 0, что нам и нужно
N <- 3
while (log2(N) %% 2 != 0) {
  while (log2(N) == ceiling(log2(N)))
    N <- N / 2
  N <- 3 * N + 1
  print(N)
}
# Не выйдет



# Возвращаюсь к ключевым n (N -> 2**(2*n))
n <- vector()
for (i in c(0:8)) {
  n <- append(n, 2 ** (2 * i))
}
n
# В двоичной системе счисления они выглядят как
# 1 100 10000 1000000 ...
# К предыдущему прибавляется два нуля

# Деление на два в двоичной СС - отброс последнего нуля
# Умножение на три и прибавление единицы в третичной системе счисления - запись единицы справа от числа


change.number.system <- function(n, from, to) {
  a <- 0
  
  if (from != 10 & to != 10) {
    n <- change.number.system(n, from, 10)
    from = 10
  }
    
  
  while (n != 0) {
    x <- floor(log(n, base = to))
    a <- a + from**(x)
    n = n - to**x
  }
  
  return(a)
}
10011 == change.number.system(19, 10, 2)
19 == change.number.system(10011, 2, 10)
10011 == change.number.system(201, 3, 2)

# Базовый алгоритм
N <- 19
while (N != 1) {
  N <- 3 * N + 1
  print(c(3, " ", N))
  while (N %% 2 == 0) {
    N <- N / 2
    print(c(2, " ", N))
  }
}

# Перепишем его со сменой систем счисления (2 и 3)
N <- 19
while (N != 1) {
  N <- change.number.system(N, 10, 3)
  N <- N * 10 + 1
  print(c(3, " ", N))
  
  N <- change.number.system(N, 3, 2)
  while (N %% 10 == 0) {
    N <- N / 10
    print(c(2, " ", N))
  }
  
  N <- change.number.system(N, 2, 10)
}

# Перепишем его со сменой систем счисления в шестиричную
N <- 19
N <- change.number.system(N, 10, 6)
while (N != 1) {
  N <- N * 3 + 1
  print(c(3, " ", N))
  
  while (N %% 2 == 0) {
    N <- N / 2
    print(c(2, " ", N))
  }
}

for (i in seq(from = 1, to = 99, by = 1)) {
  print(c(i," = ",change.number.system(i,10,6)))
}
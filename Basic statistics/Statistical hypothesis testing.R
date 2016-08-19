# Проверка статистических гипотез

# Данные о домах
d <- read.table("Datasets/Albuquerque Home Prices.txt", header = T)

names(d)
# [1] "PRICE" "SQFT"  "AGE"   "FEATS" "NE"    "CUST"  "COR"   "TAX"

summary(d)

d$AGE[d$AGE == -9999] <- NA
d$TAX[d$TAX == -9999] <- NA

# Дешевли ли дома на углу?
# H0 <- EX = EY
# H1 <- EX < EY

fligner.test(PRICE ~ COR, data = d)
# Fligner-Killeen test of homogeneity of variances
# 
# data:  PRICE by COR
# Fligner-Killeen:med chi-squared = 0.81756, df = 1, p-value = 0.3659

# Гипотеза о равенстве дисперсий не отвергается

t.test(d$PRICE[d$COR == 1], d$PRICE[d$COR == 0], alternative = "less", paired = F, var.equal = T)
# Two Sample t-test
# 
# data:  d$PRICE[d$COR == 1] and d$PRICE[d$COR == 0]
# t = -0.853, df = 115, p-value = 0.1977
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 72.56395
# sample estimates:
#   mean of x mean of y 
#    1000.318  1077.189 

# В среднем дома на углу подешевле на 76.871, но это различие статистически не значимо
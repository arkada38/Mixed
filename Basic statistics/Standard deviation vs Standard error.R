# Standard deviation vs Standard error
# http://datascienceplus.com/standard-deviation-vs-standard-error/



# Standard deviation

# Standard deviation is a measure of dispersion of the data from the mean.

set.seed(1)
# generate some random data
x <- rnorm(10)
# compute the standard deviation
sd(x)
# [1] 0.780586



# For normally distributed data the standard deviation has some extra information,
# namely the 68-95-99.7 rule which tells us the percentage of data lying
# within 1, 2 or 3 standard deviation from the mean.

plot(seq(-3.2, 3.2, length = 50), dnorm(seq(-3, 3, length = 50), 0, 1),
     type = "l", xlab = "", ylab = "", ylim = c(0, 0.5))

segments(x0 = c(-3, 3), y0 = c(-1, -1), x1 = c(-3, 3), y1 = c(1, 1))
text(x = 0, y = 0.45, labels = expression("99.7% of the data within 3" ~ sigma))
arrows(x0 = c(-2, 2), y0 = c(0.45, 0.45), x1 = c(-3, 3), y1 = c(0.45, 0.45))

segments(x0 = c(-2, 2), y0 = c(-1, -1), x1 = c(-2, 2), y1 = c(0.4, 0.4))
text(x = 0, y = 0.3, labels = expression("95% of the data within 2" ~ sigma))
arrows(x0 = c(-1.5, 1.5), y0 = c(0.3, 0.3), x1 = c(-2, 2), y1 = c(0.3, 0.3))

segments(x0 = c(-1, 1), y0 = c(-1, -1), x1 = c(-1, 1), y1 = c(0.25, 0.25))
text(x = 0, y = 0.15, labels = expression("68% of the data within 1" * sigma), cex = 0.9)

# Of course if the data are not normally distributed such interpretation is not valid.
# It remains that standard deviation can still be used as a measure of dispersion
# even for non-normally distributed data.



# Standard error of the mean

# It is a measure of how precise is our estimate of the mean.
# Computation of the standard error of the mean.

sem <- sd(x) / sqrt(length(x))
# 95% confidence intervals of the mean
c(mean(x) - 2 * sem, mean(x) + 2 * sem)
# [1] -0.3614831  0.6258887
# Standardization

# Standardization from 0 to 1
# scale(data, center = mins, scale = maxs - mins)
standardize_vector_from_0_to_1 <- function(num) {
  min <- min(num)
  max <- max(num)
  num <- (num - min) / (max - min)
  return(num)
}

# Standardization from -1 to 1
standardize_vector_from_minus_1_to_1 <- function(num) {
  min <- min(num)
  max <- max(num)
  num <- (num - min) / (max - min) * 2 - 1
  return(num)
}

# Standardization with mean = 0 and sd = 1
# scale(data, center = T, scale = T)
standardize_vector_z <- function(num) {
  r <- (num - mean(num)) / sd(num)
  return(r)
}
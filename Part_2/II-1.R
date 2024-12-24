# Question 1

## setup
rm(list = ls())
library(ggplot2)
library(tibble)

## (1)

set.seed(1234)
N <- 1000
x <- rnorm(N)
u1 <- rnorm(N)
u2 <- sapply(x, function(xi){rnorm(1,sd = abs(xi))})
a <- 1
b <- 2

### Model 1 & 2:
y1 <- a + b * x + u1
df_Model_1 <- tibble(y = y1, x)
y2 <- a + b * x + u2
df_Model_2 <- tibble(y = y2, x)

### draw plots respectively
scat_Model_1 <- ggplot(df_Model_1, aes(x, y)) +
  geom_point(color = "skyblue") +
  labs(x = 'x', y = 'y1')
scat_Model_2 <- ggplot(df_Model_2, aes(x, y)) +
  geom_point(color = "skyblue") +
  labs(x = 'x', y = 'y2')
ggsave(scat_Model_1, filename = 'output/Q1_1_scat_Model_1.png')
ggsave(scat_Model_2, filename = 'output/Q1_1_scat_Model_2.png')

## (2)

### funciton construction

reg <- function(x, y, n){
  df = tibble(x,y)
  x_bar <- mean(x)
  y_bar <- mean(y)
  SST <- sum((df$x - x_bar)^2)
  
  b_hat <- sum((df$y - y_bar) * (df$x - x_bar)) / SST
  a_hat <- y_bar - b_hat * x_bar
  sigma_hat_square = sum((df$y - a_hat - b_hat * df$x)^2) / (n -2)
  se_b_hat_0 <- sqrt(sigma_hat_square / SST)
  se_b_hat_1 <- sqrt(sum((df$x - x_bar)^2 * (df$y - a_hat - b_hat * df$x)^2) / SST^2)
  result <- c(a_hat, b_hat, sigma_hat_square, se_b_hat_0, se_b_hat_1)
  names(result) = c('a_hat', 'b_hat',  'sigma_hat^2', 'se_0', 'se_1')
  return(result)
}

### reg and compare
reg_Model_1 <- reg(x, y1, N)
print(reg_Model_1)
lm_Model_1 <- lm(y1 ~ x)

reg_Model_2 <- reg(x, y2, N)
print(reg_Model_2)
lm_Model_2 <- lm(y2 ~ x)

t_value_1 <- reg_Model_1["b_hat"] / reg_Model_1["se_0"]
print(paste("Without White standard error, the t value of b_hat in model 1 is : ", t_value_1))

t_value_2 <- reg_Model_2["b_hat"] / reg_Model_2["se_1"]
t_value_2_no <- reg_Model_2["b_hat"] / reg_Model_2["se_0"]
print(paste("With White standard error, the t value of b_hat in model 2 is : ", t_value_2))
print(paste("Without White standard error, the t value of b_hat in model 2 is : ", t_value_2_no))

### Answers in natural language

#### Plz check my Rmd file for this part.

## (3)

simulate_t_homo <- function(choice){
  N <- 1000
  x <- rnorm(N)
  u1 <- rnorm(N)
  y1 <- u1 + 1
  reg_rslt <- reg(x, y1, N)
  if (choice == 0) {
    t_value <- reg_rslt["b_hat"] / reg_rslt["se_0"]
  } else if (choice == 1) {
    t_value <- reg_rslt["b_hat"] / reg_rslt["se_1"]
  } else {
    t_value <- NA
  }
  return(sum(t_value))
}

t0 <- replicate(10000, simulate_t_homo(0))
t0_bool <- abs(t0) > 1.96
t0_reject_proportion <- sum(t0_bool) / 10000
t1 <- replicate(10000, simulate_t_homo(1))
t1_bool <- abs(t1) > 1.96
t1_reject_proportion <- sum(t1_bool) / 10000

print(paste('The proportion of rejecting the null hypothesis using t0 is ', t0_reject_proportion))
print(paste('The proportion of rejecting the null hypothesis using t1 is ', t1_reject_proportion))

## (4)

simulate_t_hetero <- function(choice){
  N <- 1000
  x <- rnorm(N)
  u2 <- sapply(x, function(xi){rnorm(1,sd = abs(xi))})
  y2 <- u2 + 1
  reg_rslt <- reg(x, y2, N)
  if (choice == 0) {
    t_value <- reg_rslt["b_hat"] / reg_rslt["se_0"]
  } else if (choice == 1) {
    t_value <- reg_rslt["b_hat"] / reg_rslt["se_1"]
  } else {
    t_value <- NA
  }
  return(sum(t_value))
}

t0 <- replicate(10000, simulate_t_hetero(0))
t0_bool <- abs(t0) > 1.96
t0_reject_proportion <- sum(t0_bool) / 10000
t1 <- replicate(10000, simulate_t_hetero(1))
t1_bool <- abs(t1) > 1.96
t1_reject_proportion <- sum(t1_bool) / 10000

print(paste('The proportion of rejecting the null hypothesis using t0 is ', t0_reject_proportion))
print(paste('The proportion of rejecting the null hypothesis using t1 is ', t1_reject_proportion))

## (5)

# check my rmd, pdf or html file for non-code answers.
# I-11

google <- as.Date("09,27,1998",
                  format = "%m, %d, %Y")
# I-12 yxc
ford = data.frame(
  date = as.Date(c("8/8/2016", "8/9/2016", "8/10/2016"), format = "%m/%d/%Y"),
  close = c(11.39, 11.51, 11.46),
  volume = c(35563393, 32122187, 26218354)
)

# ford$high <- ford$close \> 11.50 
#It's been proven that this line is not right.
print(ford)

# I-12
ford = data.frame(
  date = as.Date(c("8/8/2016", "8/9/2016", "8/10/2016"), format = "%m/%d/%Y"),
  close = c(11.39, 11.51, 11.46),
  volume = c(35563393, 32122187, 26218354)
)

ford$high <- ford$close >= 11.50
print(ford)

# I-13 yxc
jellybean <- 17
repeat {
  jellybean <- jellybean + 17
  print(jellybean)
  if (jellybean > 60) {
    print("Cup overflowed")
    break
  }
}

# I-13
jellybean <- 17
while (TRUE) {
  jellybean <- jellybean + 17
  print(jellybean)
  if (jellybean > 60) {
    print("Cup overflowed")
    break
  }
}

# I-14
set.seed(1234)

N <- 200
x1 <- runif(N, 1, 40)
x2 <- runif(N, 1, 40)
eps <- rnorm(N, 0, 2)

y <- 40 + .5 * x1 + .4* x2 + eps
df<- data.frame(y,x1,x2)
df_train <- df[1:(N/2),]
df_test <- df[(N/2+1):N,]

m1 <- lm(y ~ x1 + x2, data = df_train)

print(summary(m1))

# I-15

beta <- m1$coefficients
df_test$y_pre <- beta[1] + beta[2]*df_test$x1 + beta[3]*df_test$x2
MSE <- mean((df_test$y_pre-df_test$y)^2) # out of sample MSE of fitted model
MSE0 <- mean((df_test$y-mean(df_train$y))^2) # out of sample MSE of using training set mean
print(MSE)
print(MSE0)



# Question 4

# set up 
rm(list = ls())
library(glmnet)

# question stem's part
set.seed(1234)
p = 100
n = 1000
X = matrix(0,n,p)
eps = rnorm(n)
for (j in c(1:p)) {
  X[,j] = rnorm(n)
}
beta = c(c(1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1),rep(0,80))
Y = X%*%beta + eps
Y_train = Y[1:800]
X_train = X[1:800,]
Y_test = Y[801:1000]
X_test = X[801:1000,]

# (1)

lm_model <- lm(Y_train~X_train + 0)
summ_lm <- summary(lm_model)
sgns <- abs(coef(summ_lm)[, "t value"]) > 1.96
num_D <- sum(sgns)
num_FD <- sum(sgns[21:100])
FDP <- num_FD/num_D
cat(paste("\n\nI found", num_D, 
          "signals, of which the FDP is", FDP, '.\n'))

# (2)

sgns_BC <- abs(coef(summ_lm)[, "t value"]) > qt(1 - 0.005 / 2, 699)
num_BC_D <- sum(sgns_BC)
num_BC_FD <- sum(sgns_BC[21:100])
FDP_BC <- num_BC_FD/num_BC_D
cat(paste("\nWith Bonferroni’s rejection rule, I found", num_BC_D, 
          "signals, of which the FDP is", FDP_BC, 
          ". It did significantly improve the FDP.\n"))


# (3)

lambdas_to_try <- 10 ^ seq(-3, 5, length.out = 100)
set.seed(1234)
lasso_cv <- cv.glmnet(X_train, Y_train,
                      intercept = FALSE,
                      alpha = 1,
                      lambda = lambdas_to_try,
                      standardize = TRUE,
                      nfolds = 10
)

png("output/Q4_3_lasso_cv_plot.png", width = 600, height = 600)
plot(lasso_cv)
dev.off()

lasso_min_coef <- coef(lasso_cv, s = "lambda.min")
lasso_1se_coef <- coef(lasso_cv, s = "lambda.1se")
omv_min <- lasso_min_coef[2:101] == 0
omv_1se <- lasso_1se_coef[2:101] == 0
omv_min_num <- sum(omv_min)
omv_1se_num <- sum(omv_1se)

cat(paste(
  "\nWith lasso: \n",
  omv_min_num,
  "variables are omitted,",
  100 - omv_min_num,
  "are validated, if we choose the lambda obtaining minimal MSE(lambda.min),\n",
  omv_1se_num,
  "variables are omitted,",
  100 - omv_1se_num,
  "are validated, if we choose the lambda within a standard error's distance from minimal MSE while omitting variables as much as it can(lambda.1se).\n"
))


# (4)


MSE_test_table <- data.frame(matrix(NA,5,2))
colnames(MSE_test_table) <- c("Model_name", "Test_MSE")
omv_all <- c(rep(FALSE, 100))
omv_t <- ! sgns
omv_B <- ! sgns_BC
models <- c("OLS_all", "OLS_t", "OLS_B", "Lasso_min", "Lasso_1se")
omv_vc <- matrix(c(omv_all, omv_t, omv_B, omv_min, omv_1se), ncol = 5)
for (i in 1:length(models)) {
  model <- models[i]
  omv <- omv_vc[,i]
  X_train_filtered <- X_train[, ! omv]
  X_test_filtered <- X_test[, ! omv]
  lm_model <- lm(Y_train~X_train_filtered + 0)
  pred <- X_test_filtered %*% as.vector(coef(lm_model))
  bias <- Y_test - pred
  MSE <- mean(bias^2)
  MSE_test_table[i, 1] <- model
  MSE_test_table[i, 2] <- MSE
}

MSE_test_table$Test_MSE <- round(MSE_test_table$Test_MSE, 7)
write.csv(MSE_test_table, file = "output/Q4_4_test_MSE.csv", row.names = F)

# As we can see from the report table, t-selection method has minimal MSE while ordinary no-selection model has maximal MSE. With Lasso selection, lambda.min method doesn't work as good as lambda.1se method, as it has more false positive variables included. Lasso with lambda.1se method does performs better than B-selection method, it's still not as good as t-selection method in our simulation.
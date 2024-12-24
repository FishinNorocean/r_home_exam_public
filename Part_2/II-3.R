# Question 3

# set up
rm(list = ls())
library(ggplot2)
library(glmnet)
library(dplyr)

# (1)

# data load and pre-process
df_source <- read.csv('data/vixlarge.csv', header = FALSE, sep = ',')
colnames(df_source) <- c("date", "VIX")
df_source$date <- as.Date(df_source$date, format = "\'%Y-%m-%d\'")

# plot
VIX_date_plot <- ggplot(df_source, aes(date, VIX)) + 
  geom_line(color = "lightblue") + 
  labs(title = "VIX and Date Variation", x = 'Date', y ='VIX')
ggsave(VIX_date_plot, filename = 'output/Q3_1_VIX_Date_plot.png', width = 8, height = 4)

# (2)

# transform the data to time series
ts_source <- ts(df_source$VIX)
num <- length(ts_source)
model <- ar(ts_source, order.max = 22, method = "ols")
IC_values <- data.frame(
  order = 1:22,
  AIC = model$aic[2:23],
  BIC = numeric(22)
)
IC_values$BIC <- IC_values$AIC + (log(num) - 2) * IC_values$order

# Please note that all AIC values calculated by `ar()` have been adjusted: the lowest AIC is set to 0, while all other's values are relative to minimal AIC value. As i calcualted the BIC values from AIC values, all theses values are just realtvie ones. This is why some BIC values are abnoramlly negative.

# 请注意，所有这里的AIC都是被 `ar()` 函数调整过的：最小的AIC值被设为0，其他的AIC值都是相对于最小值的大小，而BIC的值都是我根据AIC的值计算得到的。因此这里所有的值都是相对的值，这也是什么部分BIC出现了负数的原因。

write.csv(IC_values, "output/Q3_2_AR_IC.csv", row.names = FALSE)

cat(paste0("\nFrom the data we can find out that \nwith AIC, AR(", which.min(IC_values$AIC), ") is the best, \nwhile with BIC, AR(", which.min(IC_values$BIC), ") is the best.\n"))


# (3)

# a vacant table to record the model performance
error_table <- data.frame(matrix(ncol = 44, nrow = 0))
colnames(error_table) <- c(paste0("sqr_AR", 1:22), paste0("abs_AR", 1:22))

for (mx_lag in 1:22) {
  for (stt in 1:(length(ts_source) - 3000)) {
    wd <- ts_source[stt:(stt + 2999)]
    model <- ar(wd, order.max = mx_lag, method = "ols", aic = F)
    pred <- predict(model, n.ahead = 1)
    bias <- ts_source[stt + 3000] - as.numeric(pred$pred)
    error_table[stt, mx_lag] <- bias^2
    error_table[stt, mx_lag + 22] <- abs(bias)
  }
}

# calculate mean error
mean_error <- data.frame(
  Model = c(paste0("AR(", 1:22, ")")),
  Mean_Sqr_Error = round(apply(error_table[, 1:22], 2, mean), 7),
  Mean_Abs_Error = round(apply(error_table[, 23:44], 2, mean), 7),
  row.names = NULL
)
write.csv(mean_error, file = "output/Q3_3_autoreg_error.csv",
          row.names = FALSE)

# (4)

# generate overall `y` and `X` to be indexed
vec_ts <- as.vector(ts_source)
df_ts_with_lag <- data.frame(
  "VIX" = vec_ts
)
for (i in 1:22) {
  df_ts_with_lag[,paste0("lag", i)] = lag(vec_ts, i)
}
overall_y <- df_ts_with_lag %>%
  select(VIX) %>% 
  scale(center = TRUE, scale = FALSE) %>% 
  as.matrix()
overall_X <- df_ts_with_lag %>% 
  select(-VIX) %>% 
  as.matrix()

# again another error table
RL_error_table <- data.frame(matrix(ncol = 8, nrow = 0))
model_names <- c("Ridge_1", "Ridge_10", "Lasso_1", "Lasso_10")
colnames(RL_error_table) <- c(paste0("sqr_", model_names), 
                              paste0("abs_", model_names))

# start the loop
alps <- c(0, 0, 1, 1)
lmbs <- c(1, 10, 1, 10)
for (i in 1:4) {
  alp <- alps[i]
  lmb <- lmbs[i]
  for (stt in 1:(length(ts_source) - 3000)){
    y <- overall_y[(stt + 22):(stt + 2999),]
    X <- overall_X[(stt + 22):(stt + 2999),]
    model <- glmnet(X, y, alpha = alp, lambda = lmb)
    pred <- predict(model, newx = overall_X[stt + 3000,])
    bias <- overall_y[stt + 3000] - pred
    RL_error_table[stt, i] <- bias^2
    RL_error_table[stt, i + 4] <- abs(bias)
  }
}

# calculate mean error
RL_mean_error <- data.frame(
  Model = model_names,
  Mean_Sqr_Error = round(apply(RL_error_table[, 1:4], 2, mean), 7),
  Mean_Abs_Error = round(apply(RL_error_table[, 5:8], 2, mean), 7),
  row.names = NULL
)
overall_mean_error <- rbind(mean_error, RL_mean_error)
write.csv(RL_mean_error, file = "output/Q3_4_RL_autoreg_error.csv",
          row.names = FALSE)
write.csv(overall_mean_error, file = "output/Q3_4_overall_error.csv",
          row.names = FALSE)

# (5)
mean_MSE_index <- which.min(overall_mean_error$Mean_Sqr_Error)
mean_MAE_index <- which.min(overall_mean_error$Mean_Abs_Error)
cat(paste("\nThe model with minimal MSE is",
          overall_mean_error$Model[mean_MSE_index],
          "\nThe model with minimal MAE is",
          overall_mean_error$Model[mean_MAE_index]))


# As we can see from the output and the table of Mean Errors, the model with minimal MSE and MAE are AR(5) and AR(2), seperately. However, Ridge and Lasso regularization method didn't improve the performance as we had expected, instead, the MAE and MSE of these models are obviously higher than normal AR(22) model. As i am concerned, no lags should be punished for being non-zero in our model, all lags play a part in determining the future value of VIX and thus no need to punish a coefficient to be non-zero. Therefore, the non-necessary punishment failed the model to perform better, while instead, worse.


# 我们可以命令返回的结果以及记录平均误的表格可以的得到，按照 MSE，最好的模型是 AR(5)，按照MAE，最好的模型是AR(2)。然而，Ridge and Lasso 方法并没有如同我们所预料地，改善模型的表现，反而让模型的误差明显大于 AR(22)。我认为，这背后的原因在于，我们的模型中所有的lag都差不多得决定了未来的 VIX 值，于是没有必要惩罚非零参数。因此施加不必要的参数非零惩罚反而使得模型的表现更差了。

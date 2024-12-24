review <- data.frame(
  product1 = c(0.05, 0.03, 0.04, 0.02, 0.06),  # 产品1的收益率
  product2 = c(0.06, 0.02, 0.03, 0.05, 0.04)   # 产品2的收益率
)
# the defination of `review` above is from chatgpt
sharpe <- function
    (x, rf = .00003) {
     (mean(x) -
       rf) / sd(x)
 }
lapply(review, FUN=sharpe, .00007)

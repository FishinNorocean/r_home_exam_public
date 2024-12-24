broke <- data.frame(
  tank = c(11.25, 11.47, 11.52),
  horbl = c(36.91, 38.09, 38.32)
)
print(sapply(broke, FUN = mean))
print(sapply(broke, FUN = mean,
       simply = TRUE,
       USE.NAMES = TRUE))
debt <- 3000
i <- 0
num_months = c(0)
credit = c(3000)
while (debt > 0) {
  debt <- debt - 1000
  i <- i + 1
  num_months <- c(num_months, i)
  credit <- c(credit, debt)
}
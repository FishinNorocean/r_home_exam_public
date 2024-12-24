close <- c(42.6, 42.34, NA, NA, 45.14)
for (value in close) {
    if(is.na(value)) {
        print("Skip Empty Day")
next
}
  if(value > 45) {
        print(paste(value, " - Over!"))
break
}
}

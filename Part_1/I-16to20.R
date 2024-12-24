# I-16
library(tibble)
library(ggplot2)
movies <- tibble(
  title = c('Filly Brown', 'The Dish', 'Waiting for Guffman', 'The Age of Innocence', 'Malevolence', 'Old Partner'),
  title_type = c('Feature Film', 'Feature Film', 'Feature Film', 'Feature Film', 'Feature Film', 'Documentary'),
  run_time = c(80, 101, 84, 139, 90, 78)
)
ggplot(movies, aes(run_time, stat(density))) + geom_histogram(binwidth = 10) + geom_density()

# I-17
happiness <- read.csv(
  file = 'Part_I/2015.csv',
  header =TRUE
)
happiness = tibble(happiness)
colnames(happiness)[6] = 'GDP per Capita'
colnames(happiness)[4] = 'Score'
happiness$Generosity <- cut(happiness$Generosity, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
ggplot(happiness, aes(`GDP per Capita`, Score, size = Generosity)) + geom_point()


#I-18
ral <- runif(12)
sales <- data.frame(
  Total = c(ral * 2, ral * 1, ral * 20),
  Month = c(1:12, 1:12, 1:12),
  Sector = c(
    rep("Clothing", times = 12),
    rep("Food", times = 12), 
    rep("Non-specialized", times = 12)
  )
)

ggplot(sales, aes(Month,Total, color = Sector)) +geom_line()+
  labs( x="Month", y="Total Monthly Sales")


# I-19
top5<-data.frame(
  Popularity=c(70,80,80,80,80,85,85,85,85,85,85,85,85,90,90,90,90,90,90,90,90,90,90,90,95,95,95)
)
ggplot(top5, aes(Popularity)) +
  geom_histogram(binwidth = 5)+
  scale_x_continuous(breaks = seq(70,100,5))


# I-20
wine <- data.frame(
  citric_acid = c(0.25,0.50,0.75),
  pH=c(3.0,3.5,4.0)
)

ggplot(wine, aes(citric_acid, pH)) +
  geom_point(alpha=0.3)

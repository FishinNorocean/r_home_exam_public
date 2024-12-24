# Question 2

# setup
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

# (1)

# load and pre-process data
df_source <- read.csv('data/pokedex.csv')
df_source$type <- as.factor(df_source$type)
df_source$is_legendary <- as.factor(df_source$is_legendary)
df_source <- tibble(df_source)

# first 6 rows
first_6_df_source <- head(df_source)
write.csv(first_6_df_source, 
          file = 'output/Q2_1_first_6_rows.csv', row.names = FALSE)

# num and proportion calculation
legen_num <- sum(df_source$is_legendary == 1)
nlege_num <- sum(df_source$is_legendary == 0)
legen_prop <- round(mean(df_source$is_legendary == 1), 7)
nlege_prop <- round(mean(df_source$is_legendary == 0), 7)

# table to store the results
num_prop_table <- data.frame(matrix(
  c(legen_num, nlege_num, legen_prop, nlege_prop), nrow = 2))
colnames(num_prop_table) <- c("Number", "Proportion")
rownames(num_prop_table) <- c("Legendary", "Ordinary")

write.csv(num_prop_table, file = "output/Q2_1_Legen_num_prop.csv")


# (2)
# (2) (i)
height_weight_plot <- ggplot(df_source, aes(weight_kg, height_m)) +
  geom_point(aes(color = is_legendary)) +
  geom_text(aes(label = ifelse(weight_kg > 600 | height_m > 7.5, name, "")), 
            vjust = -0.5) +
  scale_color_manual(name = "Legendary or not", 
                     values=c("lightblue", "gold"), 
                     labels=c("Ordinary","Legendary")) +
  labs(title="Height against Weight among all pokemons", 
       x="Weight, in kilograms", 
       y="Height, in meters") +
  xlim(0, 1200)

ggsave(height_weight_plot, 
       filename = "output/Q2_2_1_Height_weight_plot.png", 
       height = 6, width = 9)

# (2) (ii)
  
type_legen <- group_by(df_source, type) %>%
  summarise(proportion = mean(is_legendary == 1))

type_legen_bar <- ggplot(type_legen, aes(type, proportion)) +
  geom_bar(stat="identity", fill = 'lightblue') +
  theme(aspect.ratio = 2/3, axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "Type", y = "proportion", title = "Type and Lengendary Proportion")

ggsave(type_legen_bar, filename = "output/Q2_2_2_type_legen_barplot.png")
type_legen$proportion <- round(type_legen$proportion, 7)
write.csv(type_legen, 
          file = "output/Q2_2_2_type_legen_proportion.csv",
          row.names = FALSE
)

# (2) (iii)
get_box_plot <- function(index){
  plt <- ggplot(df_source, 
              aes(is_legendary, .data[[index]], fill = is_legendary)) +
  geom_boxplot() +
  labs(x = 'Legendary or not', y = index,
       title = paste(toTitleCase(index), 
                     "Comparison Between Legendary and Ordinary")) +
  scale_x_discrete(labels = c("0" = "Ordinary", "1" = "Legendary")) +
  scale_fill_manual(values=c("0"="lightblue","1"="gold")) +
  guides(fill = "none")
  return(plt)
}

objs <- c("attack","sp_attack","defense","sp_defense","hp","speed")

for (index in objs) {
  plt <- get_box_plot(index)
  ggsave(plt, filename = paste0('output/Q2_2_3/Q2_2_3_', 
                               toTitleCase(index), '_Boxplot.png'))
}

# (3)

# (3) (i)

# Set seed for reproducibility
set.seed(1234)
# Save number of rows in dataset
#n <- nrow(pokedex)
n <- 801
# Generate 60% sample of rows
sample_rows <- sample(n, 0.6 * n)
# Use `sample_rows` to create training set
pokedex_train <- df_source[sample_rows,]
# The left is test set
pokedex_test <- df_source[-sample_rows,]
# Replace NAs in test set with sample mean
for (col in colnames(pokedex_test)) {
  if (any(is.na(pokedex_test[[col]]))) {
    mean_col <- mean(pokedex_test[[col]], na.rm = TRUE)
    pokedex_test[is.na(pokedex_test[[col]]), col] <- mean_col
  }
}

# (3) (ii)

is_legen_logi <- glm(is_legendary ~ attack + defense + height_m + hp + 
                  sp_attack + sp_defense + speed + type + weight_kg, 
                data = pokedex_train, family = "binomial")
summ_logi <- summary(is_legen_logi)
capture.output(summ_logi, file = "output/Q2_3_2_glm_summary.txt")

# (3) (iii)
pokedex_train_text <- pokedex_train
pokedex_train_text$is_legendary <- ifelse(pokedex_train_text$is_legendary == 0,
                             "Ordinary", "Legendary")
is_legen_tree <- rpart(is_legendary ~  attack + defense + height_m + hp +
                         sp_attack + sp_defense + speed + type + weight_kg, 
                       data = pokedex_train, method = "class")
is_legen_tree_text <- rpart(is_legendary ~  attack + defense + height_m + hp +
                         sp_attack + sp_defense + speed + type + weight_kg, 
                       data = pokedex_train_text, method = "class")
png("output/Q2_3_3_tree_plot.png", width = 600, height = 600)
rpart.plot(is_legen_tree_text, box.palette = "-BuOr") 
dev.off()

# Check my explanation on my rmd

# As we can see from the plot, the tree classified the data through `height_m, hp, sp_defense, sp_attack, attack, type` variables. Pokemons with ordinary `height_m, sp_defense, sp_attack` can be predicted as ordinary, making up 84% of all. In short, pokemons with outstanding height and fight stats tend to be legendary, vice versa. 

# 我们可以从图中看到，decision tree 通过 `height_m, hp, sp_defense, sp_attack, attack, type` 等变量进行了决策，其中身高、sp攻防不出色的宝可梦就被预测为一般宝可梦，占到了所有种类的84%。总而言之，拥有出众的身高、攻击数据的宝可梦更有可能是神兽，反之亦然。

# (3) (iv)

# remove the NA values
pokedex_train_rmNA <- na.omit(pokedex_train)
# forest
is_legen_forest <- randomForest(is_legendary ~  attack + defense + height_m + 
                                  hp + sp_attack + sp_defense + speed + 
                                  type + weight_kg, 
                          data = pokedex_train_rmNA, method = "class")
capture.output(is_legen_forest, cat("\n\n\n\nSummary:\n\n\n\n"), 
               summary(is_legen_forest), 
               file = "output/Q2_3_4_forest_sum.txt")

# (4)

# prediction

ROCRpred_logi <- predict(is_legen_logi, 
                     newdata = pokedex_test, type = "response") %>% 
  prediction(pokedex_test$is_legendary)

ROCRpred_tree <- predict(is_legen_tree, 
                         newdata = pokedex_test, type = "prob")[,2] %>% 
  prediction(pokedex_test$is_legendary)

ROCRpred_forest <- predict(is_legen_forest, 
                         newdata = pokedex_test, type = "prob")[,2] %>% 
  prediction(pokedex_test$is_legendary)

# plot

png("output/Q2_4_ROC_curves.png")
plot(performance(ROCRpred_logi, "tpr", "fpr"), col = 'pink', 
     main = "ROC Curves", xlab = "FPR", ylab = "TPR")
plot(performance(ROCRpred_tree, "tpr", "fpr"), col = 'lightgreen', add = TRUE)
plot(performance(ROCRpred_forest, "tpr", "fpr"), col = 'lightblue', add = TRUE)
legend("bottomright", legend = c(
  paste("Logistic Regression, AUC:", 
        round(performance(ROCRpred_logi, measure = "auc")@y.values[[1]], 4)),
  paste("Decision Tree, AUC:", 
        round(performance(ROCRpred_tree, measure = "auc")@y.values[[1]], 4)),
  paste("Random Forest, AUC:",
        round(performance(ROCRpred_forest, measure = "auc")@y.values[[1]], 4))
  ), 
  col = c("pink", "lightgreen", "lightblue"), lty = 1)
dev.off()

# As we can see from the curve, the Logistic Regression model and Random Forest model performs similarly well according to the AUC, while Decision Tree model is not as good as those two.

# 从图中我们可以发现，依据 AUC 进行判断，逻辑回归模型和随机森林模型表现地都很好，然而决策树模型就不如前两者表现得好。

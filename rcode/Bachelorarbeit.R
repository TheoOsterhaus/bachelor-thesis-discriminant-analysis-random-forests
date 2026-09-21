# ---- packages  ----
suppressPackageStartupMessages({
  library(MASS) # for lda function
  library(ggplot2) # for plotting
  library(gridExtra) # for arranging multiple plots
  library(dplyr) # for data manipulation
  library(tidyverse) # for data manipulation
  library(ggpubr) # for arranging multiple plots
  library(ISLR2) # for regression analysis
  library(randomForest) # for random forest)
  library(caret)
  library(mlbench) # for machine learning benchmark
  library(VIM) # for data mining with R
  library(tree) # for decision trees
  library(lattice)
})


set.seed(123) #global seed
# ---- Set Parameters ----
#global set parameters
n <- 500
n_per_class <- n / 2
mu_vec_A <- c(2, 3) # mean class A
mu_vec_B <- c(3, 5) # mean class B
#set parameters data 01
cov_mat <- matrix(c(0.5, 0, 0, 0.5), nrow = 2)
#set parameters data 02
cov_matA2 <- matrix(c(1, 0.8, 0.8, 1), nrow = 2) 
cov_matB2 <- matrix(c(1, -0.5, -0.5, 1), nrow = 2)
#set parameters data 03
area_A1 <- (-1 - (-2)) * (2 - (-2)) 
area_A2 <- (1 - (-1)) * (2 - 1)     
total_area_A <- area_A1 + area_A2
# A1: V1 in [-2, -1), V2 in [-2, 2]
# A2: V1 in [-1, 1), V2 in [1, 2]

# ---- Data Generation ----
#generating data01
generate_data01 <- function(n) {
  data01_A <- mvrnorm(n = n/2, mu = mu_vec_A, Sigma = cov_mat)
  data01_B <- mvrnorm(n = n/2, mu = mu_vec_B, Sigma = cov_mat)
  data01 <- rbind(data01_A, data01_B)
  data01 <- as.data.frame(data01)
  data01$class <- c(rep("A", n/2), rep("B", n/2))
  data01  <- data01[sample(1:n),] #shuffle data (randomly permute rows)
}
#generating data02
generate_data02 <- function(n) {
  data02_A <- mvrnorm(n = n/2, mu = mu_vec_A, Sigma = cov_matA2)
  data02_B <- mvrnorm(n = n/2, mu = mu_vec_B, Sigma = cov_matB2)
  data02 <- rbind(data02_A, data02_B)
  data02 <- as.data.frame(data02)
  data02$class <- c(rep("A", n/2), rep("B", n/2))
  data02  <- data02[sample(1:n),] #shuffle data (randomly permute rows)
}

#generating data03
generate_data03 <- function(n_per_class) {
  n_A1 <- round(n_per_class * (area_A1 / total_area_A))
  n_A2 <- n_per_class - n_A1
  
  # class a (green)
  data_A1_v1 <- runif(n_A1, min = -2, max = -1)
  data_A1_v2 <- runif(n_A1, min = -2, max = 2)
  data_A1 <- data.frame(V1 = data_A1_v1, V2 = data_A1_v2, class = "A")
  
  data_A2_v1 <- runif(n_A2, min = -1, max = 1)
  data_A2_v2 <- runif(n_A2, min = 1, max = 2)
  data_A2 <- data.frame(V1 = data_A2_v1, V2 = data_A2_v2, class = "A")
  
  data_class_A <- rbind(data_A1, data_A2)
  
  area_B1 <- (2 - (-1)) * (1 - (-2)) # 3 * 3 = 9
  area_B2 <- (2 - 1) * (2 - 1)       # 1 * 1 = 1
  total_area_B <- area_B1 + area_B2
  
  n_B1 <- round(n_per_class * (area_B1 / total_area_B))
  n_B2 <- n_per_class - n_B1
  
  # class b (yellow)
  data_B1_v1 <- runif(n_B1, min = -1, max = 2)
  data_B1_v2 <- runif(n_B1, min = -2, max = 1)
  data_B1 <- data.frame(V1 = data_B1_v1, V2 = data_B1_v2, class = "B")
  
  data_B2_v1 <- runif(n_B2, min = 1, max = 2)
  data_B2_v2 <- runif(n_B2, min = 1, max = 2)
  data_B2 <- data.frame(V1 = data_B2_v1, V2 = data_B2_v2, class = "B")
  
  data_class_B <- rbind(data_B1, data_B2)
  
  # combine
  data03 <- rbind(data_class_A, data_class_B)
  data03$class <- factor(data03$class)
  data03 <- data03[sample(1:nrow(data03)), ]
  return(data03)
}
data01 <- generate_data01(n)
data02 <- generate_data02(n)
data03 <- generate_data03(n_per_class)

# ---- Data Visualization ----
#plot data01
plot01 <- ggplot(data01, aes(x = V1, y = V2, color = factor(class))) + 
  geom_point(alpha = 0.7) + 
  theme_minimal()
print(plot01)
#plot data02
plot02 <- ggplot(data02, aes(x = V1, y = V2, color = factor(class))) + 
  geom_point(alpha = 0.7) + 
  theme_minimal()
print(plot02)

#plot data03
x_plot_min <- -2
x_plot_max <- 2
y_plot_min <- -2
y_plot_max <- 2
plot03_true <- ggplot(data03, aes(x = V1, y = V2, color = class)) +
  geom_point(alpha = 0.7) +
  labs(x = "V1", y = "V2") +
  theme_minimal() +
  coord_fixed()
print(plot03_true)

hist03_v1 <- ggplot(data03, aes(x = V1, fill = class)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 60) +
  labs(title = "Histogram of V1 by Class", x = "V1", y = "Count") +
  theme_minimal()
hist03_v2 <- ggplot(data03, aes(x = V2, fill = class)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 60) +
  labs(title = "Histogram of V2 by Class", x = "V2", y = "Count") +
  theme_minimal()

#bayes decision boundary data01
slope_bayes <- -0.5
intercept_bayes <- 5.25

plot_bayes_boundary_direct <- ggplot(data01, aes(x = V1, y = V2, color = class)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = intercept_bayes, slope = slope_bayes,
              color = "black", linetype = "dashed", size = 1) +
  labs(x = "V1", y = "V2") +
  theme_minimal() +
  coord_fixed()
print(plot_bayes_boundary_direct)

#bayes decision boundary data02
inv_cov_matA2 <- solve(cov_matA2)
inv_cov_matB2 <- solve(cov_matB2)
det_cov_matA2 <- det(cov_matA2)
det_cov_matB2 <- det(cov_matB2)

discriminant_score_A <- function(x, mu, inv_sigma, det_sigma) {
  diff <- x - mu
  score <- -0.5 * log(det_sigma) - 0.5 * (t(diff) %*% inv_sigma %*% diff)
  return(as.numeric(score))
}
x_range <- range(data02$V1)
y_range <- range(data02$V2)
grid_points <- expand.grid(V1 = seq(x_range[1], x_range[2], length.out = 100),
                           V2 = seq(y_range[1], y_range[2], length.out = 100))

grid_points$score_A <- apply(grid_points, 1, function(row) {
  discriminant_score_A(c(row["V1"], row["V2"]), mu_vec_A, inv_cov_matA2, det_cov_matA2)
})
grid_points$score_B <- apply(grid_points, 1, function(row) {
  discriminant_score_A(c(row["V1"], row["V2"]), mu_vec_B, inv_cov_matB2, det_cov_matB2)
})

grid_points$score_diff <- grid_points$score_A - grid_points$score_B

#final plot
plot_bayes_quadratic_direct <- ggplot(data02, aes(x = V1, y = V2, color = class)) +
  geom_point(alpha = 0.7) +
  geom_contour(data = grid_points, aes(z = score_diff),
               breaks = 0,
               color = "black", linetype = "dashed", size = 1) +
  labs(x = "V1", y = "V2") +
  theme_minimal() +
  coord_fixed()
print(plot_bayes_quadratic_direct)

#bayes decision boundary data03
plot_bayes_rf <- ggplot(data03, aes(x = V1, y = V2, color = class)) +
  geom_point(alpha = 0.7, size = 1) +
  annotate("segment", x = -1, y = -2, xend = -1, yend = 1, color = "black", linetype = "dashed", size = 1) +
  annotate("segment", x = -1, y = 1, xend = 1, yend = 1, color = "black", linetype = "dashed", size = 1) + 
  annotate("segment", x = 1, y = 1, xend = 1, yend = 2, color = "black", linetype = "dashed", size = 1) + 
  labs(x = "V1", y = "V2") +
  theme_minimal()
print(plot_bayes_rf)



# ---- Model fitting and evaluation ----
#model fit
data01$class <- as.factor(data01$class)
data02$class <- as.factor(data02$class)
data03$class <- as.factor(data03$class)
train_control_cv_real <- trainControl(method = "cv", number = 5)

#lda
model_lda_cv01 <- train(class ~ V1 + V2, data = data01, method = "lda",
      trControl = train_control_cv_real)
model_lda_cv02 <- train(class ~ V1 + V2, data = data02, method = "lda",
      trControl = train_control_cv_real)
model_lda_cv03 <- train(class ~ V1 + V2, data = data03, method = "lda",
      trControl = train_control_cv_real)
#qda
model_qda_cv01 <- train(class ~ V1 + V2, data = data01, method = "qda",
      trControl = train_control_cv_real)
model_qda_cv02 <- train(class ~ V1 + V2, data = data02, method = "qda",
      trControl = train_control_cv_real)
model_qda_cv03 <- train(class ~ V1 + V2, data = data03, method = "qda",
      trControl = train_control_cv_real)
#random forest
model_rf_cv01 <- randomForest(class ~ V1 + V2, data = data01, ntree = 20,
                              mtry = 1, importance = TRUE)
model_rf_cv02 <- randomForest(class ~ V1 + V2, data = data02, ntree = 20,
                              mtry = 1, importance = TRUE)
model_rf_cv03 <- randomForest(class ~ V1 + V2, data = data03, ntree = 20,
                              mtry = 1, importance = TRUE)


#sample decisiontree
datatree01 <-generate_data02(n)
datatree01$class <- as.factor(datatree01$class)
n_sim <- 250
#model_tree_cv01
tree_01 <- tree(class ~ V1 + V2, data = datatree01)
plot(tree_01)
text(tree_01, pretty = 1, cex = 0.6)

tree_cv01 <- cv.tree(tree_01)
plot(tree_cv01$size, tree_cv01$dev, type = "b",
     xlab = "Tree Size", ylab = "Deviance", main = "Cross-Validation for Tree Size")
prunetree01 <- prune.tree(tree_01, best = 3)
plot(prunetree01)
text(prunetree01, pretty = 1, cex = 0.6)

error01_dttest <- numeric(n_sim)
conf_matrices_dttest <- array(0, dim = c(2, 2, n_sim),
                              dimnames = list(Predicted = c("A", "B"),
                                              Actual = c("A", "B"), Simulation = 1:n_sim))
for (i in 1:n_sim) {
  simdata01_test <- generate_data01(n_sim) # generating new data
  dt01pred <- predict(prunetree01, simdata01_test, type = "class") # predict
  error01_dttest[i] <- mean(dt01pred != simdata01_test$class)
  conf_matrices_dttest[,,i] <- table(dt01pred, simdata01_test$class)
}
boxplot(error01_dttest, main = "error",
        ylab = "errorrate", xlab = "sim")
mean(error01_dttest)
error01_ldatest <- numeric(n_sim)
conf_matrices_lda01 <- array(0, dim = c(2, 2, n_sim),
                             dimnames = list(Predicted = c("A", "B"),
                                             Actual = c("A", "B"), Simulation = 1:n_sim))



# ---- Prediction and Evaluation ----
#lda01pred
error01_ldatest <- numeric(n_sim)
conf_matrices_lda01 <- array(0, dim = c(2, 2, n_sim),
                            dimnames = list(Predicted = c("A", "B"),
                                            Actual = c("A", "B"), Simulation = 1:n_sim))

for (i in 1:n_sim) {
  simdata01_test <- generate_data01(n_sim)# generating new data
  lda01pred <- predict(model_lda_cv01, simdata01_test)# predict
  
  error01_ldatest[i] <- mean(lda01pred != simdata01_test$class)
  conf_matrices_lda01[,,i] <- table(lda01pred, simdata01_test$class)
}
boxplot(error01_ldatest, main = "meanerrorlda01",
        ylab = "errorrate", xlab = "simulation")
hist(error01_ldatest, main = "histerrorlda01",
     xlab = "error", ylab = "count", breaks = 20)
error01_ldatest_mean <- mean(error01_ldatest)
print(error01_ldatest_mean)
avg_conf_matrix_lda01 <- apply(conf_matrices_lda01, c(1, 2), mean)
print(avg_conf_matrix_lda01)


#lda02pred
error02_ldatest <- numeric(n_sim)
conf_matrices_lda02 <- array(0, dim = c(2, 2, n_sim),
                             dimnames = list(Predicted = c("A", "B"),
                                             Actual = c("A", "B"), Simulation = 1:n_sim))
for (i in 1:n_sim) {
  simdata02_test <- generate_data02(n_sim)
  lda02pred <- predict(model_lda_cv02, simdata02_test)
  
  error02_ldatest[i] <- mean(lda02pred != simdata02_test$class)
  conf_matrices_lda02[,,i] <- table(lda02pred, simdata02_test$class)
}
boxplot(error02_ldatest, main = "meanerrorlda02",
        ylab = "errorrate", xlab = "simulation")
hist(error02_ldatest, main = "histerror",
     xlab = "errorrate", ylab = "count", breaks = 20)

error02_test_mean <- mean(error02_ldatest)
print(error02_test_mean)
avg_conf_matrix_lda02 <- apply(conf_matrices_lda02,c(1, 2), mean)
print(avg_conf_matrix_lda02)

#lda03pred
error03_ldatest <- numeric(n_sim)
conf_matrices_lda03 <- array(0, dim = c(2, 2, n_sim),
                             dimnames = list(Predicted = c("A", "B"),
                                             Actual = c("A", "B"), Simulation = 1:n_sim))
for (i in 1:n_sim) {
  simdata03 <- generate_data03(125)
  lda03pred <- predict(model_lda_cv03, simdata03)
  
  error03_ldatest[i] <- mean(lda03pred != simdata03$class)
  conf_matrices_lda03[,,i] <- table(lda03pred, simdata03$class)
}
par(mfrow = c(1, 2))
boxplot(error03_ldatest, main = "meanerrorlda3",
        ylab = "errorrate", xlab = "simulation")
hist(error03_ldatest, main = "histerror",
     xlab = "errorrate", ylab = "count", breaks = 20)

error03_ldatest_mean <- mean(error03_ldatest)
print(error03_ldatest_mean)
avg_conf_matrix_lda03 <- apply(conf_matrices_lda03, c(1, 2), mean)
print(avg_conf_matrix_lda03)


#qda01pred
error01_qdatest <- numeric(n_sim)
conf_matrices_qda01 <- array(0, dim = c(2, 2, n_sim),
                             dimnames = list(Predicted = c("A", "B"),
                                             Actual = c("A", "B"), Simulation = 1:n_sim))
for (i in 1:n_sim) {
  simdata01 <- generate_data01(n_sim) # generating new data
  qda01pred <- predict(model_qda_cv01, simdata01)
  
  error01_qdatest[i] <- mean(qda01pred != simdata01$class)
  conf_matrices_qda01[,,i] <- table(qda01pred, simdata01$class)
}
par(mfrow = c(1, 2))
boxplot(error01_qdatest, main = "meanerrorqda01",
        ylab = "errorrate", xlab = "simulation")
hist(error01_qdatest, main = "histerror",
     xlab = "errorrate", ylab = "count", breaks = 20)

error01_qdatest_mean <- mean(error01_qdatest)
print(error01_qdatest_mean)
avg_conf_matrix_qda01 <- apply(conf_matrices_qda01, c(1, 2), mean)
print(avg_conf_matrix_qda01)

#qda02pred
error02_qdatest <- numeric(n_sim)
conf_matrices_qda02 <- array(0, dim = c(2, 2, n_sim),
                             dimnames = list(Predicted = c("A", "B"),
                                             Actual = c("A", "B"), Simulation = 1:n_sim))
for (i in 1:n_sim) {
  simdata02 <- generate_data02(n_sim) # generating new data
  qda02pred <- predict(model_qda_cv02, simdata02)
  
  error02_qdatest[i] <- mean(qda02pred != simdata02$class)
  conf_matrices_qda02[,,i] <- table(qda02pred, simdata02$class)
}
par(mfrow = c(1, 2))
boxplot(error02_qdatest, main = "meanerrorqda02",
        ylab = "errorrate", xlab = "simulation")
hist(error02_qdatest, main = "histerror",
     xlab = "errorrate", ylab = "count", breaks = 20)
error02_qdatest_mean <- mean(error02_qdatest)
print(error02_qdatest_mean)
avg_conf_matrix_qda02 <- apply(conf_matrices_qda02, c(1, 2), mean)
print(avg_conf_matrix_qda02)

#qda03pred
error03_qdatest <- numeric(n_sim)
conf_matrices_qda03 <- array(0, dim = c(2, 2, n_sim),
                             dimnames = list(Predicted = c("A", "B"),
                                             Actual = c("A", "B"), Simulation = 1:n_sim))
for (i in 1:n_sim) {
  simdata03 <- generate_data03(125) # generating new data
  qda03pred <- predict(model_qda_cv03, simdata03)
  
  error03_qdatest[i] <- mean(qda03pred != simdata03$class)
  conf_matrices_qda03[,,i] <- table(qda03pred, simdata03$class)
}
par(mfrow = c(1, 2))
boxplot(error03_qdatest, main = "meanerrorqda03",
        ylab = "errorrate", xlab = "simulation")
hist(error03_qdatest, main = "histerror",
     xlab = "errorrate", ylab = "count", breaks = 20)

error03_qdatest_mean <- mean(error03_qdatest)
print(error03_qdatest_mean)
avg_conf_matrix_qda03 <- apply(conf_matrices_qda03, c(1, 2), mean)
print(avg_conf_matrix_qda03)

#rf01pred
error01_rftest <- numeric(n_sim)
conf_matrices_rf01 <- array(0, dim = c(2, 2, n_sim),
                           dimnames = list(Predicted = c("A", "B"),
                          Actual = c("A", "B"), Simulation = 1:n_sim))
for (i in 1:n_sim) {
  simdata01 <- generate_data01(n_sim) # generating new data
  rf01pred <- predict(model_rf_cv01, simdata01)
  
  error01_rftest[i] <- mean(rf01pred != simdata01$class)
  conf_matrices_rf01[,,i] <- table(rf01pred, simdata01$class)
}
par(mfrow = c(1, 2))
boxplot(error01_rftest, main = "meanerrorrf01",
        ylab = "errorrate", xlab = "simulation")
hist(error01_rftest, main = "histerror",
     xlab = "errorrate", ylab = "count", breaks = 20)
error01_rftest_mean <- mean(error01_rftest)
print(error01_rftest_mean)
avg_conf_matrix_rf01 <- apply(conf_matrices_rf01, c(1, 2), mean)
print(avg_conf_matrix_rf01)


#rf02pred
error02_rftest <- numeric(n_sim)
conf_matrices_rf02 <- array(0, dim = c(2, 2, n_sim),
                            dimnames = list(Predicted = c("A", "B"),
                                            Actual = c("A", "B"), Simulation = 1:n_sim))
for (i in 1:n_sim) {
  simdata02 <- generate_data02(n_sim) # generating new data
  rf02pred <- predict(model_rf_cv02, simdata02)
  
  error02_rftest[i] <- mean(rf02pred != simdata02$class)
  conf_matrices_rf02[,,i] <- table(rf02pred, simdata02$class)
}
par(mfrow = c(1, 2))
boxplot(error02_rftest, main = "meanerrorrf02",
        ylab = "errorrate", xlab = "simulation")
hist(error02_rftest, main = "histerror",
     xlab = "errorrate", ylab = "count", breaks = 20)

error02_rftest_mean <- mean(error02_rftest)
print(error02_rftest_mean)
avg_conf_matrix_rf02 <- apply(conf_matrices_rf02, c(1, 2), mean)
print(avg_conf_matrix_rf02)


#rf03pred
error03_rftest <- numeric(n_sim)
conf_matrices_rf03 <- array(0, dim = c(2, 2, n_sim),
                            dimnames = list(Predicted = c("A", "B"),
                                            Actual = c("A", "B"), Simulation = 1:n_sim))
for (i in 1:n_sim) {
  simdata03 <- generate_data03(125) # generating new data
  rf03pred <- predict(model_rf_cv03, simdata03)
  
  error03_rftest[i] <- mean(rf03pred != simdata03$class)
  conf_matrices_rf03[,,i] <- table(rf03pred, simdata03$class)
}
par(mfrow = c(1, 2))
boxplot(error03_rftest, main = "meanerrorrf03",
        ylab = "errorrate", xlab = "simulation")
hist(error03_rftest, main = "histerror",
     xlab = "errorrate", ylab = "count", breaks = 20)
error03_rftest_mean <- mean(error03_rftest)
print(error03_rftest_mean)
avg_conf_matrix_rf03 <- apply(conf_matrices_rf03, c(1, 2), mean)
print(avg_conf_matrix_rf03)






# ---- Boxplots and Metrics ----
#dataframe containing all error rates data01
df_errors01 <- data.frame(
  datalda = error01_ldatest,
  dataqda = error01_qdatest,
  datarf = error01_rftest
)
df_errors_long <- df_errors01 %>%
  pivot_longer(
    cols = everything(),
    names_to = "Modell",
    values_to = "errorrate"
  )

df_errors_long$Modell <- factor(df_errors_long$Modell,
                                levels = c("datalda", "dataqda", "datarf"),
                                labels = c("LDA (Data 01)", "QDA (Data 01)", "Random Forest (Data 01)"))
                                


#plotcombindedboxplotdata01
plot_combined_boxplots01 <- ggplot(df_errors_long, aes(x = Modell, y = errorrate, fill = Modell)) +
  geom_boxplot() +
  labs(
    x = "classification model",
    y = "error rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("LDA (Data 01)" = "lightblue",
                               "QDA (Data 01)" = "lightgreen",
                               "Random Forest (Data 01)" = "lightcoral"))

print(plot_combined_boxplots01)

#precision
precision_lda01 <- avg_conf_matrix_lda01[1, 1] / sum(avg_conf_matrix_lda01[1, ])
precision_qda01 <- avg_conf_matrix_qda01[1, 1] / sum(avg_conf_matrix_qda01[1, ])
precision_rf01 <- avg_conf_matrix_rf01[1, 1] / sum(avg_conf_matrix_rf01[1, ])
#recall
recall_lda01 <- avg_conf_matrix_lda01[1, 1] / sum(avg_conf_matrix_lda01[, 1])
recall_qda01 <- avg_conf_matrix_qda01[1, 1] / sum(avg_conf_matrix_qda01[, 1])
recall_rf01 <- avg_conf_matrix_rf01[1, 1] / sum(avg_conf_matrix_rf01[, 1])
# F1-Score
f1_score_lda01 <- 2 * (precision_lda01 * recall_lda01) / (precision_lda01 + recall_lda01)
f1_score_qda01 <- 2 * (precision_qda01 * recall_qda01) / (precision_qda01 + recall_qda01)
f1_score_rf01 <- 2 * (precision_rf01 * recall_rf01) / (precision_rf01 + recall_rf01)

print(c("Precision", round(precision_lda01, 4), round(precision_qda01, 4), round(precision_rf01, 4)))
print(c("Recall", round(recall_lda01, 4), round(recall_qda01, 4), round(recall_rf01, 4)))
print(c("F1-Score", round(f1_score_lda01, 4), round(f1_score_qda01, 4), round(f1_score_rf01, 4)))

#dataframe containing all error rates data02
df_errors02 <- data.frame(
  datalda = error02_ldatest, 
  dataqda = error02_qdatest,
  datarf = error02_rftest   
)

df_errors_long02 <- df_errors02 %>%
  pivot_longer(
    cols = everything(),  
    names_to = "Modell",
    values_to = "errorrate" 
  )
df_errors_long02$Modell <- factor(df_errors_long02$Modell,
                                levels = c("datalda", "dataqda", "datarf"), 
                                labels = c("LDA (Data 02)", "QDA (Data 02)", "Random Forest (Data 02)"))


#plot combined boxplots data02
plot_combined_boxplots02 <- ggplot(df_errors_long02, aes(x = Modell, y = errorrate, fill = Modell)) +
  geom_boxplot() +
  labs(
    x = "classification model", 
    y = "error rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none" 
  ) +
  scale_fill_manual(values = c("LDA (Data 02)" = "lightblue",
                               "QDA (Data 02)" = "lightgreen",
                               "Random Forest (Data 02)" = "lightcoral"))

print(plot_combined_boxplots02)

#precision
precision_lda02 <- avg_conf_matrix_lda02[1, 1] / sum(avg_conf_matrix_lda02[1, ])
precision_qda02 <- avg_conf_matrix_qda02[1, 1] / sum(avg_conf_matrix_qda02[1, ])
precision_rf02 <- avg_conf_matrix_rf02[1, 1] / sum(avg_conf_matrix_rf02[1, ])
#recall
recall_lda02 <- avg_conf_matrix_lda02[1, 1] / sum(avg_conf_matrix_lda02[, 1])
recall_qda02 <- avg_conf_matrix_qda02[1, 1] / sum(avg_conf_matrix_qda02[, 1])
recall_rf02 <- avg_conf_matrix_rf02[1, 1] / sum(avg_conf_matrix_rf02[, 1])
# F1-Score
f1_score_lda02 <- 2 * (precision_lda02 * recall_lda02) / (precision_lda02 + recall_lda02)
f1_score_qda02 <- 2 * (precision_qda02 * recall_qda02) / (precision_qda02 + recall_qda02)
f1_score_rf02 <- 2 * (precision_rf02 * recall_rf02) / (precision_rf02 + recall_rf02)

print(c("Precision", round(precision_lda02, 4), round(precision_qda02, 4), round(precision_rf02, 4)))
print(c("Recall", round(recall_lda02, 4), round(recall_qda02, 4), round(recall_rf02, 4)))
print(c("F1-Score", round(f1_score_lda02, 4), round(f1_score_qda02, 4), round(f1_score_rf02, 4)))

#plotscomp03
#create a dataframe containing all error rates data03
df_errors03 <- data.frame(
  datalda = error03_ldatest, 
  dataqda = error03_qdatest,
  datarf = error03_rftest 
)
df_errors_long <- df_errors03 %>%
  pivot_longer(
    cols = everything(), 
    names_to = "Modell",
    values_to = "errorrate" 
  )
df_errors_long$Modell <- factor(df_errors_long$Modell,
                                levels = c("datalda", "dataqda", "datarf"), 
                                labels = c("LDA (Data 03)", "QDA (Data 03)", "Random Forest (Data 03)"))

#plot combined boxplots data03
plot_combined_boxplots03 <- ggplot(df_errors_long, aes(x = Modell, y = errorrate, fill = Modell)) +
  geom_boxplot() +
  labs(
    x = "classification model", 
    y = "error rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none" 
  ) +
  scale_fill_manual(values = c("LDA (Data 03)" = "lightblue",
                               "QDA (Data 03)" = "lightgreen",
                               "Random Forest (Data 03)" = "lightcoral"))

print(plot_combined_boxplots03)
#precision
precision_lda03 <- avg_conf_matrix_lda03[1, 1] / sum(avg_conf_matrix_lda03[1, ])
precision_qda03 <- avg_conf_matrix_qda03[1, 1] / sum(avg_conf_matrix_qda03[1, ])
precision_rf03 <- avg_conf_matrix_rf03[1, 1] / sum(avg_conf_matrix_rf03[1, ])
#recall
recall_lda03 <- avg_conf_matrix_lda03[1, 1] / sum(avg_conf_matrix_lda03[, 1])
recall_qda03 <- avg_conf_matrix_qda03[1, 1] / sum(avg_conf_matrix_qda03[, 1])
recall_rf03 <- avg_conf_matrix_rf03[1, 1] / sum(avg_conf_matrix_rf03[, 1])
# F1-Score
f1_score_lda03 <- 2 * (precision_lda03 * recall_lda03) / (precision_lda03 + recall_lda03)
f1_score_qda03 <- 2 * (precision_qda03 * recall_qda03) / (precision_qda03 + recall_qda03)
f1_score_rf03 <- 2 * (precision_rf03 * recall_rf03) / (precision_rf03 + recall_rf03)

print(c("Precision", round(precision_lda03, 4), round(precision_qda03, 4), round(precision_rf03, 4)))
print(c("Recall", round(recall_lda03, 4), round(recall_qda03, 4), round(recall_rf03, 4)))
print(c("F1-Score", round(f1_score_lda03, 4), round(f1_score_qda03, 4), round(f1_score_rf03, 4)))

# ---- ROC Curves and AUC ----
#ROCdata01
library(ROCR)
par(mfrow = c(1, 1))
data01ROC <- generate_data01(n_sim)
#ROCLDA
pred01_ldaROC <- predict(model_lda_cv01, data01ROC, type = "prob")
predroc_lda01 <- prediction(pred01_ldaROC[,"B"], data01ROC$class)
perform_lda01 <- performance(predroc_lda01, "tpr", "fpr")
plot(perform_lda01, colorize = TRUE, lwd = 1)
#ROCQDA
pred01_qdaROC <- predict(model_qda_cv01, data01ROC, type = "prob")
predroc_qda01 <- prediction(pred01_qdaROC[,"B"], data01ROC$class)
perform_qda01 <- performance(predroc_qda01, "tpr", "fpr")
plot(perform_qda01, colorize = TRUE, add = TRUE)
#ROCRF
pred01_rfROC <- predict(model_rf_cv01, data01ROC, type = "prob")
predroc_rf01 <- prediction(pred01_rfROC[,"B"], data01ROC$class)
perform_rf01 <- performance(predroc_rf01, "tpr", "fpr")
plot(perform_rf01, colorize = TRUE, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")
#AUC
AUC_lda01 <- performance(predroc_lda01, measure = "auc")
AUC_qda01 <- performance(predroc_qda01, measure = "auc")
AUC_rf01 <- performance(predroc_rf01, measure = "auc")
AUC01 <- c(AUC_lda01@y.values, AUC_qda01@y.values, AUC_rf01@y.values)
print(AUC01)

#ROCdata02
data02ROC <- generate_data02(n_sim)
#ROCLDA
pred02_ldaROC <- predict(model_lda_cv02, data02ROC, type = "prob")
predroc_lda02 <- prediction(pred02_ldaROC[,"B"], data02ROC$class)
perform_lda02 <- performance(predroc_lda02, "tpr", "fpr")
plot(perform_lda02, colorize = TRUE, lwd = 1)
#ROCQDA
pred02_qdaROC <- predict(model_qda_cv02, data02ROC, type = "prob")
predroc_qda02 <- prediction(pred02_qdaROC[,"B"], data02ROC$class)
perform_qda02 <- performance(predroc_qda02, "tpr", "fpr")
plot(perform_qda02, colorize = TRUE, add = TRUE)
#ROCRF
pred02_rfROC <- predict(model_rf_cv02, data02ROC, type = "prob")
predroc_rf02 <- prediction(pred02_rfROC[,"B"], data02ROC$class)
perform_rf02 <- performance(predroc_rf02, "tpr", "fpr")
plot(perform_rf02, colorize = TRUE, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")
#AUC
AUC_lda02 <- performance(predroc_lda02, measure = "auc")
AUC_qda02 <- performance(predroc_qda02, measure = "auc")
AUC_rf02 <- performance(predroc_rf02, measure = "auc")
AUC02 <- c(AUC_lda02@y.values, AUC_qda02@y.values, AUC_rf02@y.values)
print(AUC02)

#ROCdata03
data03ROC <- generate_data03(n_sim)
#ROCLDA
pred03_ldaROC <- predict(model_lda_cv03, data03ROC, type = "prob")
predroc_lda03 <- prediction(pred03_ldaROC[,"B"], data03ROC$class)
perform_lda03 <- performance(predroc_lda03, "tpr", "fpr")
plot(perform_lda03, colorize = TRUE, lwd = 1)
#ROCQDA
pred03_qdaROC <- predict(model_qda_cv03, data03ROC, type = "prob")
predroc_qda03 <- prediction(pred03_qdaROC[,"B"], data03ROC$class)
perform_qda03 <- performance(predroc_qda03, "tpr", "fpr")
plot(perform_qda03, colorize = TRUE, add = TRUE)
#ROCRF
pred03_rfROC <- predict(model_rf_cv03, data03ROC, type = "prob")
predroc_rf03 <- prediction(pred03_rfROC[,"B"], data03ROC$class)
perform_rf03 <- performance(predroc_rf03, "tpr", "fpr")
plot(perform_rf03, colorize = TRUE, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")
#AUC
AUC_lda03 <- performance(predroc_lda03, measure = "auc")
AUC_qda03 <- performance(predroc_qda03, measure = "auc")
AUC_rf03 <- performance(predroc_rf03, measure = "auc")
AUC03 <- c(AUC_lda03@y.values, AUC_qda03@y.values, AUC_rf03@y.values)
print(AUC03)





# ---- Pima Indians Diabetes Dataset ----

# <- knn(pima_data, pima_data, k = 5) # Impute missing values using k-NN
pima_data_imputed <- read.csv("pima_data_imputed.csv") # Load the imputed dataset
pima_data_imputed$diabetes <- as.factor(pima_data_imputed$diabetes)
colSums(is.na(pima_data_imputed)) # check for NAs

view(pima_data_imputed)
set.seed(123)
train_idx_real <- createDataPartition(pima_data_imputed$diabetes, p = 0.7, list = FALSE)
pima_train <- pima_data_imputed[train_idx_real, ]
pima_test <- pima_data_imputed[-train_idx_real, ]

#modellda
model_lda_cvpima <- train(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age,
                          data = pima_train,
                          method = "lda",
                          trControl = train_control_cv_real)
predpima_lda <- predict(model_lda_cvpima, pima_test)
conf_matrix_ldapima <- table(predpima_lda, pima_test$diabetes)
errorpima_testlda <- mean(predpima_lda != pima_test$diabetes)

#modelqda
model_qda_cvpima <- train(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age,
                          data = pima_train,
                          method = "qda",
                          trControl = train_control_cv_real)
predpima_qda <- predict(model_qda_cvpima, pima_test)
conf_matrix_qdapima <- table(predpima_qda, pima_test$diabetes)
errorpima_test_qda <- mean(predpima_qda != pima_test$diabetes)

#modelrf
model_rf_cvpima <- randomForest(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age,
                                data = pima_train,
                                ntree = 300,
                                mtry = 4,
                                importance = TRUE)
predpima_rf <- predict(model_rf_cvpima, pima_test)
conf_matrix_rfpima <- table(predpima_rf, pima_test$diabetes)
errorpima_test_rf <- mean(predpima_rf != pima_test$diabetes)


#evaluate metrics
#precision
precision_ldapima <- conf_matrix_ldapima[1, 1] / sum(conf_matrix_ldapima[1, ])
precision_qdapima <- conf_matrix_qdapima[1, 1] / sum(conf_matrix_qdapima[1, ])
precision_rfpima <- conf_matrix_rfpima[1, 1] / sum(conf_matrix_rfpima[1, ])
#recall
recall_ldapima <- conf_matrix_ldapima[1, 1] / sum(conf_matrix_ldapima[, 1])
recall_qdapima <- conf_matrix_qdapima[1, 1] / sum(conf_matrix_qdapima[, 1])
recall_rfpima <- conf_matrix_rfpima[1, 1] / sum(conf_matrix_rfpima[, 1])
# F1-Score
f1_score_ldapima <- 2 * (precision_ldapima * recall_ldapima) / (precision_ldapima + recall_ldapima)
f1_score_qdapima <- 2 * (precision_qdapima * recall_qdapima) / (precision_qdapima + recall_qdapima)
f1_score_rfpima <- 2 * (precision_rfpima * recall_rfpima) / (precision_rfpima + recall_rfpima)

print(c("Error rate", round(errorpima_testlda, 4), round(errorpima_test_qda, 4), round(errorpima_test_rf, 4)))
print(c("Precision", round(precision_ldapima, 4), round(precision_qdapima, 4), round(precision_rfpima, 4)))
print(c("Recall", round(recall_ldapima, 4), round(recall_qdapima, 4), round(recall_rfpima, 4)))
print(c("F1-Score", round(f1_score_ldapima, 4), round(f1_score_qdapima, 4), round(f1_score_rfpima, 4)))

#ROCLDA
predpima_ldaROC <- predict(model_lda_cvpima, pima_test, type = "prob")
predrocpima_lda <- prediction(predpima_ldaROC[,"pos"], pima_test$diabetes)
perform_lda03 <- performance(predrocpima_lda, "tpr", "fpr")
plot(perform_lda03, colorize = TRUE, lwd = 1)
#ROCQDA
predpima_qdaROC <- predict(model_qda_cvpima, pima_test, type = "prob")
predrocpima_qda <- prediction(predpima_qdaROC[,"pos"], pima_test$diabetes)
perform_qda03 <- performance(predrocpima_qda, "tpr", "fpr")
plot(perform_qda03, colorize = TRUE, add = TRUE)
#ROCRF
predpima_rfROC <- predict(model_rf_cvpima, pima_test, type = "prob")
predrocpima_rf <- prediction(predpima_rfROC[,"pos"], pima_test$diabetes)
perform_rf03 <- performance(predrocpima_rf, "tpr", "fpr")
plot(perform_rf03, colorize = TRUE, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")
#AUC
AUC_ldapima <- performance(predrocpima_lda, measure = "auc")
AUC_qdapima <- performance(predrocpima_qda, measure = "auc")
AUC_rfpima <- performance(predrocpima_rf, measure = "auc")
AUC <- c(AUC_ldapima@y.values, AUC_qdapima@y.values, AUC_rfpima@y.values)
print(AUC)

#adjust thresholdrf
predpima_rfROC_adj <- rep("neg", nrow(pima_test)) # Initialize with "neg"
predpima_rfROC_adj[predpima_rfROC[,"pos"] > 0.2] <- "pos" # Adjust threshold to 0.2
conf_matrices_rfpima_adj <- table(predpima_rfROC_adj, pima_test$diabetes)
conf_matrix_rfpima


#boxplot Insolin and Glucose
#head(pima_data_imputed, n=3)
par(mfrow = c(1, 2))
boxplot(pima_data_imputed$insulin ~ pima_data_imputed$diabetes,
        main = "Boxplot of Insulin by Diabetes Status",
        xlab = "Diabetes Status",
        ylab = "Insulin",)
boxplot(pima_data_imputed$glucose ~ pima_data_imputed$diabetes,
        main = "Boxplot of Glucose by Diabetes Status",
        xlab = "Diabetes Status",
        ylab = "Glucose",)
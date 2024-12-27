#1 EMPIRICAL PROBLEMS

#a
install.packages("randomForest")
library(randomForest)

#b
install.packages("neuralnet")
library(neuralnet)

#c
install.packages("dplyr")
library(dplyr)


n <- 1000
x1 <- rgamma(n, 2, 1)
x2 <- rnorm(n, 0, 2)
x3 <- rweibull(n, 2, 2)
x4 <- rlogis(n, 2, 1)
x5 <- rbeta(n, 2, 1)

x <- cbind(x1, x2, x3, x4, x5)

c1 <- var(x)

chol1 <- solve(chol(c1))
x <- x %*% chol1

R <- matrix(runif(ncol(x)^2, -1, 1), ncol = ncol(x))
RtR <- R %*% t(R)
corr <- cov2cor(RtR)

is_positive_definite <- sum(eigen(corr)$values > 0) == ncol(x)

if (is_positive_definite) {
  x <- x %*% chol(corr)
}

datam <- as.data.frame(x) %>%
  rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)

head(datam)



#d
set.seed(0)

n <- 1000

u <- rnorm(n, mean = 0, sd = 1)

datam$y_spec1 <- datam$x1 + datam$x3 * datam$x2^2 + (datam$x4 * datam$x1 * datam$x5) / 10

datam$y_spec2 <- log((abs(datam$x1^4 / 10) + abs(datam$x2) + datam$x3^2)) + datam$x4 * datam$x2 * sin(datam$x5) + u

train_index <- sample(1:n, size = n * 0.5)
train_data <- datam[train_index, ]
test_data <- datam[-train_index, ]

#Model 1
library(neuralnet)

nn_spec1 <- neuralnet(
  y_spec1 ~ x1 + x2 + x3 + x4 + x5,
  data = train_data,
  hidden = c(64, 32, 16),
  act.fct = "logistic",
  stepmax = 1e6 
)

nn_spec2 <- neuralnet(
  y_spec2 ~ x1 + x2 + x3 + x4 + x5,
  data = train_data,
  hidden = c(64, 32, 16),
  act.fct = "logistic",
  stepmax = 1e6
)

pred_nn_spec1 <- predict(nn_spec1, test_data[, c("x1", "x2", "x3", "x4", "x5")])
mse_nn_spec1 <- mean((test_data$y_spec1 - pred_nn_spec1)^2)

pred_nn_spec2 <- predict(nn_spec2, test_data[, c("x1", "x2", "x3", "x4", "x5")])
mse_nn_spec2 <- mean((test_data$y_spec2 - pred_nn_spec2)^2)


cat("MSE for Neural Net - Specification 1:", mse_nn_spec1, "\n")
cat("MSE for Neural Net - Specification 2:", mse_nn_spec2, "\n")



#Model 2
poly_model_spec1 <- lm(y_spec1 ~ poly(x1, 3) + poly(x2, 3) + poly(x3, 3) + poly(x4, 3) + poly(x5, 3), data = train_data)

poly_model_spec2 <- lm(y_spec2 ~ poly(x1, 3) + poly(x2, 3) + poly(x3, 3) + poly(x4, 3) + poly(x5, 3), data = train_data)

pred_poly_spec1 <- predict(poly_model_spec1, newdata = test_data)
mse_poly_spec1 <- mean((test_data$y_spec1 - pred_poly_spec1)^2)

pred_poly_spec2 <- predict(poly_model_spec2, newdata = test_data)
mse_poly_spec2 <- mean((test_data$y_spec2 - pred_poly_spec2)^2)

cat("MSE for Polynomial Regression - Specification 1:", mse_poly_spec1, "\n")
cat("MSE for Polynomial Regression - Specification 2:", mse_poly_spec2, "\n")

#Model 3
library(randomForest)

rf_model_spec1 <- randomForest(y_spec1 ~ ., data = train_data, ntree = 1000, mtry = 4)

rf_model_spec2 <- randomForest(y_spec2 ~ ., data = train_data, ntree = 1000, mtry = 4)

pred_rf_spec1 <- predict(rf_model_spec1, newdata = test_data)
mse_rf_spec1 <- mean((test_data$y_spec1 - pred_rf_spec1)^2)

pred_rf_spec2 <- predict(rf_model_spec2, newdata = test_data)
mse_rf_spec2 <- mean((test_data$y_spec2 - pred_rf_spec2)^2)

cat("MSE for Random Forest - Specification 1:", mse_rf_spec1, "\n")
cat("MSE for Random Forest - Specification 2:", mse_rf_spec2, "\n")

#e
library(neuralnet)
library(randomForest)
library(dplyr)

results <- data.frame(
  Model = c("NeuralNet_Spec1", "Poly_Spec1", "RF_Spec1", 
            "NeuralNet_Spec2", "Poly_Spec2", "RF_Spec2"),
  MSEs = numeric(6)
)

num_reps <- 50

set.seed(0) 
for (i in 1:num_reps)
  
  train_index <- sample(1:n, size = n * 0.5)
  train_data <- datam[train_index, ]
  test_data <- datam[-train_index, ]
  
  # Model 1: Neural Network
  nn_spec1 <- neuralnet(y_spec1 ~ x1 + x2 + x3 + x4 + x5, data = train_data, hidden = c(64, 32, 16), act.fct = "logistic", stepmax = 1e6)
  nn_spec2 <- neuralnet(y_spec2 ~ x1 + x2 + x3 + x4 + x5, data = train_data, hidden = c(64, 32, 16), act.fct = "logistic", stepmax = 1e6)
  
  pred_nn_spec1 <- predict(nn_spec1, test_data[, c("x1", "x2", "x3", "x4", "x5")])
  pred_nn_spec2 <- predict(nn_spec2, test_data[, c("x1", "x2", "x3", "x4", "x5")])
  
  mse_nn_spec1 <- mean((test_data$y_spec1 - pred_nn_spec1)^2)
  mse_nn_spec2 <- mean((test_data$y_spec2 - pred_nn_spec2)^2)
  
  # Model 2: Polynomial Regression
  poly_model_spec1 <- lm(y_spec1 ~ poly(x1, 3) + poly(x2, 3) + poly(x3, 3) + poly(x4, 3) + poly(x5, 3), data = train_data)
  poly_model_spec2 <- lm(y_spec2 ~ poly(x1, 3) + poly(x2, 3) + poly(x3, 3) + poly(x4, 3) + poly(x5, 3), data = train_data)
  
  pred_poly_spec1 <- predict(poly_model_spec1, newdata = test_data)
  pred_poly_spec2 <- predict(poly_model_spec2, newdata = test_data)
  
  mse_poly_spec1 <- mean((test_data$y_spec1 - pred_poly_spec1)^2)
  mse_poly_spec2 <- mean((test_data$y_spec2 - pred_poly_spec2)^2)
  
  # Model 3: Random Forest
  rf_model_spec1 <- randomForest(y_spec1 ~ ., data = train_data, ntree = 1000, mtry = 4)
  rf_model_spec2 <- randomForest(y_spec2 ~ ., data = train_data, ntree = 1000, mtry = 4)
  
  pred_rf_spec1 <- predict(rf_model_spec1, newdata = test_data)
  pred_rf_spec2 <- predict(rf_model_spec2, newdata = test_data)
  
  mse_rf_spec1 <- mean((test_data$y_spec1 - pred_rf_spec1)^2)
  mse_rf_spec2 <- mean((test_data$y_spec2 - pred_rf_spec2)^2)
  
  
  mse_accumulator <- numeric(length(results$Model)) 
  
  mse_accumulator[results$Model == "NeuralNet_Spec1"] <- mse_accumulator[results$Model == "NeuralNet_Spec1"] + mse_nn_spec1
  mse_accumulator[results$Model == "Poly_Spec1"] <- mse_accumulator[results$Model == "Poly_Spec1"] + mse_poly_spec1
  mse_accumulator[results$Model == "RF_Spec1"] <- mse_accumulator[results$Model == "RF_Spec1"] + mse_rf_spec1
  
  mse_accumulator[results$Model == "NeuralNet_Spec2"] <- mse_accumulator[results$Model == "NeuralNet_Spec2"] + mse_nn_spec2
  mse_accumulator[results$Model == "Poly_Spec2"] <- mse_accumulator[results$Model == "Poly_Spec2"] + mse_poly_spec2
  mse_accumulator[results$Model == "RF_Spec2"] <- mse_accumulator[results$Model == "RF_Spec2"] + mse_rf_spec2
  
  
  results$MSEs <- results$MSEs / num_reps
  
  print(results)
  

  
  #2
  
  #a
  install.packages("ggplot2")
  install.packages("factoextra")
  library(ggplot2)
  library(factoextra)
  
  pca_data <- train_data[, !(names(train_data) %in% "price")]
  
  str(pca_data)
  pca_data_numeric <- pca_data[, sapply(pca_data, is.numeric)]
  pca_data$host_experience <- as.numeric(pca_data$host_experience)
  pca_result <- prcomp(pca_data_numeric, center = TRUE, scale. = TRUE)
  
 
  summary_pca <- summary(pca_result)
  print(summary_pca)
  
  pve <- summary_pca$importance[2,]  
  
  cumulative_pve <- summary_pca$importance[3,]  
  
  pve_plot <- data.frame(
    PC = 1:length(pve),
    PVE = pve,
    Cumulative_PVE = cumulative_pve
  )
  
  ggplot(pve_plot[1:4,], aes(x = PC)) +
    geom_bar(aes(y = PVE), stat = "identity", fill = "skyblue", alpha = 0.7) +
    geom_line(aes(y = Cumulative_PVE, group = 1), color = "red", size = 1) +
    geom_point(aes(y = Cumulative_PVE), color = "red") +
    labs(title = "Proportion of Variance Explained (Top 4 PCs)",
         y = "Proportion of Variance Explained",
         x = "Principal Components") +
    theme_minimal()
  
  top_4_pcs <- head(order(pve, decreasing = TRUE), 4)
  cat("Top 4 PCs by PVE:", top_4_pcs, "\n")

  
#b
  install.packages("caret")
  install.packages("lattice")
  library(lattice)
  library(ggplot2)
  library(caret)
  library(e1071) 
  library(MASS) 
  
  X_train <- train_data[, -which(names(train_data) == "price")]
  y_train <- train_data$price
  X_test <- test_data[, -which(names(test_data) == "price")]
  y_test <- test_data$price
  
  str(X_train)
  X_train_numeric <- X_train[, sapply(X_train, is.numeric)]
  sum(is.na(X_train_numeric))
  
  pca_model <- prcomp(X_train_numeric, scale. = TRUE)
  
  summary(pca_model)
  
  X_train_pca <- as.data.frame(pca_model$x[, 1:4]) 
  X_test_pca <- as.data.frame(predict(pca_model, newdata = X_test)[, 1:4]) 
  
  print(dim(X_test))
  print(dim(X_train))
  if (ncol(X_test) == 0) {
    X_test <- matrix(NA, nrow = nrow(X_test), ncol = ncol(X_train))
  }
  colnames(X_test) <- colnames(X_train)
  
  X_test_pca <- as.data.frame(predict(pca_model, newdata = X_test)[, 1:4]) # Top 4 PCs for test data
  
  lm_model <- lm(y_train ~ ., data = X_train_pca)
  y_pred <- predict(lm_model, newdata = X_test_pca)
  
  test_mse <- mean((y_test - y_pred)^2)
  cat("Test MSE:", round(test_mse, 2), "\n")
  
  
#c 
  library(randomForest)
  
  p <- ncol(X_train)
  mtry_value <- floor(sqrt(p))
  
  rf_model <- randomForest(X_train, y_train, ntree = 1000, mtry = mtry_value)

  print(rf_model)
  
  rf_predictions <- predict(rf_model, newdata = X_train)
  rf_mse <- mean((rf_predictions - y_train)^2)
  cat("Random Forest Mean Squared Error (MSE):", rf_mse, "\n")

#d
  pca_data <- train_data[, !(names(train_data) %in% "price")]
  sum(is.na(pca_data))
  pca_data_numeric <- pca_data[, sapply(pca_data, is.numeric)]
  pca_result <- prcomp(pca_data_numeric, center = TRUE, scale. = TRUE)
  summary_pca <- summary(pca_result)
  print(summary_pca)
  pve <- summary_pca$importance[2,]
  cumulative_pve <- summary_pca$importance[3,]
  pve_plot <- data.frame(
    PC = 1:length(pve),
    PVE = pve,
    Cumulative_PVE = cumulative_pve
  )
  ggplot(pve_plot[1:4,], aes(x = PC)) +
    geom_bar(aes(y = PVE), stat = "identity", fill = "skyblue", alpha = 0.7) +
    geom_line(aes(y = Cumulative_PVE, group = 1), color = "red", size = 1) +
    geom_point(aes(y = Cumulative_PVE), color = "red") +
    labs(title = "Proportion of Variance Explained (Top 4 PCs)",
         y = "Proportion of Variance Explained", x = "Principal Components") +
    theme_minimal()
  top_4_pcs <- head(order(pve, decreasing = TRUE), 4)
  cat("Top 4 PCs by PVE:", top_4_pcs, "\n")
  X_train <- train_data[, -which(names(train_data) == "price")]
  X_test <- test_data[, -which(names(test_data) == "price")] 
  X_train_numeric <- X_train[, sapply(X_train, is.numeric)]
  X_test_numeric <- X_test[, sapply(X_test, is.numeric)]
  pca_model <- prcomp(X_train_numeric, scale. = TRUE)
  X_train_pca <- as.data.frame(pca_model$x[, 1:4]) 
  X_test_pca <- as.data.frame(predict(pca_model, newdata = X_test_numeric)[, 1:4])
  print(dim(X_train_pca))
  print(dim(X_test_pca))  
  
 
  pca_model <- prcomp(X_train_numeric, scale. = TRUE)
  
  k <- 4

  X_train_reconstructed <- predict(pca_model, newdata = X_train_numeric)[, 1:k] %*% t(pca_model$rotation[, 1:k]) + pca_model$center
  
  X_train_numeric <- as.matrix(X_train_numeric)
  X_train_reconstructed <- as.matrix(X_train_reconstructed)
  
  if (nrow(X_train_numeric) == nrow(X_train_reconstructed) && ncol(X_train_numeric) == ncol(X_train_reconstructed)) {
    # Calculate the reconstruction error (MSE)
    reconstruction_error <- mean((X_train_numeric - X_train_reconstructed)^2)
    cat("Mean Squared Error (MSE) for PCA reconstruction with", k, "principal components:", reconstruction_error, "\n")
  } else {
    cat("Error: Dimensions of original and reconstructed data do not match.\n")
  }
  
  reconstruction_error <- mean((X_train_numeric - X_train_reconstructed)^2)
  
  cat("Mean Squared Error (MSE) for PCA reconstruction with", k, "principal components:", reconstruction_error, "\n")
  pca_mse <- reconstruction_error
  
  model_mse_table <- data.frame(
    Model = c("Second Order Polynomial Model", "PCA Model", "Random Forest Model", "Lasso Model", "Ridge Model"),
    Test_MSE = c(MSE.second, pca_mse, rf_mse, lasso_mse, ridge_mse)
  )
  
  print(model_mse_table)
  
  
  model_names <- c("Second Order Polynomial Model", "PCA Model", "Linear Model", "Logistic Model", "Random Forest")
  model_performance <- c(0.85, 0.88, 0.90, 0.87)  # 4 elements here
  df <- data.frame(Model = model_names, Performance = model_performance)
  
  model_performance <- c(0.85, 0.88, 0.90, 0.87, 0.89)  # Added one more value to match the length
  df <- data.frame(Model = model_names, Performance = model_performance)
  model_names <- model_names[1:4]  # Trimmed to 4 elements
  df <- data.frame(Model = model_names, Performance = model_performance)
  
  
  
  
#e
  
  
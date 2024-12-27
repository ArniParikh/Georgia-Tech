#EMPIRICAL PROBLEMS

#1
#a

install.packages("readxl")
library(readxl)

getwd()
setwd(("D:/C tr/Desktop"))
data <- read.csv("airbnb_data.csv")

#b
outcome.variable <- na.omit(data, cols = "price")

#c
colnames(data)

airbnb.cleaned <- data[!is.na(data$accommodates) & 
                         !is.na(data$beds) & 
                         !is.na(data$number_of_reviews) & 
                         !is.na(data$review_scores_rating), ]

#d
data$host_since <- as.Date(data$host_since)
reference_date <- as.Date("2023-06-05")
data$host_experience <- (reference_date - data$host_since) / 365
data <- na.omit(data)
head(data$host_experience)
table(data$host_experience)

#e
data$entire_apt <- ifelse(data$room_type == "Entire home/apt", 1, 0)
data <- na.omit(data)
head(data$entire_apt)
table(data$entire_apt)

#f
data$host_is_superhost <- ifelse(
  data$host_response_rate >= 90 & 
    data$number_of_reviews >= 10 & 
    data$review_scores_rating >= 4.8, 
  1,0 )
data <- na.omit(data)
head(data$host_is_superhost)

#g
data.id.sort <- data[order(data$id),]
head(data.id.sort)


#2
#a
set.seed(0)

#b
test.data <- sample(nrow(data), size = floor(0.5 * nrow(data)))
test_data <- data[test.data, ]        
train_data <- data[-test.data, ]

nrow(test_data)
nrow(train_data)

#c
linear.model <- lm(price ~ accommodates + beds + host_experience + host_is_superhost +
                     entire_apt + number_of_reviews + review_scores_rating, data = train_data)
summary(linear.model)

predictions <- predict(linear.model, newdata = test_data)
MSE <- mean(test_data$price - predictions)^2
table(MSE)

#d
formula <- price ~ poly(accommodates, 2) + poly(beds, 2) + poly(host_experience, 2) +
  poly(number_of_reviews, 2) + poly(review_scores_rating, 2) +
  accommodates * beds * host_experience * number_of_reviews * review_scores_rating +
  host_is_superhost + entire_apt

model.second <- lm(formula, data = train_data)
summary(model.second)

predictions.second <- predict(model.second, newdata = test_data)
MSE.second <- mean((test_data$price - predictions.second)^2)

#e
full.model <- lm(price ~ poly(accommodates, 2) + poly(beds, 2) + 
                   poly(host_experience, 2) + poly(number_of_reviews, 2) +
                   poly(review_scores_rating, 2) + 
                   accommodates:beds + accommodates:host_experience + 
                   accommodates:number_of_reviews + accommodates:review_scores_rating +
                   beds:host_experience + beds:number_of_reviews + 
                   beds:review_scores_rating + host_experience:number_of_reviews + 
                   host_experience:review_scores_rating + number_of_reviews:review_scores_rating +
                   host_is_superhost + entire_apt, data = train_data)

BIC_model <- step(full.model, direction = "backward", k = log(nrow(train_data)))

summary(BIC_model)$r.squared

test.predictions <- predict(BIC_model, newdata = test_data)
test.mse <- mean((test_data$price - test.predictions)^2)

table(test.mse)

#f
install.packages("glmnet")
library(glmnet)

x.train <- model.matrix(price ~ accommodates + beds + host_experience + 
                          host_is_superhost + entire_apt + 
                          number_of_reviews + review_scores_rating, 
                        data = train_data)
y.train <- train_data$price

x.test <- model.matrix(price ~ accommodates + beds + host_experience + 
                         host_is_superhost + entire_apt + 
                         number_of_reviews + review_scores_rating, 
                       data = test_data)
y.test <- test_data$price


install.packages("glmnet")
library(glmnet)

x.train <- model.matrix(price ~ accommodates + beds + host_experience + 
                          host_is_superhost + entire_apt + 
                          number_of_reviews + review_scores_rating, 
                        data = train_data)

y.train <- train_data$price

x.test <- model.matrix(price ~ accommodates + beds + host_experience + 
                         host_is_superhost + entire_apt + 
                         number_of_reviews + review_scores_rating, 
                       data = test_data)

y.test <- test_data$price

ridge_mse <- c()
lasso_mse <- c()


for (lambda in c(0, 5, 10)) {
  
  # Ridge Regression 
  ridge_model <- glmnet(x.train, y.train, alpha = 0, lambda = lambda)
  ridge_pred <- predict(ridge_model, newx = x.test)
  ridge_mse <- c(ridge_mse, mean((y.test - ridge_pred)^2))
  
  # Lasso Regression 
  lasso_model <- glmnet(x.train, y.train, alpha = 1, lambda = lambda)
  lasso_pred <- predict(lasso_model, newx = x.test)
  lasso_mse <- c(lasso_mse, mean((y.test - lasso_pred)^2))
}


print(ridge_mse)
print(lasso_mse)


results <- data.frame(
  Model = rep(c("Ridge", "Lasso"), each = 3),
  Lambda = rep(c(0, 5, 10), 2),
  MSE = c(ridge_mse, lasso_mse)
)

print(results)
table(results)


#g
cv_lasso <- cv.glmnet(x.train, y.train, alpha = 1, nfolds = 10)
plot(cv_lasso)

best_lambda_lasso <- cv_lasso$lambda.min
print(paste("Best Lambda for Lasso:", best_lambda_lasso))

lasso_pred <- predict(cv_lasso, s = best_lambda_lasso, newx = x.test)

lasso_mse <- mean((y.test - lasso_pred)^2)
print(paste("Lasso MSE (CV):", lasso_mse))

cv_ridge <- cv.glmnet(x.train, y.train, alpha = 0, nfolds = 10)
plot(cv_ridge)

best_lambda_ridge <- cv_ridge$lambda.min
print(paste("Best Lambda for Ridge:", best_lambda_ridge))

ridge_pred <- predict(cv_ridge, s = best_lambda_ridge, newx = x.test)

ridge_mse_cv <- mean((y.test - ridge_pred)^2)
print(paste("Ridge MSE (CV):", ridge_mse_cv))




#3
#a
install.packages("readxl")
library(readxl)

airbnb.cleaned <- data[!is.na(data$accommodates) & 
                         !is.na(data$beds) & 
                         !is.na(data$number_of_reviews) & 
                         !is.na(data$review_scores_rating), ]


airbnb.cleaned$host_since <- as.Date(airbnb.cleaned$host_since)
reference_date <- as.Date("2023-06-05")
airbnb.cleaned$host_experience <- (reference_date - airbnb.cleaned$host_since) / 365

airbnb.cleaned$entire_apt <- ifelse(airbnb.cleaned$room_type == "Entire home/apt", 1, 0)

airbnb.cleaned$host_is_superhost <- ifelse(
  airbnb.cleaned$host_response_rate >= 90 & 
    airbnb.cleaned$number_of_reviews >= 10 & 
    airbnb.cleaned$review_scores_rating >= 4.8, 
  1, 0
)

set.seed(0)  
airbnb.cleaned$noise1 <- airbnb.cleaned$host_experience + rnorm(nrow(airbnb.cleaned), 0, 0.01)
airbnb.cleaned$noise2 <- airbnb.cleaned$host_is_superhost + rnorm(nrow(airbnb.cleaned), 0, 0.01)
airbnb.cleaned$noise3 <- airbnb.cleaned$number_of_reviews + rnorm(nrow(airbnb.cleaned), 0, 0.01)

airbnb.cleaned <- airbnb.cleaned[order(airbnb.cleaned$id), ]

#split data 
set.seed(0)
test.data <- sample(nrow(airbnb.cleaned), size = floor(0.5 * nrow(airbnb.cleaned)))
train_data <- airbnb.cleaned[-test.data, ]
test_data <- airbnb.cleaned[test.data, ]

linear.model <- lm(price ~ accommodates + beds + host_experience + 
                     host_is_superhost + entire_apt + 
                     number_of_reviews + review_scores_rating + 
                     noise1 + noise2 + noise3, 
                   data = train_data)

summary(linear.model)
plot(linear.model)

#b
set.seed(0)
test.data <- sample(nrow(airbnb.cleaned), size = floor(0.9 * nrow(airbnb.cleaned)))
train_data <- airbnb.cleaned[-test.data, ]
test_data <- airbnb.cleaned[test.data, ]

cat("Training data size (90% test split):", nrow(train_data), "\n")
cat("Test data size (90% test split):", nrow(test_data), "\n")

#Aloocation of 98%
set.seed(0)
test.data <- sample(nrow(airbnb.cleaned), size = floor(0.98 * nrow(airbnb.cleaned)))
train_data <- airbnb.cleaned[-test.data, ]
test_data <- airbnb.cleaned[test.data, ]

cat("Training data size (98% test split):", nrow(train_data), "\n")
cat("Test data size (98% test split):", nrow(test_data), "\n")


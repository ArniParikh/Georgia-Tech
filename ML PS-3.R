install.packages("readxl")
library(readxl)

getwd()
setwd(("D:/C tr/Desktop"))
data <- read.csv("airbnb_data.csv")

data <- read.csv("airbnb_cleaned.csv")

data$host_identity_verified <- ifelse(data$host_identity_verified == "t", 1, 0)
data <- data %>% drop_na(review_scores_rating, review_scores_accuracy, review_scores_value)

set.seed(0)
data_sample <- data %>% sample_frac(0.1)

outcome.variable <- na.omit(data, cols = "price")


airbnb.cleaned <- data[!is.na(data$accommodates) & 
                         !is.na(data$beds) & 
                         !is.na(data$number_of_reviews) & 
                         !is.na(data$review_scores_accuracy) & 
                         !is.na(data$host_experience) & 
                         !is.na(data$review_scores_rating), ]

print(dim(airbnb.cleaned))  
print(head(airbnb.cleaned))

print(head(airbnb.cleaned$host_experience))


airbnb.cleaned$host_experience <- airbnb.cleaned$number_of_reviews
head(airbnb.cleaned$host_experience)

print(dim(data))
str(data)
colnames(data)
#1 EMPIRICAL PROBLEMS 

#a
airbnb.cleaned$host_identity_verified <- ifelse(airbnb.cleaned$host_identity_verified == 't', 1, 0)
head(airbnb.cleaned$host_identity_verified)
table(airbnb.cleaned$host_identity_verified)

#b
airbnb.cleaned <- airbnb.cleaned[!is.na(airbnb.cleaned$review_scores_rating) & 
                                   !is.na(airbnb.cleaned$review_scores_accuracy) & 
                                   !is.na(airbnb.cleaned$review_scores_value), ]

summary(airbnb.cleaned[, c("review_scores_rating", "review_scores_accuracy", "review_scores_value")])
table(airbnb.cleaned[, c("review_scores_rating", "review_scores_accuracy", "review_scores_value")])


#2 ANALYSIS

#a
set.seed(0)
sample_size <- floor(0.10 * nrow(airbnb.cleaned))
test_indices <- sample(seq_len(nrow(airbnb.cleaned)), size = sample_size)
test_set <- airbnb.cleaned[test_indices, ]
training_set <- airbnb.cleaned[-test_indices, ]

training_set <- airbnb.cleaned[!is.na(airbnb.cleaned$review_scores_rating) & 
                                 !is.na(airbnb.cleaned$review_scores_accuracy) &
                                 !is.na(airbnb.cleaned$host_experience) &
                                 !is.na(airbnb.cleaned$beds) &
                                 !is.na(airbnb.cleaned$review_scores_value), ]

nrow(training_set)  
nrow(test_set)

ncol(training_set)   
ncol(test_set)

plot(training_set$review_scores_rating, training_set$host_is_superhost)
plot(test_set$review_scores_rating, test_set$host_is_superhost)

#b
linear_model <- lm(host_is_superhost ~ review_scores_rating, data = training_set)
summary(linear_model)

#c
logit_model <- glm(host_is_superhost ~ review_scores_rating, data = training_set, family = binomial)
summary(logit_model)

#d
probit_model <- glm(host_is_superhost ~ review_scores_rating, data = training_set, family = binomial(link = "probit"))
summary(probit_model)

#e
linear_coefficients <- coef(linear_model)
logit_coefficients <- coef(logit_model)
probit_coefficients <- coef(probit_model)

coefficients_table <- data.frame(
  Model = c("Linear Probability Model", "Logit Model", "Probit Model"),
  Intercept = c(linear_coefficients[1], logit_coefficients[1], probit_coefficients[1]),
  Review_Scores_Rating = c(linear_coefficients[2], logit_coefficients[2], probit_coefficients[2])
)

print(coefficients_table)

#f
install.packages("e1071")
library(e1071)

cost_values <- 10^(-2:2)

gamma_value <- 0.1

sum(is.na(training_set$review_scores_rating))  
sum(is.na(training_set$host_is_superhost))     

training_set <- na.omit(training_set[, c("review_scores_rating", "host_is_superhost")])

training_set$host_is_superhost <- as.factor(training_set$host_is_superhost)
training_set$review_scores_rating <- as.numeric(training_set$review_scores_rating)

svm_model <- tune.svm(
  host_is_superhost ~ review_scores_rating,  
  data = training_set,                       
  kernel = "radial",                         
  gamma = gamma_value,                       
  cost = cost_values,                        
  tunecontrol = tune.control(cross = 10)     
)

best_cost <- svm_model$best.parameters$cost
print(best_cost)
print(svm_model$best.parameters)
print(svm_model$performances)


#g
gamma_value <- 10

svm_model_gamma_10 <- tune.svm(
  host_is_superhost ~ review_scores_rating,  
  data = training_set,                       
  kernel = "radial",                         
  gamma = gamma_value,                       
  cost = cost_values,                        
  tunecontrol = tune.control(cross = 10)     
)

best_cost <- svm_model$best.parameters$cost
print(best_cost)

#h
install.packages("glmnet")
library(glmnet)

colnames(training_set)
predictors <- data.frame(
  review_scores_rating = training_set$review_scores_rating,
  review_scores_rating_squared = training_set$review_scores_rating^2
)

data_combined <- cbind(predictors, y)
data_clean <- na.omit(data_combined)
predictors_clean <- data_clean[, -ncol(data_clean)]
y_clean <- data_clean[, ncol(data_clean)]
predictors_matrix <- as.matrix(predictors_clean)

y <- training_set$host_is_superhost
lasso_model <- cv.glmnet(predictors_matrix, y_clean, alpha = 1, family = "binomial", nfolds = 10)
best_lambda <- lasso_model$lambda.min
print(best_lambda)

training_set$rating_squared <- training_set$review_scores_rating^2
training_set$experience_squared <- training_set$host_experience^2
training_set$accuracy_squared <- training_set$review_scores_accuracy^2
training_set$beds_squared <- training_set$beds^2
training_set$value_squared <- training_set$review_scores_value^2

training_set_clean <- training_set[!is.na(training_set$review_scores_rating) & 
                                     !is.na(training_set$host_experience) & 
                                     !is.na(training_set$review_scores_accuracy) & 
                                     !is.na(training_set$beds) & 
                                     !is.na(training_set$review_scores_value) & 
                                     !is.na(training_set$host_is_superhost), ]

predictors <- model.matrix(~ review_scores_rating + host_experience + 
                             review_scores_accuracy + beds + 
                             review_scores_value, 
                           data = training_set_clean)

predictors <- data.frame(
  review_scores_rating = training_set$review_scores_rating,
  review_scores_accuracy = training_set$review_scores_accuracy,
  host_experience = training_set$host_experience,
  beds = training_set$beds,
  review_scores_value = training_set$review_scores_value
)

predictors_full <- model.matrix(~ (review_scores_rating +
                                     review_scores_accuracy + beds + 
                                     review_scores_value)^2 + 
                                  I(review_scores_rating^2) + 
                                  I(host_experience^2) + 
                                  I(review_scores_accuracy^2) + 
                                  I(beds^2) + 
                                  I(review_scores_value^2),
                                data = predictors)

training_set$rating_experience <- training_set$review_scores_rating * training_set$host_experience
training_set$rating_accuracy <- training_set$review_scores_rating * training_set$review_scores_accuracy
training_set$rating_beds <- training_set$review_scores_rating * training_set$beds
training_set$rating_value <- training_set$review_scores_rating * training_set$review_scores_value
training_set$experience_accuracy <- training_set$host_experience * training_set$review_scores_accuracy
training_set$experience_beds <- training_set$host_experience * training_set$beds
training_set$experience_value <- training_set$host_experience * training_set$review_scores_value
training_set$accuracy_beds <- training_set$review_scores_accuracy * training_set$beds
training_set$accuracy_value <- training_set$review_scores_accuracy * training_set$review_scores_value
training_set$beds_value <- training_set$beds * training_set$review_scores_value

predictors <- model.matrix(host_is_superhost ~ review_scores_rating + host_experience + 
                             review_scores_accuracy + beds + review_scores_value + 
                             rating_squared + experience_squared + accuracy_squared + beds_squared + 
                             value_squared + rating_experience + rating_accuracy + 
                             rating_beds + rating_value + experience_accuracy + 
                             experience_beds + experience_value + accuracy_beds + 
                             accuracy_value + beds_value, data = training_set)

outcome <- as.factor(training_set_clean$host_is_superhost)
outcome <- as.factor(training_set$host_is_superhost)
lasso_model <- cv.glmnet(predictors, outcome, family = "binomial", alpha = 1, nfolds = 10)
lasso_model <- cv.glmnet(predictors, outcome, family = "binomial", alpha = 1, nfolds = 10)


optimal_lambda <- lasso_model$lambda.min
print(optimal_lambda)

lasso_coefficients <- coef(lasso_model, s = optimal_lambda)
print(lasso_coefficients)


#i
linear_prob <- predict(linear_model, newdata = test_set)
linear_pred <- ifelse(linear_prob > 0.5, 1, 0)  
linear_error <- mean(linear_pred != test_set$host_is_superhost)

logit_prob <- predict(logit_model, newdata = test_set, type = "response")
logit_pred <- ifelse(logit_prob > 0.5, 1, 0)  
logit_error <- mean(logit_pred != test_set$host_is_superhost)

probit_prob <- predict(probit_model, newdata = test_set, type = "response")
probit_pred <- ifelse(probit_prob > 0.5, 1, 0) 
probit_error <- mean(probit_pred != test_set$host_is_superhost) 

svm_model <- tune(svm, host_is_superhost ~ ., data = training_set, 
                  kernel = "radial", ranges = list(sigma = c(0.1), cost = c(1, 10, 100)))
best_svm_model <- svm_model$best.model
svm_pred_gamma_0.1 <- predict(best_svm_model, newdata = test_set)

svm_pred_gamma_0.1 <- predict(svm_model, newdata = test_set)
svm_error_gamma_0.1 <- mean(svm_pred_gamma_0.1 != test_set$host_is_superhost)

svm_pred_gamma_10 <- predict(svm_model_gamma_10$best.model, newdata = test_set)
svm_error_gamma_10 <- mean(svm_pred_gamma_10 != test_set$host_is_superhost)

lasso_prob <- predict(lasso_model, newx = model.matrix(~ ., test_set)[, -1], s = optimal_lambda, type = "response")
lasso_pred <- ifelse(lasso_prob > 0.5, 1, 0)  
lasso_error <- mean(lasso_pred != test_set$host_is_superhost)


test_set$rating_squared <- test_set$review_scores_rating^2
svm_pred_gamma_0.1 <- predict(best_svm_model, newdata = test_set)

error_table <- data.frame(
  Model = c("Linear Probability Model", "Logit Model", "Probit Model", 
            "SVM (γ = 0.1)", "SVM (γ = 10)", "Lasso Logistic Regression"),
  Mean_Classification_Error = c(
    ifelse(is.null(linear_error), NA, linear_error),
    ifelse(is.null(logit_error), NA, logit_error),
    ifelse(is.null(probit_error), NA, probit_error),
    ifelse(is.null(svm_error_gamma_0.1), NA, svm_error_0.1),
    ifelse(is.null(svm_error_gamma_10), NA, svm_error_gamma_10),
    ifelse(is.null(lasso_error), NA, lasso_error)
  )
)
print(error_table)


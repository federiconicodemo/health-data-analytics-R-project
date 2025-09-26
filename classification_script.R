# ==== Load Required Libraries ----
library(tidyverse)     # data manipulation and plotting
library(caret)         # model training and evaluation
library(class)         # for knn function
library(e1071)         # for naiveBayes
library(nnet)          # for multinomial regression
library(kknn)          # for kernel kNN
library(fastDummies)   # for dummy variables creation
library(ggplot2)       # for advanced plotting
library(leaps)         # best subset selection
library(glmnet)        # LASSO
library(MASS)          # stepwise regression
library(dplyr)
# ==== Load and Prepare Dataset ----
df <- read.csv("cirrhosis_cleaned.csv")

# Convert selected categorical variables to factor
categorical_vars <- c("Drug", "Sex", "Ascites", "Hepatomegaly", "Spiders", "Edema")
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)

# Create dummy variables (remove first to avoid multicollinearity)
cirrhosis_with_dummies <- dummy_cols(
  df,
  select_columns = categorical_vars,
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE
)

# ==== Function Definitions ----

# Train KNN with cross-validation and plot accuracy vs k
run_knn_cv <- function(data, target_var, k_values = c(1, 3, 5, 7, 9, 11, 13)) {
  ctrl <- trainControl(method = "cv", number = 10)
  fit <- train(
    as.formula(paste(target_var, "~ .")),
    data = data,
    method = "knn",
    trControl = ctrl,
    tuneGrid = expand.grid(k = k_values),
    metric = "Accuracy"
  )
  plot(fit, main = paste("Accuracy vs k for", target_var))
  return(fit)
}

# Plot confusion matrix highlighting errors
plot_confusion_matrix_color <- function(pred, actual, title = "Confusion Matrix") {
  conf_mat <- table(Predicted = pred, Actual = actual)
  conf_mat_melt <- as.data.frame(conf_mat)
  actual_levels <- levels(as.factor(actual))
  actual_counts <- table(actual)
  new_x_labels <- paste0(actual_levels, " (n = ", actual_counts[actual_levels], ")")
  names(new_x_labels) <- actual_levels
  conf_mat_melt <- conf_mat_melt %>%
    mutate(is_correct = ifelse(Predicted == Actual, "Correct", "Error"))
  
  ggplot(conf_mat_melt, aes(x = Actual, y = Predicted)) +
    geom_tile(aes(fill = is_correct), color = "white") +
    geom_text(aes(label = Freq), size = 4, family = "mono") +
    scale_fill_manual(
      name = "Prediction Type",
      values = c("Correct" = "#5cb85c", "Error" = "#d9534f"),
      labels = c("Correct Prediction", "Error")
    ) +
    labs(title = title, x = "Actual Class", y = "Predicted Class") +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.line = element_line(color = "black"),
      panel.border = element_rect(fill = NA, color = "gray"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    ) +
    scale_x_discrete(labels = new_x_labels)
}

# ==== KNN for Status_group ----

# Train/Test Split
set.seed(123)
status_index <- createDataPartition(cirrhosis_with_dummies$Status_group, p = 0.8, list = FALSE)
train_status_raw <- cirrhosis_with_dummies[status_index, ]
test_status_raw <- cirrhosis_with_dummies[-status_index, ]

# Feature Scaling
status_features_train <- train_status_raw 
status_features_train$Stage_group <- NULL
status_features_train$Status_group <- NULL
status_features_test <- test_status_raw 
status_features_test$Stage_group <-NULL
status_features_test$Status_group <-NULL
preProc_status <- preProcess(status_features_train, method = c("center", "scale"))
status_train_scaled <- predict(preProc_status, status_features_train)
status_test_scaled <- predict(preProc_status, status_features_test)

# Add Target Variable Back
train_status <- cbind(status_train_scaled, Status_group = train_status_raw$Status_group)
test_status <- cbind(status_test_scaled, Status_group = test_status_raw$Status_group)

# Train Model
cat("\n### KNN Evaluation for target: Status_group\n")
fit_status <- run_knn_cv(train_status, "Status_group")
print(fit_status)

# Evaluate on Test Set
best_k_status <- fit_status$bestTune$k
pred_status <- predict(fit_status, newdata = test_status)
cm_status <- confusionMatrix(as.factor(pred_status), as.factor(test_status$Status_group))
print(cm_status)
plot_confusion_matrix_color(pred_status, test_status$Status_group, "Status Group - Confusion Matrix")

# ==== KNN for Stage_group ----

# Train/Test Split
set.seed(123)
stage_index <- createDataPartition(cirrhosis_with_dummies$Stage_group, p = 0.8, list = FALSE)
train_stage_raw <- cirrhosis_with_dummies[stage_index, ]
test_stage_raw <- cirrhosis_with_dummies[-stage_index, ]

# Feature Scaling
stage_features_train <- train_stage_raw 
stage_features_train$Stage_group <- NULL
stage_features_train$Status_group <- NULL

stage_features_test <- test_stage_raw 
stage_features_test$Stage_group <- NULL
stage_features_test$Status_group <- NULL

preProc_stage <- preProcess(stage_features_train, method = c("center", "scale"))
stage_train_scaled <- predict(preProc_stage, stage_features_train)
stage_test_scaled <- predict(preProc_stage, stage_features_test)

# Add Target Variable Back
train_stage <- cbind(stage_train_scaled, Stage_group = train_stage_raw$Stage_group)
test_stage <- cbind(stage_test_scaled, Stage_group = test_stage_raw$Stage_group)

# Convert Target to Factor
train_stage$Stage_group <- as.factor(train_stage$Stage_group)
test_stage$Stage_group <- as.factor(test_stage$Stage_group)

# Train Model
cat("\n### KNN Evaluation for target: Stage_group\n")
fit_stage <- run_knn_cv(train_stage, "Stage_group")
print(fit_stage)

# Evaluate on Test Set
best_k_stage <- fit_stage$bestTune$k
pred_stage <- predict(fit_stage, newdata = test_stage)
cm_stage <- confusionMatrix(pred_stage, test_stage$Stage_group)
print(cm_stage)
plot_confusion_matrix_color(pred_stage, test_stage$Stage_group, "Stage Group - Confusion Matrix")

# ==== Kernel KNN Evaluation ----

# Define kernels to evaluate
kernel_list <- c("rectangular", "triangular", "epanechnikov", "gaussian", "optimal", 
                 "biweight", "triweight", "cos", "inv", "rank")

# Function to evaluate different kernels
evaluate_kernels <- function(train_data, test_data, target_col, k = 13, kernels) {
  results <- list()
  for (kernel in kernels) {
    cat("\n---- Evaluating kernel:", kernel, "----\n")
    model <- kknn(
      formula = as.formula(paste(target_col, "~ .")),
      train = train_data,
      test = test_data,
      k = k,
      kernel = kernel
    )
    pred <- fitted(model)
    actual <- test_data[[target_col]]
    cm <- confusionMatrix(as.factor(pred), as.factor(actual))
    acc <- cm$overall["Accuracy"]
    results[[kernel]] <- list(
      confusion_matrix = cm,
      accuracy = acc,
      predictions = pred
    )
    print(cm$table)
    cat("Accuracy:", round(acc, 4), "\n")
  }
  return(results)
}

# Run kernel evaluation
kernel_results_stage <- evaluate_kernels(train_stage, test_stage, "Stage_group", k = 13, kernels = kernel_list)

# Plot Accuracy Comparison Across Kernels
accuracy_df <- data.frame(
  Kernel = names(kernel_results_stage),
  Accuracy = sapply(kernel_results_stage, function(x) x$accuracy)
)

ggplot(accuracy_df, aes(x = Kernel, y = Accuracy, fill = Kernel)) +
  geom_bar(stat = "identity") +
  labs(title = "KNN Accuracy by Kernel (Stage_group) with k=13", x = "Kernel Type", y = "Accuracy") +
  theme_minimal() +
  theme(legend.position = "none")

# ==== CV for Kernel KNN ----

cv_kknn_kernel <- function(data, target_col, k_values, kernels, folds = 10) {
  set.seed(123)
  folds_list <- createFolds(data[[target_col]], k = folds, list = TRUE)
  results <- data.frame()
  for (kernel in kernels) {
    for (k in k_values) {
      accuracies <- c()
      for (i in 1:folds) {
        val_idx <- folds_list[[i]]
        train_fold <- data[-val_idx, ]
        val_fold <- data[val_idx, ]
        model <- kknn(
          formula = as.formula(paste(target_col, "~ .")),
          train = train_fold,
          test = val_fold,
          k = k,
          kernel = kernel
        )
        pred <- fitted(model)
        actual <- val_fold[[target_col]]
        cm <- confusionMatrix(as.factor(pred), as.factor(actual))
        accuracies <- c(accuracies, cm$overall["Accuracy"])
      }
      mean_acc <- mean(accuracies)
      results <- rbind(results,
                       data.frame(Kernel = kernel, K = k, Accuracy = mean_acc))
    }
  }
  return(results)
}

# Run CV
k_values <- c(3, 5, 7, 9, 11, 13, 15)
kernel_list <- c("rectangular", "triangular", "epanechnikov", "gaussian", "optimal", 
                 "biweight", "triweight", "cos", "inv", "rank")
cv_results <- cv_kknn_kernel(train_stage, "Stage_group", k_values, kernel_list, folds = 10)

# Best parameters
best_params <- cv_results %>% slice_max(Accuracy, n = 1)

ggplot(cv_results, aes(x = K, y = Accuracy, color = Kernel)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Cross-Validation Accuracy for Different Kernels (k-NN)",
    x = "Number of Neighbors (k)",
    y = "Accuracy",
    color = "Kernel"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )


best_k <- best_params$K
best_kernel <- as.character(best_params$Kernel)
cat("\nBest kernel:", best_kernel, "- Best k:", best_k, "\n")

# Final model
final_model <- kknn(
  formula = Stage_group ~ .,
  train = train_stage,
  test = test_stage,
  k = best_k,
  kernel = best_kernel
)

# Evaluate final model
pred_final <- fitted(final_model)
cm_final <- confusionMatrix(as.factor(pred_final), as.factor(test_stage$Stage_group))
print(cm_final)
plot_confusion_matrix_color(pred_final, test_stage$Stage_group, "Stage Group - Confusion Matrix (kNN with k=13, kernel=gaussian)")

# ==== Naive Bayes Classification ====

run_naive_bayes <- function(data, target_var) {
  index <- createDataPartition(data[[target_var]], p = 0.7, list = FALSE)
  train_data <- data[index, ]
  test_data <- data[-index, ]
  X_train <- train_data[, !names(train_data) %in% c("Status_group", "Stage_group")]
  y_train <- train_data[[target_var]]
  X_test <- test_data[, !names(test_data) %in% c("Status_group", "Stage_group")]
  y_test <- test_data[[target_var]]
  
  model_nb <- naiveBayes(X_train, y_train, laplace = 1)
  y_pred <- predict(model_nb, X_test)
  cat("\n--- Results for target:", target_var, "---\n")
  print(confusionMatrix(as.factor(y_pred), as.factor(y_test)))
  plot_confusion_matrix_color(as.factor(y_pred), as.factor(y_test), "Naive Bayes")
}

run_naive_bayes(cirrhosis_with_dummies, "Status_group")

run_naive_bayes(cirrhosis_with_dummies, "Stage_group")

# ==== Binary Logistic Regression for Status_group ----

# Train/Test Split (80/20)
set.seed(123)
train_index <- createDataPartition(cirrhosis_with_dummies$Status_group, p = 0.8, list = FALSE)
X_train <- cirrhosis_with_dummies[train_index, !(names(cirrhosis_with_dummies) %in% c("Status_group", "Stage_group"))]
X_test <- cirrhosis_with_dummies[-train_index, !(names(cirrhosis_with_dummies) %in% c("Status_group", "Stage_group"))]
y_train <- cirrhosis_with_dummies$Status_group[train_index]
y_test <- cirrhosis_with_dummies$Status_group[-train_index]

y_train <- as.factor(y_train)
y_test <- as.factor(y_test)

# Fit logistic regression model
logit_model <- glm(y_train ~ ., data = X_train, family = binomial())

# Summary of the model
summary(logit_model)

# Predict probabilities on test set
pred_logit <- predict(logit_model, newdata = X_test, type = "response")

# Convert probabilities to class predictions
pred_class <- ifelse(pred_logit > 0.5, "dead", "alive")
pred_class <- factor(pred_class, levels = levels(y_test))

# Confusion matrix
cm_logit <- confusionMatrix(pred_class, y_test)
print(cm_logit)

# Plot confusion matrix
plot_confusion_matrix_color(pred_class, y_test, "Logistic Regression - Status Group")

# ==== Multinomial Logistic Regression for Stage_group ----

# Train/Test Split (80/20)
set.seed(123)
train_index2 <- createDataPartition(cirrhosis_with_dummies$Stage_group, p = 0.8, list = FALSE)
X_train2 <- cirrhosis_with_dummies[train_index2, !(names(cirrhosis_with_dummies) %in% c("Status_group", "Stage_group"))]
X_test2 <- cirrhosis_with_dummies[-train_index2, !(names(cirrhosis_with_dummies) %in% c("Status_group", "Stage_group"))]
y_train2 <- cirrhosis_with_dummies$Stage_group[train_index2]
y_test2 <- cirrhosis_with_dummies$Stage_group[-train_index2]

# Fit multinomial logistic regression model
multinom_model <- multinom(y_train2 ~ ., data = X_train2)

# Summary of the model
summary(multinom_model)

# Make predictions
pred_multinom <- predict(multinom_model, newdata = X_test2)

# Confusion matrix
cm_multinom <- confusionMatrix(as.factor(pred_multinom), as.factor(y_test2))
print(cm_multinom)

# Plot confusion matrix
plot_confusion_matrix_color(pred_multinom, y_test2, "Multinomial Logistic Regression - Stage Group")

# ===== Multinomial Regularized Logistic Regression for Stage_group (with Elastic Net) ====

# Split train/test

train_index <- createDataPartition(cirrhosis_with_dummies$Stage_group, p = 0.7, list = FALSE)
train_index <- as.vector(train_index)

features <- !(names(cirrhosis_with_dummies) %in% c("Status_group", "Stage_group"))

X_train <- as.matrix(cirrhosis_with_dummies[train_index, features])
X_test  <- as.matrix(cirrhosis_with_dummies[-train_index, features])

y_train <- cirrhosis_with_dummies$Stage_group[train_index]
y_test  <- cirrhosis_with_dummies$Stage_group[-train_index]

cv_model <- cv.glmnet(
  x = X_train,
  y = y_train,
  family = "multinomial",
  alpha = 0.5,      # Lasso regularization
  nfolds = 10      # Cross-validation 10-fold
)

plot(cv_model)

# Test set prediction
pred_test <- predict(cv_model, newx = X_test, s = "lambda.1se", type = "class")

pred_test <- as.factor(as.vector(pred_test))
y_test <- as.factor(y_test)

# Confusion matrix
conf_matrix <- confusionMatrix(pred_test, y_test)
print(conf_matrix)

plot_confusion_matrix_color(pred_test, y_test, "Regularized Multinomial Log. Regression")


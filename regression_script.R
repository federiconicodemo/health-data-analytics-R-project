library(ggplot2)
library(ADGofTest)
library(nortest)
library(fitdistrplus)

# ---- import data ----
df = read.csv('air_quality_health_impact_data.csv')

removed_features = c('RecordID')
df <- df[, !(names(df) %in% removed_features)]


# ---- histogram ----

# Istogrammi
hist(df$AQI, main = "Histogram of AQI", xlab = "AQI", col = "lightblue", border = "black")
hist(df$PM10, main = "Histogram of PM10", xlab = "PM10", col = "lightblue", border = "black")
hist(df$PM2_5, main = "Histogram of PM2.5", xlab = "PM2.5", col = "lightblue", border = "black")
hist(df$NO2, main = "Histogram of NO2", xlab = "NO2", col = "lightblue", border = "black")
hist(df$SO2, main = "Histogram of SO2", xlab = "SO2", col = "lightblue", border = "black")
hist(df$O3, main = "Histogram of O3", xlab = "O3", col = "lightblue", border = "black")
hist(df$Temperature, main = "Histogram of Temperature", xlab = "Temperature", col = "lightblue", border = "black")
hist(df$Humidity, main = "Histogram of Humidity", xlab = "Humidity", col = "lightblue", border = "black")
hist(df$WindSpeed, main = "Histogram of WindSpeed", xlab = "WindSpeed", col = "lightblue", border = "black")
hist(df$RespiratoryCases, main = "Histogram of Respiratory Cases", xlab = "RespiratoryCases", col = "lightblue", border = "black")
hist(df$CardiovascularCases, main = "Histogram of Cardiovascular Cases", xlab = "CardiovascularCases", col = "lightblue", border = "black")



ggplot(df, aes(x = HospitalAdmissions)) +
  geom_histogram(binwidth = 1, fill = 'darkviolet', color = 'black') +
  scale_y_log10() +
  labs(
    title = "Hospital admission",
    x = "Hospital admission",
    y = "Frequency log-scale"
  ) +
  theme_minimal()

ggplot(df, aes(x = HealthImpactClass)) +
  geom_histogram(binwidth = 1, fill = 'darkviolet', color = 'black') +
  scale_y_log10() +
  labs(
    title = "Health impact class",
    x = "Health Impact Class",
    y = "Frequency log-scale"
  ) +
  theme_minimal()
ggplot(df, aes(x = HealthImpactScore)) +
  geom_histogram(binwidth = 5, fill = 'darkviolet', color = 'black') +
  scale_y_log10() +
  labs(
    title = "Health impact score",
    x = "Health Impact Score",
    y = "Frequency log-scale"
  ) +
  theme_minimal()

plots <- lapply(names(df[-(12:14)]), function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "darkviolet", color = "black") +
    labs(title = var, x = var, y = "Frequency") +
    theme_minimal()
})
do.call(gridExtra::grid.arrange, plots)

#---- normal distributions ----
library(nortest)

vars <- c("AQI", "PM10", "PM2_5", "NO2", "SO2", "O3",
          "Temperature", "Humidity", "WindSpeed",
          "RespiratoryCases", "CardiovascularCases",
          "HospitalAdmissions", "HealthImpactScore")

results <- data.frame(
  Variable = character(),
  Statistic = numeric(),
  P.Value = numeric(),
  stringsAsFactors = FALSE
)

for (var in vars) {
  test <- nortest::ad.test(df[[var]])
  results <- rbind(results, data.frame(
    Variable = var,
    Statistic = test$statistic,
    P.Value = test$p.value
  ))
}


print(results)
# ---- other distributions ----

# AQI
min_AQI <- min(df$AQI); max_AQI <- max(df$AQI)
test2.AQI <- ks.test(df$AQI, "punif", min = min_AQI, max = max_AQI)# D = 0.0082377, p-value = 0.8252
print(test2.AQI)

# PM10
min_PM10 <- min(df$PM10); max_PM10 <- max(df$PM10)
test2.PM10 <- ks.test(df$PM10, "punif", min = min_PM10, max = max_PM10)# D = 0.015949, p-value = 0.104
print(test2.PM10)

# PM2.5
min_PM2_5 <- min(df$PM2_5); max_PM2_5 <- max(df$PM2_5)
test2.PM2_5 <- ks.test(df$PM2_5, "punif", min = min_PM2_5, max = max_PM2_5)# D = 0.0091111, p-value = 0.7203
print(test2.PM2_5)

# NO2
min_NO2 <- min(df$NO2); max_NO2 <- max(df$NO2)
test2.NO2 <- ks.test(df$NO2, "punif", min = min_NO2, max = max_NO2)# D = 0.021469, p-value = 0.00943
print(test2.NO2)

# SO2
min_SO2 <- min(df$SO2); max_SO2 <- max(df$SO2)
test2.SO2 <- ks.test(df$SO2, "punif", min = min_SO2, max = max_SO2)# D = 0.016795, p-value = 0.0754
print(test2.SO2)

# O3
min_O3 <- min(df$O3); max_O3 <- max(df$O3)
test2.O3 <- ks.test(df$O3, "punif", min = min_O3, max = max_O3)# D = 0.0096148, p-value = 0.6559
print(test2.O3)

# Temperature
min_Temp <- min(df$Temperature); max_Temp <- max(df$Temperature)
test2.Temperature <- ks.test(df$Temperature, "punif", min = min_Temp, max = max_Temp)# D = 0.0066068, p-value = 0.9616
print(test2.Temperature)

# Humidity
min_Hum <- min(df$Humidity); max_Hum <- max(df$Humidity)
test2.Humidity <- ks.test(df$Humidity, "punif", min = min_Hum, max = max_Hum)# D = 0.0081463, p-value = 0.8353
print(test2.Humidity)

# WindSpeed
min_WS <- min(df$WindSpeed); max_WS <- max(df$WindSpeed)
test2.WindSpeed <- ks.test(df$WindSpeed, "punif", min = min_WS, max = max_WS)# D = 0.0095494, p-value = 0.6643
print(test2.WindSpeed)


print(chisq_test)



x <- df$HospitalAdmissions

cat("=== HospitalAdmissions ===\n")
library(fitdistrplus)

# exp
fit_exp <-fitdist(x, "exp") 
ks_exp <- ks.test(x, "pexp", rate = fit_exp$estimate) # D = 0.26046, p-value < 2.2e-16
cat("KS Test vs Esponenziale:\n")
print(ks_exp)


# HealthImpactScore

y <- df$HealthImpactScore

cat("\n=== HealthImpactScore ===\n")

# exp
fit_exp_y <- fitdistrplus::fitdist(y, "exp")
ks_exp_y <- ks.test(y, "pexp", rate = fit_exp_y$estimate)# D = 0.44389, p-value < 2.2e-16
cat("KS Test vs Esponenziale:\n")
print(ks_exp_y)

#log
y_log <- log10(y)
test.HealthImpactScore_log <- nortest::ad.test(y_log)# A = 1203.1, p-value < 2.2e-16
print(test.HealthImpactScore_log)



#---- box plot ----

var_names <- names(df)
par(mfrow = c(3, 5))
for (var in var_names) {
  boxplot(df[[var]], main = var, ylab = "Value", col = "lightblue", border = "black")
}
par(mfrow = c(1, 1))
# there are too many 100 on HealthImpactScore


df$HealthImpactClass[df$HealthImpactScore == 100] <- -1
df$HealthImpactClass = df$HealthImpactClass +1
sum(df$HealthImpactScore==100)# 4284 values 100
sum(df$HealthImpactClass==0)


class_0 <- df[df$HealthImpactClass == 0, ]
other_classes <- df[df$HealthImpactClass != 0, ]

set.seed(123)
n_to_keep =400


undersampled_class_0 <- class_0[sample(nrow(class_0), n_to_keep), ]

df_balanced <- rbind(undersampled_class_0, other_classes)

table(df$HealthImpactClass)
table(df_balanced$HealthImpactClass)
hist(df_balanced$HealthImpactScore)

removed_features_2 = c('HealthImpactClass')
df_balanced <- df_balanced[, !(names(df) %in% removed_features_2)]
# ---- exploratory analysis ----
library(viridis)
library(corrplot)
cor_matrix= cor(df_balanced, use = "pairwise.complete.obs")
corrplot(cor_matrix,method='color',type='upper',col=inferno(100),addCoef.col = 'white',tl.col = 'navy',tl.srt=45,diag=F,number.cex =0.8,tl.cex=0.8,cl.pos='r',cl.cex = 0.6,title='Correlation Matrix',mar=c(0,0,1,0))

summary(df_balanced)
# ---- univariate analysis ----
predictors <- c("AQI", "PM10", "PM2_5", "NO2", "SO2", "O3", "Temperature", "Humidity", "WindSpeed", "RespiratoryCases", "CardiovascularCases", "HospitalAdmissions")


pearson_test = function(x, y) {
  cor.test(x, y, method = "pearson")
}

results = lapply(predictors, function(var) {
  pearson_test(df[[var]], df$HealthImpactScore)
})

correlations = sapply(results, function(res) res$estimate)
p_values = sapply(results, function(res) res$p.value)

correlation_results = data.frame(
  Variable = predictors,
  Correlation = correlations,
  P_Value = p_values
)

correlation_results = correlation_results[order(abs(correlation_results$Correlation), decreasing = TRUE), ]

print(correlation_results)

#study of health score distribution after reducing 100 values
taus <- c(0.05,0.1, 0.25, 0.5, 0.75, 0.9)
quantiles <- quantile(df_balanced$HealthImpactScore, probs = taus)


dens <- density(df_balanced$HealthImpactScore)

plot(dens, main = "Densità con Quantili", xlab = "HealthImpactScore", ylab = "Densità")


cols <- inferno(length(quantiles))
for(i in seq_along(quantiles)) {
  abline(v = quantiles[i], col = cols[i], lwd = 2)
}


legend("topleft", legend = names(quantiles), col = cols, lty = 1, lwd = 2, title = "Quantili",cex=0.7)
# ---- principal components analysis ----
df_cleaned= df_balanced[,1:9]

pca_result=prcomp(df_cleaned,center= T, scale=T)
library(factoextra)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))
#for this reason no PCA

# ---- linear regression ----
library(caTools)
set.seed(123)
df_balanced$HealthImpactScore= df_balanced$HealthImpactScore/100
split_index <-caTools::sample.split(df_balanced$HealthImpactScore, SplitRatio = 0.7)

train_set <- subset(df_balanced, split_index == TRUE)
test_set <- subset(df_balanced, split_index == FALSE)

cat("Dimensioni del Training Set:", nrow(train_set), "righe\n")
cat("Dimensioni del Test Set:", nrow(test_set), "righe\n")
health_score_formula = HealthImpactScore ~ AQI + PM10 + PM2_5 + NO2 + SO2 + O3 + Temperature + Humidity + WindSpeed

linear_model= lm(health_score_formula, data=train_set,)
summary(linear_model)
health_score_formula_2 = HealthImpactScore ~ AQI + PM10 + PM2_5 + NO2 + SO2 + O3 
linear_model_2= lm(health_score_formula_2,data= train_set)
summary(linear_model_2)
result.linear_model=predict(linear_model, newdata = test_set, interval = "confidence")
result.linear_model_2=predict(linear_model_2, newdata = test_set, interval = "confidence")

plot_data_lm1 <- data.frame(
  Actual = test_set$HealthImpactScore,
  Predicted = result.linear_model[, "fit"],
  Lower_CI = result.linear_model[, "lwr"],
  Upper_CI = result.linear_model[, "upr"]
)
plot_data_lm1$error <- with(plot_data_lm1, Actual - Predicted)
rmse_lm1 <- sqrt(mean((plot_data_lm1$Actual - plot_data_lm1$Predicted)^2))
mae_lm1 <- mean(abs(plot_data_lm1$Actual - plot_data_lm1$Predicted))

ggplot(plot_data_lm1, aes(x = Predicted, y = Actual)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "lightgreen", alpha = 0.9, color = NA) +
  geom_point(aes(color = abs(error)), alpha = 0.8, size = 2) +
  scale_color_gradient(low = "blue", high = "red", name = "absolute error",limits=c(0,0.5)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted (Linear Model 1)",
    subtitle = paste0("RMSE = ", round(rmse_lm1, 4), " | MAE = ", round(mae_lm1, 4)),
    x = "Health Score Predicted",
    y = "Health Score Actual"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1)

plot_data_lm2 <- data.frame(
  Actual = test_set$HealthImpactScore,
  Predicted = result.linear_model_2[, "fit"],
  Lower_CI = result.linear_model_2[, "lwr"],
  Upper_CI = result.linear_model_2[, "upr"]
)
plot_data_lm2$error <- with(plot_data_lm2, Actual - Predicted)
rmse_lm2 <- sqrt(mean((plot_data_lm2$Actual - plot_data_lm2$Predicted)^2))
mae_lm2 <- mean(abs(plot_data_lm2$Actual - plot_data_lm2$Predicted))

ggplot(plot_data_lm2, aes(x = Predicted, y = Actual)) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "lightgreen", alpha = 0.5, color = NA) +
  geom_point(aes(color = abs(error)), alpha = 0.8, size = 2) +
  scale_color_gradient(low = "blue", high = "red", name = "absolute error",limits=c(0,0.5)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted (Linear Model 2)",
    subtitle = paste0("RMSE = ", round(rmse_lm2, 4), " | MAE = ", round(mae_lm2, 4)),
    x = "Health Score Predicted",
    y = "Health Score Actual"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1)

models <- list(
  "linear model" = result.linear_model[, "fit"],
  "linear model only significant varibles" = result.linear_model_2[, "fit"]
)

for (model_name in names(models)) {
  pred <- models[[model_name]]
  actual <- test_set$HealthImpactScore
  rmse <- sqrt(mean((pred - actual)^2))
  mae <- mean(abs(pred - actual))
  

  Metric_lm <- rbind(Metric_lm, data.frame(
    Model = model_name,
    RMSE = rmse,
    MAE = mae,
    stringsAsFactors = FALSE
  ))
}

print(Metric_lm)
# ---- ridge e lasso ----
library(glmnet)
library(ggplot2)
library(gridExtra)

# --- 1. data ---
x_train <- as.matrix(train_set[, 1:9])
y_train <- train_set$HealthImpactScore
x_test <- as.matrix(test_set[, 1:9])
y_test <- test_set$HealthImpactScore

# --- 2. Lasso Cross-Validation ---
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5, standardize = TRUE)
fit_lasso <- cv_lasso$glmnet.fit
pred_lasso <- predict(cv_lasso, newx = x_test)

# --- 3. Ridge Cross-Validation ---
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 5, standardize = TRUE)
fit_ridge <- cv_ridge$glmnet.fit
pred_ridge <- predict(cv_ridge, newx = x_test)

# --- 4. Coefficient Paths - Lasso ---
plot(fit_lasso, xvar = "lambda", label = TRUE)
title("Coefficient Paths - Lasso Regression")

# --- 5. Coefficient Paths - Ridge ---
plot(fit_ridge, xvar = "lambda", label = TRUE)
title("Coefficient Paths - Ridge Regression")

# --- 6. Cross-Validation - Lasso ---
plot(cv_lasso, xvar = "lambda")
title("Cross-Validation Error - Lasso Regression")

# --- 7. Cross-Validation - Ridge ---
plot(cv_ridge, xvar = "lambda")
title("Cross-Validation Error - Ridge Regression")

# --- 8. Actual vs Predicted - Lasso ---
plot_data_lasso <- data.frame(
  Actual = y_test,
  Predicted = as.vector(pred_lasso)
)
plot_data_lasso$error <- plot_data_lasso$Actual - plot_data_lasso$Predicted
rmse_lasso <- sqrt(mean(plot_data_lasso$error^2))
mae_lasso <- mean(abs(plot_data_lasso$error))

p_lasso <- ggplot(plot_data_lasso, aes(x = Predicted, y = Actual)) +
  geom_point(aes(color = abs(error)), alpha = 0.8, size = 2) +
  scale_color_gradient(low = "blue", high = "red", name = "Absolute Error",limits=c(0,0.5)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted (Lasso Regression)",
    subtitle = paste0("RMSE = ", round(rmse_lasso, 4), " | MAE = ", round(mae_lasso, 4)),
    x = "Health Score Predicted",
    y = "Health Score Actual"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  theme(legend.position = "right")

print(p_lasso)

# --- 9. Actual vs Predicted - Ridge ---
plot_data_ridge <- data.frame(
  Actual = y_test,
  Predicted = as.vector(pred_ridge)
)
plot_data_ridge$error <- plot_data_ridge$Actual - plot_data_ridge$Predicted
rmse_ridge <- sqrt(mean(plot_data_ridge$error^2))
mae_ridge <- mean(abs(plot_data_ridge$error))

p_ridge <- ggplot(plot_data_ridge, aes(x = Predicted, y = Actual)) +
  geom_point(aes(color = abs(error)), alpha = 0.8, size = 2) +
  scale_color_gradient(low = "blue", high = "red", name = "Absolute Error",limits=c(0,0.5)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted (Ridge Regression)",
    subtitle = paste0("RMSE = ", round(rmse_ridge, 4), " | MAE = ", round(mae_ridge, 4)),
    x = "Health Score Predicted",
    y = "Health Score Actual"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  theme(legend.position = "right")
print(p_ridge)
print(cv_lasso)
print(cv_ridge)

# --- 10. tables---
Metric_rl <- data.frame(Model = character(), RMSE = numeric(), MAE = numeric(), stringsAsFactors = FALSE)

models <- list(
  "Ridge Regression" = pred_ridge,
  "Lasso Regression" = pred_lasso
)

for (model_name in names(models)) {
  pred <- as.vector(models[[model_name]])
  actual <- y_test
  rmse <- sqrt(mean((pred - actual)^2))
  mae <- mean(abs(pred - actual))
  
  Metric_rl <- rbind(Metric_rl, data.frame(
    Model = model_name,
    RMSE = rmse,
    MAE = mae,
    stringsAsFactors = FALSE
  ))
}

print(Metric_rl)
print(lasso_model)



# ---- quantile regression ----
library(quantreg)

library(tidyr)
taus <- c(0.05,0.1, 0.25, 0.5, 0.75, 0.9)


health_score_formula <- HealthImpactScore ~ AQI + PM10 + PM2_5 + NO2 + SO2 + O3 + Temperature + Humidity + WindSpeed



models <- list()
predictions <- list()

for (tau in taus) {
  cat("Fitting model for tau =", tau, "\n")
  model <- rq(formula = health_score_formula, data = train_set, tau = tau)
  models[[as.character(tau)]] <- model
  
  pred <- predict(model, newdata = test_set)
  predictions[[as.character(tau)]] <- pred
}

plot_data <- data.frame(
  Actual = test_set$HealthImpactScore
)

for (tau in names(predictions)) {
  plot_data[[paste0("Pred_tau_", tau)]] <- predictions[[tau]]
}


plot_data_long <- plot_data %>%
  pivot_longer(
    cols = starts_with("Pred_tau"),
    names_to = "Quantile",
    values_to = "Predicted"
  )

metriche <- plot_data_long %>%
  group_by(Quantile) %>%
  summarise(
    RMSE = sqrt(mean((Predicted - Actual)^2)),
    MAE = mean(abs(Predicted - Actual))
  ) %>%
  mutate(
    label = paste0("RMSE = ", round(RMSE, 3), "\nMAE = ", round(MAE, 3))
  )
set.seed(123)
sampled_data <- plot_data_long %>% 
  sample_frac(0.5)

ggplot(sampled_data, aes(x = Predicted, y = Actual, color = Quantile)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dotted") +
  labs(
    title = "Quantile Regression: Actual vs Predicted",
    subtitle = "Comparison across different quantiles τ",
    x = "Health Score Predicted",
    y = "Health Score Actual",
    color = "Quantile (τ)"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  scale_color_manual(
    values = c(
      "Pred_tau_0.05" = "#FF6B6B",
      "Pred_tau_0.1" = "#FFA500",
      "Pred_tau_0.25" = "#4ECDC4",
      "Pred_tau_0.5" = "#45B7D1",
      "Pred_tau_0.75" = "#9B59B6",
      "Pred_tau_0.9" = "#34495E"
    )
  ) +
  facet_wrap(~ Quantile, ncol = 3) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  annotate(
    "text",
    x = Inf, y = -Inf,
    hjust = 1, vjust = -0.5,
    label = paste0(
      "Metrics per quantile:\n",
      paste(metriche$Quantile, metriche$label, collapse = "\n")
    ),
    size = 3.5
  )
Metric=c()
for (tau in names(predictions)) {
  pred <- predictions[[tau]]
  actual <- test_set$HealthImpactScore
  
  rmse <- sqrt(mean((pred - actual)^2))
  mae <- mean(abs(pred - actual))
  
  Metric <- rbind(Metric, data.frame(
    Quantile = tau,
    RMSE = rmse,
    MAE = mae
  ))
}


print(Metric)


# ---- knn ----
library(FNN)
library(ggplot2)
mse_func <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

K <- 5
n <- nrow(train_set)
folds <- split(sample(1:n), factor(sort(rep(1:K, length = n))))

mse_results <- numeric(50)

for (k in 1:50) {
  fold_errors <- numeric(K)
  for (i in 1:K) {
    test_idx <- folds[[i]]
    train_idx <- setdiff(1:n, test_idx)
    
    X_train <- as.matrix(scale(train_set[train_idx, 1:9]))
    y_train <- train_set[train_idx, "HealthImpactScore"]
    
    X_test <- as.matrix(scale(train_set[test_idx, 1:9]))
    y_test <- train_set[test_idx, "HealthImpactScore"]
    
    knn_model <- knn.reg(train = X_train, y = y_train, test = X_test, k = k)
    predictions <- knn_model$pred
    
    fold_errors[i] <- mse_func(y_test, predictions)
  }
  mse_results[k] <- mean(fold_errors)
}
mse_results
best_k <- which.min(mse_results)

training_x <- scale(train_set[, 1:9])
training_y <- train_set$HealthImpactScore
testing_x <- scale(test_set[, 1:9])
testing_y <- test_set$HealthImpactScore

knn_model <- knn.reg(train = training_x, y = training_y, test = testing_x, k = best_k)
best_k
pred_y_knn <- knn_model$pred

mse_knn <- mse_func(testing_y, pred_y_knn)
rmse <- sqrt(mean((testing_y - pred_y_knn)^2))
mae <- mean(abs(testing_y - pred_y_knn))

plot_data_knn <- data.frame(
  Actual = testing_y,
  Predicted = pred_y_knn
)

plot_data_knn$error <- with(plot_data_knn, Actual - Predicted)

p <- ggplot(plot_data_knn, aes(x = Predicted, y = Actual)) +
  geom_point(aes(color = abs(error)), alpha = 0.8, size = 2) +
  scale_color_gradient(low = "blue", high = "red", name = "absolute error",limits=c(0,0.5)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  geom_smooth(method = "lm", color = "green", fill = "lightgreen", se = TRUE, alpha = 0.3) +
  labs(
    title = paste0("Real vs Predicted KNN K=",best_k),
    subtitle = paste0("RMSE = ", round(rmse, 3), ", MAE = ", round(mae, 3)),
    x = "Health Score Predicted",
    y = "Health Score Real"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  theme(
    legend.position = "right",
    plot.subtitle = element_text(size = 10, face = "italic")
  )

print(p)


# ---- polynomial regression ----
library(ggplot2)
library(caTools)


train_poly <- train_set[,-(10:12)]
test_poly <- test_set[,-(10:12)]


# --- 1. Variables to test ---
variables <- c("AQI", "PM10", "PM2_5", "NO2", "SO2", "O3", "Temperature", "Humidity", "WindSpeed")

# --- 2. Table to store results ---
results <- data.frame(
  Variable = character(),
  RMSE = numeric(),
  MAE = numeric(),
  p_linear = numeric(),    
  p_quadratic = numeric(), 
  stringsAsFactors = FALSE
)

# --- 3. Loop for each variable ---
for (var in variables) {
  # Create dynamic formula: HealthImpactScore ~ x + x²
  formula <- as.formula(paste0("HealthImpactScore ~ ", var, " + I(", var, "^2)"))
  
  model <- lm(formula, data = train_poly)

  pred <- predict(model, newdata = test_poly)

  rmse <- sqrt(mean((test_poly$HealthImpactScore - pred)^2))
  mae <- mean(abs(test_poly$HealthImpactScore - pred))
  
  coeff_summary <- summary(model)$coefficients
  p_linear <- coeff_summary[var, 4]           
  p_quadratic <- coeff_summary[paste0("I(", var, "^2)"), 4] 
  
  results <- rbind(results, data.frame(
    Variable = var,
    RMSE = rmse,
    MAE = mae,
    p_linear = p_linear,
    p_quadratic = p_quadratic,
    stringsAsFactors = FALSE
  ))
}

# --- 4. Sort results by RMSE ---
results <- results[order(results$RMSE), ]
print(results)



terms <- c()

for (i in 1:nrow(results)) {
  var <- results$Variable[i]
  p_lin <- results$p_linear[i]
  p_quad <- results$p_quadratic[i]

  if (p_lin < 0.05) {
    terms <- c(terms, var)
  }
 
  if (p_quad < 0.05) {
    terms <- c(terms, paste0("I(", var, "^2)"))
  }
}

if (length(terms) == 0) {

  formula_final <- HealthImpactScore ~ 1
} else {
  formula_final <- as.formula(paste("HealthImpactScore ~", paste(terms, collapse = " + ")))
}
formula_final

model_final <- lm(formula_final, data = train_poly)

pred_final <- predict(model_final, newdata = test_poly)

rmse_final <- sqrt(mean((test_poly$HealthImpactScore - pred_final)^2))
mae_final <- mean(abs(test_poly$HealthImpactScore - pred_final))

# --- 6.Actual vs Predicted ---
plot_data_final <- data.frame(
  Actual = test_poly$HealthImpactScore,
  Predicted = pred_final
)
plot_data_final$error <- with(plot_data_final, Actual - Predicted)
ggplot(plot_data_final, aes(x = Predicted, y = Actual)) +
  geom_point(aes(color = abs(error)), alpha = 0.8, size = 2) +
  scale_color_gradient(low = "blue", high = "red", name = "absolute error",limits=c(0,0.5)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted (Final Polynomial Model)",
    subtitle = paste0("RMSE = ", round(rmse_final, 4), " | MAE = ", round(mae_final, 4)),
    x = "Predicted",
    y = "Actual"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1)
# ---- interaction linear regression ----

library(leaps)  


train_full <- train_set[,-(10:12)]
test_full <- test_set[,-(10:12)]

model_interaction <- lm(HealthImpactScore ~ .^2, data = train_full)
coef_inter <- summary(model_interaction)$coefficients


features_sign <- rownames(coef_inter)[ coef_inter[,4] < 0.05 ]

features_sign <- setdiff(features_sign, "(Intercept)")


formula_inter <- as.formula(paste("HealthImpactScore ~", paste(features_sign, collapse = " + ")))

model_inter_final <- lm(formula_inter, data = train_full)
summary(model_inter_final)


full_model <- lm(HealthImpactScore ~ .^2, data = train_full)
step_model_bwd <- step(full_model, direction = "backward", trace = T)
summary(step_model_bwd)


null_model <- lm(HealthImpactScore ~ 1, data = train_full)
step_model_fwd <- step(
  object    = null_model,
  scope     = list(lower = null_model, upper = full_model),
  direction = "forward",
  trace     = T
)

summary(step_model_fwd)


step_model_hybrid <- step(
  object    = null_model,
  scope     = list(lower = null_model, upper = full_model),
  direction = "both",
  trace     = T
)

summary(step_model_hybrid)

step_model_bwd_bic <- step(full_model, direction = "backward", trace = T,  k=log(nrow(train_full)))
summary(step_model_bwd_bic)

step_model_fwd_bic <- step(
  object    = null_model,
  scope     = list(lower = null_model, upper = full_model),
  direction = "forward",
  trace     = T,
  k=log(nrow(train_full))
)

summary(step_model_fwd_bic)

step_model_hybrid_bic <- step(
  object    = null_model,
  scope     = list(lower = null_model, upper = full_model),
  direction = "both",
  trace     = T,
  k=log(nrow(train_full))
)

summary(step_model_hybrid_bic)



library(leaps)

X <- model.matrix(HealthImpactScore ~ .^2, data = train_full)[, -1]
y <- train_full$HealthImpactScore

fit_leaps <- regsubsets(
  x      = X,
  y      = y,
  nbest  = 1,
  nvmax  = 20,
  method = "exhaustive"
)

sum_leaps <- summary(fit_leaps)

df_plot <- data.frame(
  nv    = 1:length(sum_leaps$bic),
  Cp    = sum_leaps$cp,
  BIC   = sum_leaps$bic,
  AdjR2 = sum_leaps$adjr2
)

plot(
  df_plot$nv, df_plot$BIC, type = "b", pch = 16,
  xlab = "Number of Selected Variables",
  ylab = "Value of BIC (or Cp, or AdjR2)",
  ylim = c(
    min(df_plot$Cp, df_plot$BIC),
    max(df_plot$Cp, df_plot$BIC)
  ),
  col  = "blue",
  main = "Comparison of BIC and Cp vs Model Size"
)
lines(df_plot$nv, df_plot$Cp, type = "b", pch = 17, col = "red")

par(new = TRUE)
plot(
  df_plot$nv, df_plot$AdjR2, type = "b", pch = 15, col = "darkgreen",
  axes = FALSE, xlab = "", ylab = ""
)



models <- list(
  "StepBackward AIC"   = step_model_bwd,
  "StepForward AIC"    = step_model_fwd,
  "StepBoth AIC"       = step_model_hybrid,
  "StepBackward BIC"   = step_model_bwd_bic,
  "StepForward BIC"    = step_model_fwd_bic,
  "StepBoth BIC"       = step_model_hybrid_bic
)

metrics <- data.frame(
  Model = names(models),
  NumVars = sapply(models, function(m) length(coef(m)) - 1),
  MSE = sapply(models, function(m) {
    preds <- predict(m, newdata = test_full)
    mean((test_full$HealthImpactScore - preds)^2)
  })
)

colors <- ifelse(grepl("AIC", metrics$Model), "red", "blue")
positions <- c(3, 4, 2, 1, 3, 4)

xpad <- (max(metrics$NumVars) - min(metrics$NumVars)) * 0.1
ypad <- (max(metrics$MSE)     - min(metrics$MSE))     * 0.1

plot(
  metrics$NumVars, metrics$MSE,
  xlab = "Number of Variables",
  ylab = "Test MSE",
  main = "Model Complexity vs Test MSE",
  pch     = 16,
  col     = colors,
  xlim    = c(min(metrics$NumVars) - xpad, max(metrics$NumVars) + xpad),
  ylim    = c(min(metrics$MSE)     - ypad,     max(metrics$MSE)     + ypad),
  xpd     = TRUE
)

text(
  metrics$NumVars, metrics$MSE,
  labels = metrics$Model,
  pos    = positions,
  col    = colors,
  cex    = 0.8,
  xpd    = TRUE
)



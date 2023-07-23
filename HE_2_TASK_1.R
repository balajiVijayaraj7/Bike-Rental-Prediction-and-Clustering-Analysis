
set.seed(123)
# Loading of data ####
train_set <- read.csv("/Users/balajivijayaraj/Desktop/Statistical_Learning/HomeExercise_2/New Data/train_160523.csv")
test_set <- read.csv("/Users/balajivijayaraj/Desktop/Statistical_Learning/HomeExercise_2/New Data/test_160523.csv")

# Data Preprocessing ####
full_set <- rbind(train_set[,1:10],test_set)
full_set$weather <- as.factor(full_set$weather)

test_set <- full_set[8709:10886,]
train_set <- cbind(full_set[1:8708,], train_set[,11:13])

levels(train_set$weather)
levels(test_set$weather)

train_set$year <- year(train_set$datetime)
train_set$month <- month(train_set$datetime)
train_set$day <- day(train_set$datetime)
train_set$hour <- hour(train_set$datetime)
train_set$minute <- minute(train_set$datetime)

train_set$season <- as.factor(train_set$season )
train_set$holiday <- as.factor(train_set$holiday )
train_set$workingday <- as.factor(train_set$workingday )
train_set$weather <- as.factor(train_set$weather )
train_set$year <- as.factor(train_set$year )
train_set$month <- as.factor(train_set$month )
train_set$day <- as.factor(train_set$day )
train_set$hour <- as.factor(train_set$hour )
train_set  <- train_set[,-18]
train_set  <- train_set[,-1]
train_set  <- train_set[,-1]
train_set  <- train_set[,-9]
train_set  <- train_set[,-9]

test_set$year <- year(test_set$datetime)
test_set$month <- month(test_set$datetime)
test_set$day <- day(test_set$datetime)
test_set$hour <- hour(test_set$datetime)
test_set$minute <- minute(test_set$datetime)

test_set$season <- as.factor(test_set$season )
test_set$holiday <- as.factor(test_set$holiday )
test_set$workingday <- as.factor(test_set$workingday )
test_set$weather <- as.factor(test_set$weather )
test_set$year <- as.factor(test_set$year )
test_set$month <- as.factor(test_set$month )
test_set$day <- as.factor(test_set$day )
test_set$hour <- as.factor(test_set$hour )
test_set  <- test_set[,-15]
test_set  <- test_set[,-1]
test_set  <- test_set[,-1]
str(train_set)
str(test_set)
# RandomForest ####
set.seed(123)

tune.out <- tune(randomForest, count ~ ., data = train_set,
                 ranges = list(ntree = c(100, 200, 300, 400, 500),
                               mtry = c(3,6,8,10,12)))
#best ntree = 500, best mtry = 10

#Random Forest model with best parameters
rf_model <- randomForest(count ~ ., data = train_set, ntree = 500, mtry = 10, importance = TRUE)

# Predict the target variable for the testing dataset
rf_pred <- predict(rf_model, newdata = test_set)

test_set$rf_pred <- round(rf_pred)

# Check if 'rf_pred' column has any negative values
if (any(test_set$rf_pred < 0)) {
  # Replace negative values with 0
  test_set$rf_pred[test_set$rf_pred < 0] <- 0
}

# SVM with Radial kernel ####

set.seed(123)

tune.out <- tune(svm, count ~., data=train_set, kernel = "radial",
                 ranges = list(cost=c(0.1, 1, 5, 10, 100),
                               gamma = 0.001, 0.01, 0.1, 1, 10))
#best cost = 10, best gamma = 0.1

# Build an SVM model with RBF kernel
svm_rbf <- svm(count ~., data = train_set, kernel = "radial", cost = 10, gamma = 0.1)

# Predict the target variable for the testing dataset
svm_rbf_pred <- predict(svm_rbf, newdata = test_set)

test_set$svm_rbf_pred <- round(svm_rbf_pred)

# Check if 'rf_pred' column has any negative values
if (any(test_set$svm_rbf_pred < 0)) {
  # Replace negative values with 0
  test_set$svm_rbf_pred[test_set$svm_rbf_pred < 0] <- 0
}

# Linear Regression ####

# compute correlation matrix 
cor_matrix <- cor(train_set, use = "complete.obs") #removing NA

# plot correlation matrix
corrplot(cor_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("red", "white", "blue"))(100), 
         cl.pos = "n", addCoef.col = "black", 
         number.cex = 0.7, number.round = 2)
#month, season, temp and a temp are highly correlated, with 0.98 and 0.97
# create the full model
full_model <- lm(count ~. -temp -season, data = train_set)
# perform backward selection
backward_model <- step(full_model, direction = "backward")

# Define the training control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Train the linear regression model with cross-validation
lm.model <- train(count ~  atemp + humidity + windspeed + weather + month + year + hour, data = train_set, method = "lm", trControl = ctrl)

linear_model <- lm(count ~  atemp + humidity + windspeed + weather + month + year + hour, data = train_set)
# Resampling results:
#   RMSE      Rsquared   MAE     
# 101.4012  0.6876693  75.24474

# Predict the target variable for the testing dataset
linear_pred <- predict(linear_model, newdata = test_set)

test_set$linear_pred <- round(linear_pred)
#test_set_original <- test_set_original[,-10]

# Check if 'rf_pred' column has any negative values
if (any(test_set$linear_pred < 0)) {
  # Replace negative values with 0
  test_set$linear_pred[test_set$linear_pred < 0] <- 0
}


# write the data frame to a CSV file ####
predictions <- data.frame(rf_pred = test_set$rf_pred, svm_rbf_pred = test_set$svm_rbf_pred, linear_pred = test_set$linear_pred)
write.csv(predictions, "Predictions_1.csv", row.names = FALSE)








# cat and num dataset ####
columns_to_remove <- c("temp", "atemp", "humidity", "windspeed", "count")
cat_data <- train_set[, !(colnames(train_set) %in% columns_to_remove)]
num_data <- train_set[, !(colnames(train_set) %in% colnames(cat_data))]


# Specify the columns to be one-hot encoded
columns_to_encode <- c("season", "year", "month", "weather")

# Perform one-hot encoding
encoded_data <- dummyVars(~., data = cat_data[, columns_to_encode], fullRank = TRUE)
encoded_data <- as.data.frame(predict(encoded_data, newdata = cat_data[, columns_to_encode]))

# Add columns for the reference levels
reference_levels <- sapply(cat_data[, columns_to_encode], levels)
for (col in columns_to_encode) {
  reference_level <- reference_levels[[col]][1]
  col_name <- paste0(col, ".", reference_level)
  encoded_data[[col_name]] <- ifelse(cat_data[[col]] == reference_level, 1, 0)
}

columns_to_encode_1 <- c("day", "hour")

# Perform one-hot encoding
encoded_data_1 <- dummyVars(~., data = cat_data[, columns_to_encode_1], fullRank = TRUE)
encoded_data_1 <- as.data.frame(predict(encoded_data_1, newdata = cat_data[, columns_to_encode_1]))

# Add columns for the reference levels
reference_levels <- sapply(cat_data[, columns_to_encode_1], levels)
for (col in columns_to_encode_1) {
  reference_level <- reference_levels[[col]][1]
  col_name <- paste0(col, ".", reference_level)
  encoded_data_1[[col_name]] <- ifelse(cat_data[[col]] == reference_level, 1, 0)
}

combined_encoded_data <- cbind(encoded_data, encoded_data_1, holiday = cat_data$holiday, workingday = cat_data$workingday)

combined_full_data <- cbind(combined_encoded_data, num_data)
colnames(combined_full_data)
str(combined_full_data)
combined_full_data$holiday <- as.numeric(combined_full_data$holiday)
combined_full_data$workingday <- as.numeric(combined_full_data$workingday)
str(combined_full_data)

# linear with PCA ####
# Scale the variables
scaled_data <- scale(combined_full_data[, -which(names(combined_full_data) == "count")])

pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Get the principal components
principal_components <- pca_result$x

# Get the standard deviations of the principal components
standard_deviations <- pca_result$sdev

# Get the proportion of variance explained by each principal component
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Calculate the cumulative variance explained
cumulative_variance <- cumsum(variance_explained)

# Print the cumulative variance explained
print(cumulative_variance)

# Select the first 51 principal components
selected_components <- principal_components[, 1:52]

# Create a data frame with the selected components and count variable
pca_data <- data.frame(selected_components, count = combined_full_data$count)

# Define the training control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Train the linear regression model with cross-validation
lm.model <- train(count ~ ., data = pca_data, method = "lm", trControl = ctrl)

# Perform linear regression
lm_model <- lm(count ~ ., data = pca_data)

# Predict the target variable for the testing dataset
linear_pred <- predict(lm_model, newdata = test_set)

test_set_original$linear_pred <- round(linear_pred)
#test_set_original <- test_set_original[,-10]

# Check if 'rf_pred' column has any negative values
if (any(test_set_original$linear_pred < 0)) {
  # Replace negative values with 0
  test_set_original$linear_pred[test_set_original$linear_pred < 0] <- 0
}

# Calculate evaluation metrics
mse <- mean((test_data$count - predictions)^2)
rmse <- sqrt(mse)
r2 <- cor(predictions, test_data$count)^2
mae <- mean(abs(test_data$count - predictions))

# Print the results
cat("MSE: ", mse, "\n")
cat("RMSE: ", rmse, "\n")
cat("R-squared: ", r2, "\n")
cat("MAE: ", mae, "\n")



# Define the training control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Train the linear regression model with cross-validation
lm.model <- train(count ~ . -month -holiday - temp -day, data = train_set, method = "lm", trControl = ctrl)

# Lasso ####
library(caret)
library(glmnet)

# Define the training control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Train the Lasso regression model with cross-validation
lasso_model <- train(count ~ ., data = pca_data, method = "glmnet", trControl = ctrl, 
                     tuneGrid = expand.grid(alpha = 1, lambda = seq(0.01, 1, by = 0.01)))

# Print the results
lasso_model
library(mgcv)

# Define the formula with spline terms and knots
formula <- count ~ s(atemp, 5) + s(humidity, 5) + s(windspeed, 5) + season + holiday + workingday + weather + day + hour

# Define the training control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Train the regression model with spline terms and knots using cross-validation
spline_model <- train(formula, data = train_set, method = "gam", trControl = ctrl)

# Print the model results
spline_model
# GAM ####
# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(nrow(train_set), 0.8 * nrow(train_set))  # 80% for training
train_data <- train_set[train_indices, ]
test_data <- train_set[-train_indices, ]

# Define the formula with cubic splines
formula <- count ~ s(atemp, k = 10, bs = "cr") +
  s(humidity, k = 10, bs = "cr") +
  s(windspeed, k = 10, bs = "cr") +
  month  + workingday + weather + hour


# Fit the GAM model on the training data
gam_model <- gam(formula, data = train_data)

# Make predictions on the test data
predictions <- predict(gam_model, newdata = test_data)

# Calculate RMSE, MAE, and R-squared
actual <- test_data$count
rmse <- sqrt(mean((actual - predictions)^2))
mae <- mean(abs(actual - predictions))
rsquared <- 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)

# Print the results
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")


results <- data.frame(actual = test_data$count, predicted = predictions)

# Create scatter plot of predicted vs actual values
ggplot(results, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Count", y = "Predicted Count", title = "Random Forest Model")












colnames(predictions)

# Calculate the average difference between Random Forest and SVM with Radial Kernel predictions
rf_svm_avg_diff <- mean(abs(predictions$rf_pred - predictions$svm_rbf_pred))
cat("Average difference between Random Forest and SVM with Radial Kernel predictions:", rf_svm_avg_diff, "\n")

# Calculate the average difference between Random Forest and Linear Regression predictions
rf_linear_avg_diff <- mean(abs(predictions$rf_pred - predictions$linear_pred))
cat("Average difference between Random Forest and Linear Regression predictions:", rf_linear_avg_diff, "\n")

# Calculate the average difference between SVM with Radial Kernel and Linear Regression predictions
svm_linear_avg_diff <- mean(abs(predictions$svm_rbf_pred - predictions$linear_pred))
cat("Average difference between SVM with Radial Kernel and Linear Regression predictions:", svm_linear_avg_diff, "\n")


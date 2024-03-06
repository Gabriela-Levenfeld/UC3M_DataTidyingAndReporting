# Packages ---------------------------------------------------------------------
library(doParallel)
library(ggplot2)
library(plotly)
library(shapr)
library(randomForest)
library(class) # For knn
# For SVM + hyper-parameter tuning
library(e1071)
library(caret)

# Load data --------------------------------------------------------------------
load(file = "qmnist_nist.RData")

# Set up parallel processing
registerDoParallel(cores = detectCores())

train_data <- as.matrix(train_nist$px) # Feature matrix
labels <- as.factor(train_nist$digit) # Target

# Specify the folder path
folder_path <- "precomputed_data"
# Create the folder if it doesn't exist
if (!dir.exists(folder_path)) {
  dir.create(folder_path)
}

# Professor classifier ---------------------------------------------------------
avg_train_images <- sapply(0:9, function(d) {
  colMeans(train_nist$px[train_nist$digit == d, ])
})

av_img_classifier <- function(vec_img) {
  which.min(colMeans((avg_train_images - vec_img)^2)) - 1
}

predictions_avg <- vector("numeric", length = nrow(test_nist))
counter_good_pred <- 0 # contador
for(img_index in 1:nrow(test_nist)){
  current_img <- test_nist$px[img_index,]
  prepared_img <- c(255 * t(current_img))
  predicted_digit <- av_img_classifier(prepared_img)
  predictions_avg[img_index] <- predicted_digit
  
  if (predicted_digit == test_nist$digit[img_index]) {
    counter_good_pred <- counter_good_pred + 1
  }
}
saveRDS(predictions_avg, file="precomputed_data/avg_predictions.rds")

accuracy_avg <- counter_good_pred / nrow(test_nist)
accuracy_avg
saveRDS(accuracy_avg, file="precomputed_data/avg_accuracy.rds")

# KNN --------------------------------------------------------------------------
prediction_knn <- knn(train=train_nist$px,
                      test= test_nist$px, 
                      cl=train_nist$digit,
                      k = 3)
saveRDS(prediction_knn, file="precomputed_data/knn_predictions.rds")

accuracy_knn <- mean(prediction_knn==test_nist$digit) # 0.978293
saveRDS(accuracy_knn, file="precomputed_data/knn_accuracy.rds")


# Random Forest model ----------------------------------------------------------
# Tiempo en ejecutar: 4 mins
# rf_model <- randomForest(x=as.matrix(train_nist$px),
#                          y=as.factor(train_nist$digit),
#                          ntree=100)

# Tiempo en ejecutar: 14 mins
rf_model <- randomForest(x=train_data,
                         y=labels,
                         ntree=100, 
                         do.trace=100, 
                         importance=TRUE,
                         nThreads = detectCores())


saveRDS(rf_model, file="precomputed_data/rf_model.rds")
print(rf_model) # Displays the model summary

# Evaluate the model on test data
test_pred <- predict(rf_model, newdata = as.matrix(test_nist$px)) # No tarda en vd
conf_mat <- table(Predicted = test_pred, Actual = test_nist$digit)
saveRDS(conf_mat, file = "precomputed_data/rf_conf_mat.rds")

accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
print(paste("Accuracy on test set:", accuracy)) # 0.975826344351258
saveRDS(accuracy, file = "precomputed_data/rf_accuracy.rds")

feature_importance <- importance(rf_model)
saveRDS(importance, file = "precomputed_data/rf_importance.rds")

## Plots ------------------------------------------------------------------------
# Reshape the feature importance into a 28x28 data frame for plotting
importance_matrix <- matrix(feature_importance[, "MeanDecreaseGini"], nrow = 28, byrow = TRUE)
feature_grid <- expand.grid(row = 28:1, col = 1:28) # Create a grid for plotting
feature_grid$importance <- as.vector(importance_matrix)

# Plot the heatmap
gg <- ggplot(feature_grid, aes(x = col, y = row, fill = importance)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of pixel importance", x = "Pixel column", y = "Pixel row") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 12),
        title = element_text(size = 14))

# Optional: If you want to use plotly for an interactive plot
ggplotly(gg)







# TODO: Not working
# SVM with non-linear PCA ------------------------------------------------------
# Install and load the kernlab package for Kernel PCA
if (!requireNamespace("kernlab", quietly = TRUE)) install.packages("kernlab")
library(kernlab)

# Assuming you've already loaded your dataset into `train_data` and `test_data`
# Perform Kernel PCA on the training data
kpca_train <- kpca(~., data=as.data.frame(train_nist$px), kernel="rbfdot", features=50)

# Transform both training and test data using the Kernel PCA model
train_data_transformed <- as.matrix(predict(kpca_train, as.data.frame(train_data)))
test_data_transformed <- as.matrix(predict(kpca_train, as.data.frame(test_nist$px)))

# SVM training on the transformed data
svm_model <- svm(x = train_data_transformed, y = labels, kernel = "radial", cost = 1, gamma = 1/50)

# Save the trained SVM model to disk
saveRDS(svm_model, file = "precomputed_data/svm_model.rds")


# Support Vector Machine ------------------------------------------------------
train_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Hyper-parameter tuning
svm_grid <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.01, 0.1, 1))

set.seed(123) # For reproducibility
svm_model <- train(x = train_data, y = labels,
                   method = "svmLinear")
saveRDS(svm_model, file="precomputed_data/svm_model.rds")

# # SVM classifier ---------------------------------------------------------------
# trainControl <- trainControl(method = "cv", number = 3) # Training control
# # Train the SVM model
# svm_model <- train(x = data,
#                    y = labels,
#                    method = "svmLinear")
# 
# # Save the trained SVM model
# saveRDS(svm_model, "svm_model.rds")

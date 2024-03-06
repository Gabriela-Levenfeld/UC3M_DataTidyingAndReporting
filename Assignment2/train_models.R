# Packages ---------------------------------------------------------------------
library(doParallel)
library(ggplot2)
library(plotly)
library(shapr)
library(randomForest)
# For 
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

## Support Vector Machine ------------------------------------------------------
train_control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Hyper-parameter tuning
svm_grid <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.01, 0.1, 1))

set.seed(123) # For reproducibility
svm_model <- train(x = train_data, y = labels,
                   method = "svmRadial",
                   trControl = train_control,
                   tuneGrid = svm_grid,
                   preProcess = "scale", # Scale features
                   metric = "Accuracy")
saveRDS(svm_model, file="precomputed_data/svm_model.rds")


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


saveRDS(rf_model, file="precomputed_data/rf_model.rds") # Save to disk
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

# Plots ------------------------------------------------------------------------
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














trainControl <- trainControl(method = "cv", number = 3) # Training control

# # SVM classifier ---------------------------------------------------------------
# # Train the SVM model
# svm_model <- train(x = data,
#                    y = labels,
#                    method = "svmLinear")
# 
# # Save the trained SVM model
# saveRDS(svm_model, "svm_model.rds")

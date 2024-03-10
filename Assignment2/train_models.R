# Packages ---------------------------------------------------------------------
library(doParallel)
library(ggplot2)
library(plotly)
library(shapr)
library(randomForest)
library(class) # For knn

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
counter_good_pred <- 0
for(img_index in 1:nrow(test_nist)){
  current_img <- test_nist$px[img_index,]
  prepared_img <- c(t(current_img))
  predicted_digit <- av_img_classifier(prepared_img)
  predictions_avg[img_index] <- predicted_digit
  
  if (predicted_digit == test_nist$digit[img_index]) {
    counter_good_pred <- counter_good_pred + 1
  }
}
saveRDS(predictions_avg, file="precomputed_data/avg_predictions.rds")

accuracy_avg <- counter_good_pred / nrow(test_nist)
saveRDS(accuracy_avg, file="precomputed_data/avg_accuracy.rds")

# predictions_avg <- readRDS("precomputed_data/avg_predictions.rds")
conf_mat_avg <- table(Predicted = predictions_avg, Actual = test_nist$digit)
saveRDS(conf_mat_avg, file = "precomputed_data/avg_conf_mat.rds")

# KNN --------------------------------------------------------------------------
# Model is implemented inside the shiny app, these are just for precomputed data
prediction_knn <- knn(train=train_nist$px,
                      test= test_nist$px, 
                      cl=train_nist$digit,
                      k = 3)
saveRDS(prediction_knn, file="precomputed_data/knn_predictions.rds")

accuracy_knn <- mean(prediction_knn==test_nist$digit) # 0.978293
saveRDS(accuracy_knn, file="precomputed_data/knn_accuracy.rds")

conf_mat_knn <- table(Predicted = prediction_knn, Actual = test_nist$digit)
saveRDS(conf_mat_knn, file = "precomputed_data/knn_conf_mat.rds")

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
test_pred <- predict(rf_model, newdata = as.matrix(test_nist$px))
rf_conf_mat <- table(Predicted = test_pred, Actual = test_nist$digit)
saveRDS(rf_conf_mat, file = "precomputed_data/rf_conf_mat.rds")

accuracy <- sum(diag(rf_conf_mat)) / sum(rf_conf_mat)
print(paste("Accuracy on test set:", accuracy)) # 0.975826344351258
saveRDS(accuracy, file = "precomputed_data/rf_accuracy.rds")

feature_importance <- importance(rf_model)
saveRDS(importance, file = "precomputed_data/rf_importance.rds")



# Plot for aboutApp.md ---------------------------------------------------------
library(ggplot2)

ggplot(data.frame(train_nist$digit), aes(x = factor(train_nist$digit))) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.25) +
  labs(title = "Frequency of Each Digit in the Dataset",
       x = "Digit") +
  theme_minimal()

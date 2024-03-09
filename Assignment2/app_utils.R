# Load materials ---------------------------------------------------------------
# Libraries
library(png)
library(caret)
library(class) # For KNN
library(randomForest)

# MNIST data
load(file = "qmnist_nist.RData")

# Classifier Functions ---------------------------------------------------------
# Average Image Classifier
avg_train_images <- sapply(0:9, function(d) {
  colMeans(train_nist$px[train_nist$digit == d, ])
})

av_img_classifier <- function(vec_img) {
  which.min(colMeans((avg_train_images - vec_img)^2)) - 1
}

# K-Nearest Neighbors Classifier
knn_model <- function(img, k = 3) {
  train_knn <- t(apply(train_nist$px, 1, as.numeric))
  prediction <- knn(train_knn, as.numeric(img), train_nist$digit, k = k)
  return(prediction)
}

# Customize functions ----------------------------------------------------------
# Process the uploaded image
process_image <- function(imagePath) {
  uploaded_img <- png::readPNG(imagePath)
  processed_img <- as.numeric(t(uploaded_img)) * 255 
  # Use scale 0-255 and flatten the
  # rotated image so that the layout is comparable to $px
}

# Prediction value using the selected classifier
make_prediction <- function(processed_img, classifierType) {
  switch(classifierType,
         "Average Image" = av_img_classifier(processed_img),
         "K-Nearest Neighbors" = knn_model(processed_img),
         "Random Forest" = {
           rf_model <- readRDS("precomputed_data/rf_model.rds")
           predict(rf_model, newdata = t(processed_img))
         },
         NA # Default or error case
  )
}

# Accuracy of the selected classifier using the test set
get_accuracy <- function (classifierType) {
  switch (classifierType,
          "Average Image" = readRDS("precomputed_data/avg_accuracy.rds"),
          "K-Nearest Neighbors" = readRDS("precomputed_data/knn_accuracy.rds"),
          "Random Forest" = readRDS("precomputed_data/rf_accuracy.rds"),
          NA
  )
}

# Model explanation of the selected classifier
get_model_explanation <- function(classifierType) {
  explanations <- list(
    "Average Image" = "The Average Image classifier compares the uploaded image against average images of digits.",
    "K-Nearest Neighbors" = "K-Nearest Neighbors classifier, commonly known as KNN, looks at the closest training examples in the feature space and uses a majority vote to determine the digit.",
    "Random Forest" = "Random Forest is an ensemble method that uses multiple decision trees to improve prediction accuracy and control overfitting, making it perfect for handling complex image data."
  )
  explanations[[classifierType]]
}

# Notification handling --------------------------------------------------------
handle_notifications <- function(input, prediction){
  # Check if an image file has been uploaded
  if (is.null(input$imageInput)) {
    showNotification("Please upload an image file to make a prediction.",
                     type = "error",
                     duration = 5)
    return()  # Stop further execution
  }
  
  # Check for PNG file format
  ext <- tools::file_ext(input$imageInput$datapath)
  if (ext != "png") {
    showNotification("Only PNG images are supported, please upload a PNG file.",
                     type = "error",
                     duration = NULL) # Display for 5 seconds
    return() # Stop further execution
  }
  
  # If a PNG image is uploaded, proceed to show a waiting notification
  id_notification <- showNotification("Computing prediction, please wait...",
                                      type = "message",
                                      duration = NULL,
                                      closeButton = FALSE)
  prediction() # Make the prediction
  removeNotification(id_notification)
}
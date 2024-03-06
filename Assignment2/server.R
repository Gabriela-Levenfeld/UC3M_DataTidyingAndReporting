# Packages ---------------------------------------------------------------------
library(shiny)
library(png)
library(caret)
library(class)
library(randomForest)

# Load data --------------------------------------------------------------------
load(file = "qmnist_nist.RData")

########## CLASSIFIERS ##########
## Professor classifier --------------------------------------------------------
avg_train_images <- sapply(0:9, function(d) {
  colMeans(train_nist$px[train_nist$digit == d, ])
})

av_img_classifier <- function(vec_img) {
  which.min(colMeans((avg_train_images - vec_img)^2)) - 1
}

## KNN --------------------------------------------------------------------------
knn_model <- function(img, k = 3) {
  train_knn <- t(apply(train_nist$px, 1, as.numeric))
  prediction <- knn(train_knn, as.numeric(img), train_nist$digit, k = k)
  return(prediction)
}

# Common for svm and rf
train_data <- as.matrix(train_nist$px) # Feature matrix
labels <- as.factor(train_nist$digit) # Target

# Helper function to process the uploaded image
process_image <- function(imagePath) {
  uploaded_img <- png::readPNG(imagePath)
  processed_img <- as.numeric(t(uploaded_img)) * 255 
  # Use scale 0-255 and flatten the
  # rotated image so that the layout is comparable to $px
}

# Prediction using the selected classifier
make_prediction <- function(processed_img, classifierType) {
  switch(classifierType,
         "Average Image" = av_img_classifier(processed_img),
         "SVM" = {
           svm_model <- load_svm_model() # Placeholder for actual SVM model loading function
           predict(svm_model, newdata = t(processed_img))
         },
         "KNN" = knn_model(processed_img),
         "RF" = {
           rf_model <- readRDS("precomputed_data/rf_model.rds")
           predict(rf_model, newdata = t(processed_img))
         },
         NA # Default or error case
  )
}

shinyServer(function(input, output) {
  prediction <- reactive({
    file <- input$imageInput
    ext <- tools::file_ext(file$datapath)
    
    req(file)  # Ensure file is uploaded
    validate(need(ext == "png", "Please upload a PNG image."))

    processed_img <- process_image(input$imageInput$datapath)
    classifierType <- input$classifierType  # Get the selected classifier type from UI
    make_prediction(processed_img, classifierType)  # Use the selected classifier
  })
  
  output$digitImage <- renderImage({
    req(input$imageInput)  # Ensure file is uploaded
    
    list(src = input$imageInput$datapath, 
         contentType = 'image/png',
         alt = "Uploaded Image")
  }, deleteFile = FALSE)
  
  output$prediction <- renderText({
    req(input$imageInput)  # Ensure prediction is recalculated for each new upload
    paste("Predicted digit:", prediction()) # Reactive expression
  })
})

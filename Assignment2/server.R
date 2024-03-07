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

## KNN -------------------------------------------------------------------------
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

# Prediction using the selected classifier
make_prediction <- function(processed_img, classifierType) {
  switch(classifierType,
         "Average Image" = av_img_classifier(processed_img),
         "SVM" = {
           svm_model <- load_svm_model() # Placeholder for actual SVM model loading function
           predict(svm_model, newdata = t(processed_img))
         },
         "KNN" = knn_model(processed_img),
         "Random Forest" = {
           rf_model <- readRDS("precomputed_data/rf_model.rds")
           predict(rf_model, newdata = t(processed_img))
         },
         NA # Default or error case
  )
}

get_accuracy <- function (classifierType) {
  switch (classifierType,
    "Average Image" = readRDS("precomputed_data/avg_accuracy.rds"),
    "KNN" = readRDS("precomputed_data/knn_accuracy.rds"),
    "Random Forest" = readRDS("precomputed_data/rf_accuracy.rds"),
    NA
  )
}


# Server -----------------------------------------------------------------------
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
  
  # Load model accuracy
  output$modelAccuracyBox <- renderValueBox({
    accuracy <- get_accuracy(input$classifierType)
    valueBox(
      scales::percent(accuracy, accuracy = 0.01),  # Percentage with two decimals
      "Model Accuracy",  # Title of the value box
      icon = icon("check"),
      color = ifelse(accuracy > 0.9, "green", "red")  # Display color box
    )
  })

  # Display user image
  output$digitImage <- renderImage({
    req(input$imageInput)  # Ensure file is uploaded
    
    list(src = input$imageInput$datapath, 
         contentType = 'image/png',
         alt = "Uploaded Image")
  }, deleteFile = FALSE)
  
  # Predicted value
  output$prediction <- renderText({
    req(input$imageInput)  # Ensure prediction is recalculated for each new upload
    paste("Predicted digit:", prediction())
  })
})

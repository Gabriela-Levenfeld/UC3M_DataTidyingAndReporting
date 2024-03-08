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
         "K-Nearest Neighbors" = knn_model(processed_img),
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
    "K-Nearest Neighbors" = readRDS("precomputed_data/knn_accuracy.rds"),
    "Random Forest" = readRDS("precomputed_data/rf_accuracy.rds"),
    NA
  )
}

get_model_explanation <- function(classifierType) {
  explanations <- list(
    "Average Image" = "The Average Image classifier compares the uploaded image against average images of digits.",
    "K-Nearest Neighbors" = "K-Nearest Neighbors classifier, commonly known as KNN, looks at the closest training examples in the feature space and uses a majority vote to determine the digit.",
    "Random Forest" = "Random Forest is an ensemble method that uses multiple decision trees to improve prediction accuracy and control overfitting, making it perfect for handling complex image data."
  )
  explanations[[classifierType]]
}


# Server -----------------------------------------------------------------------
shinyServer(function(input, output, session) {
  predictionMade <- reactiveVal(FALSE)
  
  prediction <- eventReactive(input$predictButton,{
    file <- input$imageInput
    ext <- tools::file_ext(file$datapath)
    
    req(file)  # Ensure file is uploaded
    validate(need(ext == "png", "Please upload a PNG image."))

    processed_img <- process_image(input$imageInput$datapath)
    classifierType <- input$classifierType  # Get the selected classifier type from UI
    predictionMade(TRUE)
    make_prediction(processed_img, classifierType)  # Use the selected classifier
  })
  
  # Track if a model has been selected and prediction button clicked
  modelSelected <- reactive({
    input$predictButton > 0 && !is.null(input$classifierType) && input$classifierType != ""
  })
  
  # Load model accuracy
  output$modelAccuracyBox <- renderValueBox({
    req(prediction()) # Wait for the prediction to be made
    accuracy <- get_accuracy(input$classifierType)
    valueBox(
      scales::percent(accuracy, accuracy = 0.01),  # Percentage with two decimals
      "Accuracy",
      icon = icon("check"),
      color = ifelse(accuracy > 0.9, "green", "yellow")  # Display color box
    )
  })
  
  output$modelSummaryUI <- renderUI({
    req(predictionMade()) 
    # Check if a classifier has been selected
    explanationSpecificModel <- get_model_explanation(input$classifierType)
    box(
      title = "Model Summary",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      # Use fluidRow to organize the content
      fluidRow(
        column(12, p(tags$strong("Brief description:"))),
        column(12, p(explanationSpecificModel)),
        column(12,
               p(tags$strong("Model accuracy obtained for 30405 new images:")),
               valueBoxOutput("modelAccuracyBox")
              )
        )
    )
  })

  # Display user image
  output$digitImage <- renderImage({
    input$predictButton
    
    isolate({
      req(input$imageInput)  # Ensure file is uploaded
      
      list(src = input$imageInput$datapath, 
           contentType = 'image/png',
           alt = "Uploaded Image",
           width = "100%",
           height = "auto")
    })
  }, deleteFile = FALSE)
  
  # Predicted value
  output$prediction <- renderUI({
    req(input$imageInput)  # Ensure prediction is recalculated for each new upload
    HTML(paste(
      "<div style='text-align: center;'>",
      "<span style='font-size: 24px; font-weight: bold;'>Predicted digit:</span>",
      "<br><span style='font-size: 32px;'>", prediction(), "</span>",
      "</div>"
      ))
  })
  # Reset predictionMade to FALSE when a new file is uploaded or classifier is changed
  observeEvent(input$imageInput, {
    predictionMade(FALSE)
  }, ignoreNULL = TRUE)
  observeEvent(input$classifierType, {
    predictionMade(FALSE)
  }, ignoreNULL = TRUE)
  
})

# Packages ----
library(shiny)
library(png)

# Load data ----
load(file = "qmnist_nist.RData")

# REVIEW: Define classifier globally or inside server function (?)
avg_train_images <- sapply(0:9, function(d) {
  colMeans(train_nist$px[train_nist$digit == d, ])
})

classifier <- function(vec_img) {
  which.min(colMeans((avg_train_images - vec_img)^2)) - 1
}

shinyServer(function(input, output) {
  prediction <- reactive({
    file <- input$imageInput
    ext <- tools::file_ext(file$datapath)
    
    req(file)  # Ensure file is uploaded
    validate(need(ext == "png", "Please upload a PNG image."))
    
    file <- input$imageInput
    uploaded_img <- png::readPNG(file$datapath)
    processed_img <- c(255 * t(uploaded_img))# Use scale 0-255 and flatten the
    # rotated image so that the layout is comparable to $px
    classifier(processed_img)  # Professor classifier
  })
  
  output$digitImage <- renderImage({
    req(input$imageInput)  # Ensure file is uploaded
    
    list(src = input$imageInput$datapath, contentType = 'image/png', alt = "Uploaded Image")
  }, deleteFile = FALSE)
  
  output$prediction <- renderText({
    req(input$imageInput)  # Ensure prediction is recalculated for each new upload
    paste("Predicted digit:", prediction()) # Reactive expression
  })
})
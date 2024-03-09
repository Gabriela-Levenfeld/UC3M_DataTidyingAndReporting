# Load materials ---------------------------------------------------------------
# Packages
library(shiny)

# Load required script
source("app_utils.R")

# Server -----------------------------------------------------------------------
shinyServer(function(input, output, session) {
  predictionMade <- reactiveVal(FALSE) # Tracking if a prediction has been made
  
  prediction <- eventReactive(input$predictButton,{
    file <- input$imageInput
    ext <- tools::file_ext(file$datapath)
    
    req(file) # Ensure file is uploaded
    validate(
      need(input$predictButton > 0, "Please click 'Make Prediction' to start."),
      need(ext == "png", "Error: only PNG images are supported.")
    )
    
    # Ensure the app still on even if it is not a handwritten image
    processed_img <- tryCatch({
      process_image(input$imageInput$datapath)
    }, error = function(e) {
      # Display an error notification if processing fails
      showNotification("Error processing image: Make sure it's a proper PNG image.", type = "error", duration = NULL)
      return(NULL) # Return NULL to halt further execution
    })
    
    # Only when processed_img is not NULL
    if (!is.null(processed_img)) {
      classifierType <- input$classifierType # Get the selected classifier type from UI
      result <- make_prediction(processed_img, classifierType) # Use the selected classifier
      predictionMade(TRUE)
      return(result)
    }
  })
  
  # UI for model accuracy
  output$modelAccuracyBox <- renderValueBox({
    req(prediction()) # Ensure a prediction has been made
    accuracy <- get_accuracy(input$classifierType)
    valueBox(
      scales::percent(accuracy, accuracy = 0.01), # Percentage with two decimals
      "Accuracy",
      icon = icon("check"),
      color = ifelse(accuracy > 0.9, "green", "yellow") # Display color box
    )
  })
  
  # UI model summary box: brief explanation + accuracy of the selected model
  output$modelSummaryUI <- renderUI({
    req(predictionMade()) # Ensure a prediction has been made
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
               p(tags$strong("Model evaluation")),
               p("Accuracy is define as the propotion of correctly predicted digits to the total number of digits images evaluated."),
               p("To compute this metric, it has been used a reserved test set containing 30450 images that the model had not seen during training, ensuring a reliable assessment of its performance."),
               valueBoxOutput("modelAccuracyBox")
              )
        )
    )
  })

  # Display the upload user image
  output$digitImage <- renderImage({
    input$predictButton
    isolate({
      req(input$imageInput) # Ensure file is uploaded
      
      list(src = input$imageInput$datapath, 
           contentType = 'image/png',
           alt = "",
           width = "100%",
           height = "auto")
    })
  }, deleteFile = FALSE)
  
  # Display the predicted value
  output$prediction <- renderUI({
    req(prediction()) # Ensure a prediction is available
    HTML(paste(
      "<div style='text-align: center;'>",
      "<br><span style='font-size: 32px; font-weight: bold;'>", prediction(), "</span>",
      "</div>"
      ))
  })
  
  # Reset predictionMade to FALSE if new file upload or classifier change
  observeEvent(c(input$imageInput, input$classifierType), {
    predictionMade(FALSE)
  }, ignoreInit = TRUE)
  
  # Show notifications based on user actions
  observeEvent(input$predictButton, {
    handle_notifications(input, prediction)
  })
})

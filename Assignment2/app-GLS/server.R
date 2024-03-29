#-------------------------------------------------------------------------------
# server.R
# Server for the Handwritten digit recognition Shiny app
#-------------------------------------------------------------------------------
#
# This script contains the server logic of the Shiny application, it holds the
# reactive components and data processing technique. It dynamically generates UI
# elements based on the user interaction.
#-------------------------------------------------------------------------------

# Load required script
source("app_utils.R")

# Server -----------------------------------------------------------------------
shinyServer(function(input, output, session) {
  # Analyze Digit tab ----------------------------------------------------------
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
      showNotification("Error processing image: Make sure it's a proper PNG image.",
                       type = "error", 
                       duration = NULL)
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
  
  # Model performance tab ------------------------------------------------------
  # Without icon -> No re-used code for analyzeDigit tab
  render_custom_box <- function(id, model_name) {
    output[[id]] <- renderUI({
      accuracy <- get_accuracy(model_name)
      tags$div(class = "custom-value-box",
               style = ifelse(accuracy > 0.9, 
                              "background-color: #008000;", 
                              "background-color: #ffab00;"),
               tags$div(class = "custom-value-box-content",
                        tags$h3(scales::percent(accuracy, accuracy = 0.01), 
                                class = "custom-value-box-number"),
                        tags$h4(model_name, class = "custom-value-box-title")
               )
      )
    })
  }
  
  # Model accuracy outputs
  render_custom_box("accuracyAvgBox", "Average Image")
  render_custom_box("accuracyKnnBox", "K-Nearest Neighbors")
  render_custom_box("accuracyRfBox", "Random Forest")
  
  # Confusion matrix plot
  output$confMatrixPlot <- renderPlot({
    req(input$modelChoice) # Ensure that a model has been selected
    confMatrix <- get_confusion_matrix(input$modelChoice)
    
    # Transform the matrix to long format
    confMatrixLong <- reshape2::melt(as.matrix(confMatrix),
                                     varnames = c("Predicted", "Actual"))
    confMatrixLong$value <- as.numeric(confMatrixLong$value) # Forcing a numeric data type

    class_labels <- as.character(0:9) # For printing digits as a label
    
    # Plot using ggplot2
    ggplot(data = confMatrixLong, aes(x = Actual, y = Predicted, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = value), color = "black", size = 4, vjust = 1) + # Add each value in the cell
      scale_fill_gradient(low = "white", 
                          high = "blue", 
                          limits = c(0, max(confMatrixLong$value, na.rm = TRUE))) + # For blue color gradient
      scale_x_continuous(breaks = seq(0, 9, by = 1), labels = 0:9) +  # Center label digit in axis
      scale_y_continuous(breaks = seq(0, 9, by = 1), labels = 0:9) +  # Center label digit in axis
      theme_minimal() +
      labs(x = 'Actual', y = 'Predicted', fill = 'Count') +
      ggtitle(paste("Confusion matrix:", input$modelChoice))
  })
})

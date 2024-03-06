# Packages ---------------------------------------------------------------------
library(shiny)
library(shinythemes)

fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Handwritten digit recognition"), # App title
  
  tabsetPanel(
    # Tab 1: Default - Explaining the app ----
    tabPanel("Getting Started", 
             h3("About the App"),
             p("This application is designed for recognizing handwritten digits. It uses various machine learning classifiers to predict the digit in an uploaded PNG image."),
             h4("Understanding the challenge"),
             p("Handwritten digit recognition involves the conversion of human handwritten digit images into digital representations. This task might be challenging due to humans imperfections and the wide variability of handwritten forms. As well as some digits might look similar, increasing the difficulty of the task."),
             h4("How to use it?"),
             p("1. Move to the 'Prediction' tab to start."),
             p("2. Choose a classifier from the available options: Average Image, SVM, KNN, and RF."),
             p("3. Upload a PNG image of a handwritten digit."),
             p("4. View the prediction and metrics about the model's accuracy."),
             ),
    # Tab 2: Prediction - Uploading images and making predictions ----
    tabPanel("Prediction", 
             sidebarLayout(
               sidebarPanel(
                 fileInput(inputId = "imageInput", 
                           label = "Choose PNG image",
                           accept = c("image/png")),
                 selectInput(inputId = "classifierType",
                             label = "Choose classifier type",
                             choices = c("Average Image", "SVM", "KNN", "RF"))
               ),
               mainPanel(
                 imageOutput("digitImage"),
                 textOutput("prediction"),
                 h4("Model Metrics"),
                 #textOutput("modelMetrics") # Placeholder for model metrics output
               )
             )
    )
  )
)

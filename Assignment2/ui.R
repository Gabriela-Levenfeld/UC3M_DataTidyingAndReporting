library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Handwritten digit recognition",
                  tags$li(class = "dropdown",
                          tags$a(href = "https://github.com/Gabriela-Levenfeld/UC3M_DataTidyingAndReporting/tree/main/Assignment2",
                                 target = "_blank",
                                 tags$i(class = "fa fa-github"), " Source Code",
                                 style = "color: white; padding-top: 15px; padding-right: 20px;"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Get Started", tabName = "getStarted"),
      menuItem("Prediction", tabName = "prediction")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .shiny-image-output { 
        text-align: center; 
      }
      .shiny-image-output img {
        max-width: 100%; 
        max-height: 400px; /* Adjust based on your preference */
        height: auto; 
        width: auto;
      }
      /* Custom CSS for valueBox to ensure it takes up more width */
      .value-box {
        width: 100%; /* Adjust this value as needed */
        margin-bottom: 15px; /* Add some space below the box */
     }
    "))),
    tabItems(
      tabItem(tabName = "getStarted",
              h3("About the App"),
              p("This application is designed for recognizing handwritten digits. It uses various machine learning classifiers to predict the digit in an uploaded PNG image."),
              h4("Understanding the challenge"),
              p("Handwritten digit recognition involves the conversion of human handwritten digit images into digital representations. This task might be challenging due to humans' imperfections and the wide variability of handwritten forms. As well as some digits might look similar, increasing the difficulty of the task."),
              h4("How to use it?"),
              p("1. Move to the 'Prediction' tab to start."),
              p("2. Choose a classifier from the available options: Average Image, SVM, KNN, and RF."),
              p("3. Upload a PNG image of a handwritten digit."),
              p("4. View the prediction and metrics about the model's accuracy.")),
      tabItem(tabName = "prediction",
              fluidRow(
                column(6,  # Half width for the image display
                       imageOutput("digitImage")
                ),
                column(6,  # Half width for the prediction display
                       verbatimTextOutput("prediction")
                )
              ),
              fluidRow(
                column(3,  # Adjust the width of the sidebar equivalent if needed
                       fileInput("imageInput", "Choose PNG image", accept = c("image/png")),
                       selectInput("classifierType", "Choose classifier type",
                                   choices = c("Average Image", "KNN", "Random Forest"))
                ),
                column(9,  # Adjust the width of the main panel equivalent
                       box(
                         title = "Model Metrics",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 12,  # Here we set the box to take the full width of the column
                         valueBoxOutput("modelAccuracyBox")
                       )
                )
              )
      )
    )
  )
)

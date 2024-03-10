# Packages ---------------------------------------------------------------------
library(shiny)
library(shinydashboard)

# User Interface ---------------------------------------------------------------
dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Handwritten digit recognition",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/Gabriela-Levenfeld/UC3M_DataTidyingAndReporting/tree/main/Assignment2",
        target = "_blank",
        tags$i(class = "fa fa-github"), " Source Code",
        style = "color: white; padding-top: 15px; padding-right: 20px;"
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Get Started", tabName = "getStarted"),
      menuItem("Analyze Digit", tabName = "analyzeDigit"),
      menuItem("Model Performance", tabName = "modelPerformance")
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app_styles.css")),
    tabItems(
      tabItem(tabName = "getStarted",
              fluidRow(
                tags$img(src = "uc3m_logo.svg", height = "50px", style = "float: right;"),
                tags$h4("Data Tidying and Reporting - Task 2", style = "margin-left: 60px; line-height: 50px; font-style: italic;"),
                div(style = "clear: both;")
              ),
              includeMarkdown("www/aboutApp.md")
      ),
      tabItem(
        tabName = "analyzeDigit",
        fluidRow(
          column(
            3,
            fileInput("imageInput", "Choose PNG image", accept = c("image/png")),
            selectInput("classifierType", "Choose classifier type",
              choices = c("Average Image", "K-Nearest Neighbors", "Random Forest")
            ),
            actionButton("predictButton", "Make Prediction")
          ),
          column(
            6,
            box(
              title = "Uploaded image",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              width = NULL,
              align = "center",
              div(class = "custom-image-container", imageOutput("digitImage"))
            )
          ),
          column(
            3,
            box(
              title = "Prediction Result",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              width = NULL,
              align = "center",
              uiOutput("prediction", style = "text-align:center;")
            )
          )
        ),
        fluidRow(
          column(12, uiOutput("modelSummaryUI"))
        )
      ),
      tabItem(tabName = "modelPerformance",
              fluidRow(
                div(class = "well",
                    p(tags$strong("Model evaluation"), style = "text-align: center;"),
                    p("Accuracy is defined as the proportion of correctly predicted digits to the total number of digit images evaluated.", style = "text-align: center;"),
                    p("To compute this metric, it has been used a reserved test set containing 30450 images that the model had not seen during training, ensuring a reliable assessment of its performance.", style = "text-align: center;"),
                    style = "text-align: center;"
                )
              ),
              fluidRow(
                column(width = 12,
                       div(
                         style = "display: flex; justify-content: space-around;", # Flexbx for centering
                         uiOutput("accuracyAvgBox", style = "flex: 1;"), # Each box will take equal space
                         uiOutput("accuracyRfBox", style = "flex: 1;"),
                         uiOutput("accuracyKnnBox", style = "flex: 1;")
                       )
                      )
              ),
              fluidRow(
                selectInput("modelChoice", "Choose model to view confusion matrix:",
                            choices = c("Average Image", "K-Nearest Neighbors", "Random Forest")),
                plotOutput("confMatrixPlot") # Placeholder for the confusion matrix plot
      )
              )
    )
  )
)

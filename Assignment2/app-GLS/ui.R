#-------------------------------------------------------------------------------
# ui.R
# User interface for the Handwritten digit recognition Shiny app
#-------------------------------------------------------------------------------
#
# It includes the creation of the dashboard elements such as headers, sidebars,
# body, and the placement of input and output elements for interaction.
#-------------------------------------------------------------------------------

# User Interface ---------------------------------------------------------------
dashboardPage(
  skin = "blue", # Color for the dashboard appearance
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
    # Initialize CSS for custom functionality
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app_styles.css")),
    # First tab: Get Started ---------------------------------------------------
    tabItems(
      tabItem(tabName = "getStarted",
              fluidRow(
                div(class = "header-container",
                    div(class = "header-titles",
                        tags$h4("Data Tidying and Reporting - Task 2",
                                class = "header-main-title"
                        ),
                        tags$h4("MSc in Statistics for Data Science at UC3M",
                                class = "header-sub-title"
                        )
                    )
                )
              ),
              includeMarkdown("www/aboutApp.md")
      ),
      # Second tab: Analyze Digit ----------------------------------------------
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
      # Third tab: Model Comparison --------------------------------------------
      tabItem(tabName = "modelPerformance",
              fluidRow(
                div(class = "well",
                    p(tags$strong("Model evaluation"), style = "text-align: center;"),
                    p("Accuracy is defined as the proportion of correctly predicted digits to the total number of digit images evaluated.", 
                      style = "text-align: center;"),
                    p("To compute this metric, it has been used a reserved test set containing 30450 images that the model had not seen during training, ensuring a reliable assessment of its performance.", 
                      style = "text-align: center;"),
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
                      ),
                style = "padding-bottom: 20px;" # Add extra space at the bottom
              ),
              fluidRow(
                column(width = 8,
                       plotOutput("confMatrixPlot")
                ),
                column(width = 4,
                       h4("Confusion Matrix", class = "text-center", style = "margin-bottom: 20px;"),
                       p("A confusion matrix is a table that is used to describe the performance of a classification model. Each cell in the table shows the count of predictions for each class combination."),
                       br(),
                       radioButtons("modelChoice", "Choose model:",
                                    choices = c("Average Image",
                                                "K-Nearest Neighbors",
                                                "Random Forest"),
                                    selected = "Average Image"
                       )
                )
              )
            )
    )
  )
)

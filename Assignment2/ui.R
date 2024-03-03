# Packages ----
library(shiny)  # Required to run any Shiny app
library(shinythemes) # flatly, yeti

fluidPage(
  theme = shinytheme("cerulean"),
  # App title ----
  titlePanel("Handwritten digit recognition"),
  
  # Sidebar panel for inputs ----
  sidebarLayout(
    # Input: Select a image ----
    # where users interact with the app
    sidebarPanel(
      fileInput(inputId = "imageInput", 
                label = "Choose PNG Image",
                accept = c("image/png"))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      imageOutput("digitImage"),
      textOutput("prediction")
    )
  )
)

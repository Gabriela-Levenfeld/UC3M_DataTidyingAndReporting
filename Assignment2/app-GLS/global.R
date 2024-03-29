#-------------------------------------------------------------------------------
# global.R
#-------------------------------------------------------------------------------
#
# Script for loading all required packages for the Handwritten Digit Recognition 
# Shiny app
#-------------------------------------------------------------------------------

# ui.R and server.R ------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(reshape2)
library(ggplot2)

# app_utils.R ------------------------------------------------------------------
library(png)
library(class) # For KNN
library(randomForest)
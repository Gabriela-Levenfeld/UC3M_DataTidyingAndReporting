#-------------------------------------------------------------------------------
# setup_packages.R
#-------------------------------------------------------------------------------
#
# Script to ensure all required packages for the Handwritten Digit Recognition 
# Shiny app are installed
#-------------------------------------------------------------------------------

# Before running this app, make sure you have the following packages installed:
# - shiny
# - shinydashboard
# - reshape2
# - ggplot2
# - png
# - class
# - randomForest
# 
# You can install these packages using the install.packages() function in R:
required_packages <- c("shiny", "shinydashboard", "reshape2", "ggplot2", "png", "class", "randomForest")

# Loop through the required packages, installing them if they are not already installed
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Handwritten Digit Recognition Shiny-App

This repository contains a shiny app designed to identify digits from 0 to 9 based on uploaded images. The app is fully containerized with Docker, making it easy to run on any platform that supports Docker. Additionally, instructions for manual installation without using Docker are provided for users not on Linux or without Docker.

## Local deployment

**Prerequisites**

- [Docker](https://www.docker.com/products/docker-desktop) installed on your machine.

- An internet connection to pull the Docker image.

**Installation**

1. Pull the docker image.

   Open a terminal and pull the docker image from Docker Hub:

``` bash
docker pull glevenfeld/digit-recognition-shiny:latest
```

2. Run the docker container.

   Start the app by running the container:

``` bash
docker run --rm -p 3838:3838 glevenfeld/digit-recognition-shiny:latest
```

3. Access the shiny app.

   Open a web browser and navigate to http://localhost:3838. You should now have access to the Handwritten Digit Recognition Shiny-App.
   
## Manual installation

For users not using Docker, you can run the app directly in R or RStudio by following these steps:

**Prerequisites**

- Install the latest version of R from [CRAN](https://cran.r-project.org/).

- [RStudio](https://rstudio.com/products/rstudio/download/) optional but recommended.

**Installation**

1. Download the folder from GitHub.

   The shiny-app is available in the [Gabriela-Levenfeld GitHub repository](https://github.com/Gabriela-Levenfeld/UC3M_DataTidyingAndReporting/tree/main/Assignment2), specifically within the "app-GLS" subfolder. This folder contains several shiny app scripts (global.R, ui.R, server.R), as well as the renv package library for dependency management. Download this folder, and optionally, open "app-GLS.Rproj" to set up the project context already configured.
   
2. Run the shiny app.

   In R or RStudio, set the working directory to the folder containing the app (app-GLS), then execute:
   
``` R
shiny::runApp()
```

## Testing the app

The test_img folder contains 10 photos of digits from 0 to 9 for you to test the app. You can upload these images in the app to see the digit recognition in action.

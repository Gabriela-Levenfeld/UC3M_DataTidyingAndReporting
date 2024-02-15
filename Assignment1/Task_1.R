#########################################
##  TASK 1 - Gabriela Levenfeld Sabau  ##
#########################################

# Functions ------------------------------------------------------------------------

# Prepare data for two specific digits and adapt it to glmnet format
prepare_data <- function(data, digits) {
  ind <- data$digit %in% digits # For filtering 2 digits
  x <- as.matrix(data$px[ind, ])
  y <- ifelse(data$digit[ind] == as.numeric(digits[1]), 1, 0)
  list(x = x, y = y)
}

# Train and evaluate a model
train_and_evaluate <- function(digit_a, digit_b, train_data, test_data) {
  data_train <- prepare_data(train_data, c(digit_a, digit_b))
  data_test <- prepare_data(test_data, c(digit_a, digit_b))
  
  # Perform cross-validation to find the optimal regularization parameter (lambda)
  cv_model <- cv.glmnet(x = data_train$x, y = data_train$y, alpha = 0, family = "binomial",
                        nfolds = 10, standardize = FALSE)
  
  lambda_optimal <- cv_model$lambda.min
  
  # Fit the model with the optimal lambda
  model <- glmnet(x = data_train$x, y = data_train$y, alpha = 0, lambda = lambda_optimal, family = "binomial",
                  standardize = FALSE)
  
  # Predict on test data and evaluate accuracy
  predictions <- predict(model, newx = data_test$x, s = lambda_optimal, type = "response")
  accuracy <- mean((predictions > 0.5) == data_test$y)
  
  list(accuracy = accuracy, lambda = lambda_optimal)
}

# Plot beta coefficients
plot_beta_coef <- function(lambda_optimal, digit_a, digit_b, train_data) {
  data_train <- prepare_data(train_data, c(digit_a, digit_b))
  # Fit the model with the optimal lambda
  model <- glmnet(
    x = data_train$x,
    y = data_train$y, 
    alpha = 0, 
    lambda = lambda_optimal, 
    family = "binomial",
    standardize = FALSE
  )
  # Extract beta coefficients for the optimal lambda, excluding the intercept
  beta_values <- as.vector(coef(model, s = lambda_optimal)[-1])
  df_beta <- data.frame(PixelIndex = 1:length(beta_values), BetaValue = beta_values)
  
  # Plot beta coefficients
  ggplot(df_beta, aes(x = PixelIndex, y = BetaValue)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(
      title = expression(paste("Estimated ", beta, " coefficients")),
      x = "Pixel index", 
      y = expression(paste(beta, " value"))
    )
}

# Display digit image
show_digit <- function(x, col = gray(255:1 / 255), ...) {
  l <- sqrt(length(x))
  image(matrix(as.numeric(x), nrow = l)[, l:1], col = col, ...)
}

# Visualize i-th image -> REMOVED (?)
i <- 10
show_digit(x = train_nist$px[i, ])
train_nist$digit[i]


# Load libraries -------------------------------------------------------------------
library(glmnet)
library(ggplot2)


# Prepare the environment ----------------------------------------------------------
set.seed(42) # Ensures reproducibility
load(file = "qmnist_nist.RData")


# Classification task 4 vs 9 -------------------------------------------------------
# Example of classification task digit_a vs digit_b
digit_a <- 4
digit_b <- 9

result_ab <- train_and_evaluate(digit_a, digit_b, train_nist, test_nist)
plot_beta_coef(result_ab$lambda, digit_a, digit_b, train_nist)

print(paste("Accuracy for", digit_a, "vs", digit_b, "is", result_ab$accuracy))
print(paste("Optimal lambda for", digit_a, "vs", digit_b, "is", result_ab$lambda))


# Time-consuming! (3 mins) -> Solved by setting fixed values
result_49 <- list(accuracy = 0.977986348122867, lambda = 21.9758474587424)
plot_beta_coef(result_49$lambda, digit_a, digit_b, train_nist)

# Optional analysis - Comparing all digit pairs ------------------------------------

digits <- 0:9

# Initialize structures to store results
lambda_optimals <- matrix(0, nrow = 10, ncol = 10, dimnames = list(digits, digits))
accuracy_matrix <- matrix(0, nrow = 10, ncol = 10, dimnames = list(digits, digits))

# Iterate over all unique pairs of digits
for(i in digits) {
  for(j in digits) {
    # Symmetric matrix
    if(j > i) {
      result <- train_and_evaluate(i, j, train_nist, test_nist)
      accuracy_matrix[i+1, j+1] <- result$accuracy
      lambda_optimals[i+1, j+1] <- result$lambda
      print(paste(i, "_", j))
      print(paste("Accuracy: ", accuracy_matrix[i+1, j+1]))
      print(paste("Optimal lambda: ", lambda_optimals[i+1, j+1]))
    }
  }
}

print(lambda_optimals)
print(accuracy_matrix)

# Hasta aquí es el code para sacar los lambdas y accuracy

# Precomputed data (lambda and accuracy)
lambda_precomputed <- c(
  0, 11.72903, 35.984219, 20.63340, 8.438745, 15.188551, 10.398900, 4.786700, 39.540633, 19.647706,
  0, 0, 4.386601, 14.72075, 9.055458, 4.542014, 7.198158, 5.834560, 18.191963, 5.250782,
  0, 0, 0, 49.86109, 30.673030, 15.672550, 13.161608, 21.111556, 41.315761, 6.543830,
  0, 0, 0, 0, 4.527115, 39.707292, 4.517932, 18.694359, 58.154696, 11.250051,
  0, 0, 0, 0, 0, 7.386997, 12.079968, 12.924441, 22.558261, 21.975847,
  0, 0, 0, 0, 0, 0, 23.899922, 4.110943, 54.588054, 21.540560,
  0, 0, 0, 0, 0, 0, 0, 10.752439, 9.257947, 8.774185,
  0, 0, 0, 0, 0, 0, 0, 0, 11.342793, 29.846691,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 23.762220,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

accuracy_precomputed <- c(
  0, 0.9996938, 0.9927043, 0.9947343, 0.9973571, 0.9892808, 0.9933028, 0.9974579, 0.9934598, 0.9963648,
  0, 0, 0.9979857, 0.9972532, 0.9987382, 0.9968699, 0.9992197, 0.9974164, 0.9901593, 0.9977911,
  0, 0, 0, 0.9786718, 0.9897925, 0.9872064, 0.9900728, 0.9932432, 0.9819477, 0.9889521,
  0, 0, 0, 0, 0.9970370, 0.9745047, 0.9980466, 0.9925574, 0.9719733, 0.9909435,
  0, 0, 0, 0, 0, 0.9933834, 0.9946037, 0.9941003, 0.9935854, 0.9779863,
  0, 0, 0, 0, 0, 0, 0.9897527, 0.9967421, 0.9759462, 0.9923077,
  0, 0, 0, 0, 0, 0, 0, 0.9996759, 0.9938251, 0.9979757,
  0, 0, 0, 0, 0, 0, 0, 0, 0.9943219, 0.9798361,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0.9890240,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

# Load precomputed data (lambda optimal and accuracy matrix)
lambda_data <- matrix(lambda_precomputed, nrow = 10, ncol = 10, byrow = TRUE)
rownames(lambda_data) <- colnames(lambda_data) <- as.character(0:9) # Assign row and column names
print(lambda_data)

accuracy_data <- matrix(accuracy_precomputed, nrow = 10, ncol = 10, byrow = TRUE)
rownames(accuracy_data) <- colnames(accuracy_data) <- as.character(0:9)
print(accuracy_data)


# Visualized beta coefficients with a rank heatmap
# TODO: WIP - De aquí hacia abajo comienza el caos

extract_and_aggregate_betas <- function(train_data, lambda_data) {
  beta_aggregate <- matrix(0, nrow = 28*28, ncol = 1) # Images are 28x28 pixels
  colnames(beta_aggregate) <- "Beta"
  
  counter <- 1
  for (i in 0:8) {
    for (j in (i + 1):9) {
      lambda_optimal <- lambda_data[i + 1, j + 1] # Due to index start at 1, not 0
      if (lambda_optimal > 0) { # >0 -> Indicates a computed model
        data_train <- prepare_data(train_data, c(i, j))
        model <- glmnet(
          x = data_train$x,
          y = data_train$y, 
          alpha = 0, 
          lambda = lambda_optimal, 
          family = "binomial",
          standardize = FALSE
        )

        beta_values <- as.vector(coef(model, s = lambda_optimal)[-1]) # Beta extraction
        beta_aggregate <- beta_aggregate + abs(beta_values) # Using absolute values for simplicity
      }
      counter <- counter + 1
    }
  }
  beta_aggregate <- beta_aggregate / counter # Average
}


beta_aggregate <- extract_and_aggregate_betas(train_nist, lambda_data)
beta_ranks <- rank(beta_aggregate[,1], ties.method = "average") # Rank betas; high absolute values get high ranks
beta_rank_matrix <- matrix(beta_ranks, nrow = 28, byrow = TRUE) # Convert ranks to a matrix for visualization

# Visualize using a heatmap
visualize_beta_ranks <- function(beta_rank_matrix) {
  ggplot(melt(beta_rank_matrix), aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue", name = "Rank") +
    labs(title = expression(paste("Average rank of ", beta, " coefficients")),
         x = "Pixel column",
         y = "Pixel row") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

library(reshape2) # For using melt function
visualize_beta_ranks(beta_rank_matrix)


# Extra: Plotting stuff for me -----------------------------------------------------

# 1. Plot beta coefficients
plot_beta_coef(result_ab$lambda, digit_a, digit_b, train_nist)

# Some conclusion:
# This show the magnitude and direction of the coefficients, indicating which 
# pixels contribute more to the classification of digits 4 and 9.


# TODO: Review this function, not working now
# Exploration of pixels which contributed more to the model

# 2. Heatmap of beta coefficients using show_digit function
show_digit(x = beta_values, col = colorRampPalette(c("blue", "white", "red"))(256))

# This will display the beta coefficients in the format of the digit image,
# using a blue-white-red color scheme to indicate the strength and direction of 
# each pixel's influence.

# Conclusion:
# The color gradient from blue (negative influence) through white (neutral) 
# to red (positive influence) will intuitively show the areas of importance 
# across the digit's shape.


# Add References -------------------------------------------------------------------

# Returns the names of all packages loaded in the current R session
knitr::write_bib(.packages(), "references.bib")
# Reference for a specific package
toBibtex(citation("glmnet"))

# TODO: Chapter 7 for documentation of the functions

#########################################
##  TASK 1 - Gabriela Levenfeld Sabau  ##
#########################################

# Functions ------------------------------------------------------------------------

# Prepare data for two specific digits and adapt it to glmnet format
prepare_data <- function(data, digits) {
  ind <- data$digit %in% digits
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
  # CHANGED: Removed lambda plot
  # plot(cv)
  
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
    labs(title = expression(paste("Estimated ", beta, " coefficients")), x = "Pixel index", y = expression(paste(beta, " value")))
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

digit_a <- 4
digit_b <- 9

# Example of training and evaluating
result_ab <- train_and_evaluate(digit_a, digit_b, train_nist, test_nist)
# Plot beta coefficients for the model
plot_beta_coef(result_ab$lambda, digit_a, digit_b, train_nist)

print(paste("Accuracy for", digit_a, "vs", digit_b, "is", result_ab$accuracy))
print(paste("Optimal lambda for", digit_a, "vs", digit_b, "is", result_ab$lambda))


# Time-consuming! (3 mins) -> Solved by setting fixed values
result_49 <- list(accuracy = 0.977986348122867, lambda = 21.9758474587424)


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


# TODO: Visualized beta coefficients with a rank heatmap

lambda_data <- c(
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

accuracy_data <- c(
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

# Download the lambda optimal and accuracy matrix
lambda_optimals_new <- matrix(lambda_data, nrow = 10, ncol = 10, byrow = FALSE)
# Assign row and column names
rownames(lambda_optimals_new) <- colnames(lambda_optimals_new) <- as.character(0:9)
print(lambda_optimals_new)

# Create the accuracy matrix
accuracy_matrix_new <- matrix(accuracy_data, nrow = 10, ncol = 10, byrow = FALSE)
# Assign row and column names
rownames(accuracy_matrix_new) <- colnames(accuracy_matrix_new) <- as.character(0:9)
print(accuracy_matrix_new)


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

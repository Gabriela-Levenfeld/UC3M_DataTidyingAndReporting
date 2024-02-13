#########################################
##  TASK 1 - Gabriela Levenfeld Sabau  ##
#########################################

# Function to filter data for two specific digits and adapt to glmnet format
prepare_data <- function(data, digits) {
  ind <- data$digit %in% digits
  x <- as.matrix(data$px[ind, ])
  y <- ifelse(data$digit[ind] == as.numeric(digits[1]), 1, 0)
  list(x = x, y = y)
}

# Step 1: Sketch provided by the professor -----------------------------------------

# Load data
load(file = "qmnist_nist.RData")

# Visualization helper
show_digit <- function(x, col = gray(255:1 / 255), ...) {
  l <- sqrt(length(x))
  image(matrix(as.numeric(x), nrow = l)[, l:1], col = col, ...)
}

# Visualize i-th image
i <- 10
show_digit(x = train_nist$px[i, ])
train_nist$digit[i]

train_data <- prepare_data(train_nist, c("4", "9"))


# Step 2: Model Training -----------------------------------------------------------

library(glmnet)
set.seed(42)

# Time-consuming! -> 3 mins and Accuracy: 0.977986348122867
cv <- cv.glmnet(x = train_data$x, y = train_data$y, alpha = 0, family = "binomial",
                nfolds = 10, standardize = FALSE)
plot(cv)

# Find optimal lambda
lambda_optimal <- cv$lambda.min
# Solved by setting lambda as a fixed value
lambda_optimal <- 21.97585

# Fit ridge model with optimal lambda
ridge_model <- glmnet(x = train_data$x, y = train_data$y, alpha = 0, lambda = lambda_optimal, family = "binomial",
                   standardize = FALSE)


# Step 3: Plotting beta ------------------------------------------------------------

# REVIEW: Is this the correct way for plotting beta?
# Extract the beta coefficients for the optimal lambda, excluding the intercept
beta_values <- as.vector(coef(ridge_model, s = lambda_optimal)[-1])

# 1. Plot beta coefficients
barplot(beta_values, names.arg = 1:length(beta_values), 
        main = expression(paste("Estimated ", beta, " coefficients")),
        xlab = "Pixel index", ylab = expression(paste(beta, " value")))

# Some conclusion:
# This show the magnitude and direction of the coefficients, indicating which 
# pixels contribute more to the classification of digits 4 and 9.


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


# 3. Analytical exploration of beta coefficients
abs_beta_values <- abs(beta_values) # Absolute values to assess influence
beta_df <- data.frame(pixel_index = 1:length(abs_beta_values), beta_value = beta_values, abs_beta = abs_beta_values)
beta_df_sorted <- beta_df[order(-beta_df$abs_beta), ] # Sort the dataframe
top_n_pixels <- head(beta_df_sorted, 10) # Extract top N pixels (N=10)
print(top_n_pixels)


# Step 4: Model Evaluation ---------------------------------------------------------

test_data <- prepare_data(test_nist, c("4", "9"))

# Make prediction on the test data for 4's and 9's
predictions <- predict(ridge_model, type = "response", s = lambda_optimal, newx = test_data$x)

# Evaluate model accuracy
accuracy <- mean((predictions > 0.5) == test_data$y)
print(paste("Accuracy:", accuracy))


# Step 5: Optional -----------------------------------------------------------------
# TODO: Implement Step 5

# Modified function to return both accuracy and optimal lambda
train_and_evaluate <- function(digit1, digit2, train_data, test_data) {
  data_train <- prepare_data(train_data, c(digit1, digit2))
  data_test <- prepare_data(test_data, c(digit1, digit2))
  
  cv_model <- cv.glmnet(x = data_train$x, y = data_train$y, alpha = 0, family = "binomial",
                        nfolds = 10, standardize = FALSE)
  
  lambda_optimal <- cv_model$lambda.min
  model <- glmnet(x = data_train$x, y = data_train$y, alpha = 0, lambda = lambda_optimal, family = "binomial",
                  standardize = FALSE)
  
  predictions <- predict(model, newx = data_test$x, s = lambda_optimal, type = "response")
  accuracy <- mean((predictions > 0.5) == data_test$y)
  
  return(list(accuracy = accuracy, lambda = lambda_optimal))
}

digits <- 0:9
# Initialize structures to store results
lambda_optimals <- matrix(0, nrow = 10, ncol = 10, dimnames = list(digits, digits))
accuracy_matrix <- matrix(0, nrow = 10, ncol = 10, dimnames = list(digits, digits))
# Iterate over all unique digit pairs
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

# Save lambdas and accuracy list to a file
save(lambda_optimals, file = "precomputed_lambdas.RData")
save(accuracy_matrix, file = "precomputed_accuracy.RData")
# In a future session, you can load the precomputed lambdas
load("precomputed_lambdas.RData")
load("precomputed_accuracy.RData")

# Define the data as a vector (filling in the matrix by columns)
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

# Define the accuracy data as a vector
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
# REVIEW: Check the accuracy matrix is correct

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


# Add References -------------------------------------------------------------------

# Returns the names of all packages loaded in the current R session
knitr::write_bib(.packages(), "references.bib")
# Reference for a specific package
toBibtex(citation("glmnet"))

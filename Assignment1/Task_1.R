#########################################
##  TASK 1 - Gabriela Levenfeld Sabau  ##
#########################################

# Function to filter data for two specific digits and adapt to glmnet format
prepare_data <- function(data, digits) {
  ind <- data$digit %in% digits
  x <- as.matrix(data$px[ind, ])
  y <- ifelse(data$digit[ind] == as.character(digits[1]), 1, 0)
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


# Add References -------------------------------------------------------------------

# Returns the names of all packages loaded in the current R session
knitr::write_bib(.packages(), "references.bib")
# Reference for a specific package
toBibtex(citation("glmnet"))

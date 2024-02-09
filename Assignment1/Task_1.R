#########################################
##  TASK 1 - Gabriela Levenfeld Sabau  ##
#########################################

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

# Filter for 4's and 9's
ind_49 <- train_nist$digit %in% c(4, 9)
x_49 <- train_nist$px[ind_49, ]
y_49 <- train_nist$digit[ind_49]

# Adapt to glmnet desired input
x_49 <- as.matrix(x_49)
y_49 <- ifelse(y_49 == "4", 1, 0)


# Step 2: Model Training -----------------------------------------------------------

# TODO: Time-consuming!
library(glmnet)
set.seed(42)
cv <- cv.glmnet(x = x_49, y = y_49, alpha = 0, family = "binomial",
                nfolds = 10, standardize = FALSE)
plot(cv)


# Find optimal lambda
lambda_optimal <- cv$lambda.min

# Fit ridge model with optimal lambda
ridge_model <- glmnet(x = x_49, y = y_49, alpha = 0, lambda = lambda_optimal, family = "binomial",
                   standardize = FALSE)


# Step 3: Plotting beta ------------------------------------------------------------

# REVIEW: Is this the correct way for plotting beta?
# Extract coefficients
beta <- coef(ridge_model, s = lambda_optimal)

# Plot beta coefficients
barplot(beta[-1], names.arg = 1:length(beta[-1]), 
        main = "Estimated Beta Coefficients", xlab = "Pixel Index", ylab = "Beta Value")

# Some conclusion:
# This show the magnitude and direction of the coefficients, indicating which 
# features (pixels) contribute more to the classification of digits 4 and 9.


# Step 4: Model Evaluation ---------------------------------------------------------

# Filter for 4's and 9's in the test data
ind_test_49 <- test_nist$digit %in% c("4", "9")
x_test_49 <- test_nist$px[ind_test_49, ]
y_test_49 <- test_nist$digit[ind_test_49]

# Adapt to glmnet desired input
x_test_49 <- as.matrix(x_test_49)
y_test_49 <- ifelse(y_test_49 == "4", 1, 0)

# Make prediction on the test data for 4's and 9's
predictions <- predict(ridge_model, type = "response", s = lambda_optimal, newx = x_test_49)

# Evaluate model accuracy
accuracy <- mean((predictions > 0.5) == y_test_49)
print(paste("Accuracy:", accuracy))

# Step 5: Optional -----------------------------------------------------------------
# TODO: Implement Step 5

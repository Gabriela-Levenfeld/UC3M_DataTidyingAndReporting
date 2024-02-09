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

# Time-consuming!
library(glmnet)
set.seed(42)
cv <- cv.glmnet(x = x_49, y = y_49, alpha = 0, family = "binomial",
                nfolds = 10, standardize = FALSE)
plot(cv)

# Step 3: Plotting beta ------------------------------------------------------------
# Step 4: Model Evaluation ---------------------------------------------------------
# Step 5: Optional -----------------------------------------------------------------

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

# Train and evaluate a model, and compute beta coefficients
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
  
  # Extract beta coefficients, excluding the intercept
  beta_values <- as.vector(coef(model, s = lambda_optimal)[-1])
  
  list(accuracy = accuracy, lambda = lambda_optimal, beta_values = beta_values)
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

# Visualize average rank beta coefficients using a heatmap
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
library(reshape2) # For using melt function


# Prepare the environment ----------------------------------------------------------
set.seed(42) # Ensures reproducibility
load(file = "qmnist_nist.RData")


# Classification task 4 vs 9 -------------------------------------------------------
# Example of classification task digit_a vs digit_b
digit_a <- 4
digit_b <- 9

# FIXME: Falta meter result_49 el beta_value
result_ab <- train_and_evaluate(digit_a, digit_b, train_nist, test_nist)
plot_beta_coef(result_ab$lambda, digit_a, digit_b, train_nist)
# TODO: Falta meter el heatmap


print(paste("Accuracy for", digit_a, "vs", digit_b, "is", result_ab$accuracy))
print(paste("Optimal lambda for", digit_a, "vs", digit_b, "is", result_ab$lambda))


# Time-consuming! (3 mins) -> Solved by setting fixed values
# FIXME: Falta meter result_49 el beta_value
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
      beta_aggregate <- beta_aggregate + abs(result$beta_values) # Using absolute values for simplicity
      
      print(paste(i, "_", j))
      print(paste("Accuracy: ", accuracy_matrix[i+1, j+1]))
      print(paste("Optimal lambda: ", lambda_optimals[i+1, j+1]))
      print(paste("Beta aggregate: ", beta_aggregate))
    }
  }
}

# Average beta values
beta_aggregate <- beta_aggregate/(length(digits)*(length(digits)-1)/2)
beta_ranks <- rank(beta_aggregate[,1], ties.method = "average") # Rank betas; high absolute values get high ranks
beta_rank_matrix <- matrix(beta_ranks, nrow = 28, byrow = TRUE) # Reshape for visualization (28x28 pixels)
visualize_beta_ranks(beta_rank_matrix)

print(lambda_optimals)
print(accuracy_matrix)

# Precomputed data: lambda, accuracy, and beta_ranks coefficients
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

beta_ranks <- c(
  37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 120, 200, 205, 126, 37, 37, 37, 37,
  37, 37, 37, 37, 81, 86, 124, 141, 154, 161, 186, 166, 196, 176, 169, 228, 258, 233, 173, 209, 194, 162, 220, 181, 37, 37, 37,
  37, 37, 37, 75, 96, 122, 102, 133, 177, 207, 234, 262, 273, 259, 264, 266, 285, 302, 284, 294, 281, 236, 248, 230, 183, 164, 149, 37,
  37, 37, 37, 103, 109, 137, 202, 212, 245, 275, 272, 296, 324, 362, 393, 417, 480, 451, 425, 462, 414, 392, 354, 311, 265, 211, 167, 37,
  37, 37, 74, 127, 106, 123, 246, 263, 307, 310, 320, 358, 377, 477, 623, 589, 551, 590, 540, 531, 407, 386, 437, 438, 421, 368, 283, 178,
  112, 37, 37, 108, 143, 163, 295, 309, 342, 347, 380, 435, 450, 527, 735, 646, 510, 713, 541, 441, 626, 543, 548, 549, 615, 428, 298, 170,
  37, 37, 95, 182, 219, 247, 314, 335, 371, 434, 479, 431, 422, 454, 501, 561, 635, 688, 568, 486, 493, 560, 426, 572, 586, 553, 340, 226,
  119, 37, 160, 223, 239, 329, 388, 382, 398, 463, 498, 460, 564, 640, 606, 538, 668, 751, 705, 614, 478, 418, 499, 559, 537, 542, 375, 241,
  114, 147, 156, 222, 244, 339, 445, 378, 411, 471, 455, 489, 508, 519, 518, 528, 515, 691, 682, 706, 660, 627, 556, 702, 739, 725, 427, 253,
  129, 117, 180, 213, 271, 350, 390, 403, 402, 529, 512, 476, 583, 554, 484, 631, 598, 557, 701, 722, 711, 569, 653, 730, 771, 741, 448, 256,
  158, 121, 168, 238, 289, 361, 356, 372, 466, 573, 509, 685, 671, 535, 503, 715, 736, 717, 729, 732, 699, 641, 642, 714, 767, 745, 384, 267,
  152, 97, 172, 237, 291, 370, 334, 424, 456, 684, 704, 723, 658, 689, 639, 726, 708, 747, 678, 695, 555, 731, 672, 657, 674, 648, 367, 261,
  148, 37, 130, 232, 282, 364, 363, 458, 497, 621, 697, 690, 765, 753, 770, 759, 775, 780, 680, 628, 743, 571, 655, 603, 577, 399, 321, 277,
  142, 87, 146, 206, 270, 366, 389, 387, 649, 634, 757, 777, 693, 773, 779, 769, 772, 774, 716, 632, 677, 565, 727, 681, 679, 433, 344, 287,
  204, 125, 138, 184, 260, 338, 376, 511, 656, 613, 675, 719, 746, 748, 768, 776, 778, 766, 724, 579, 444, 459, 526, 496, 624, 430, 348, 250,
  115, 105, 37, 155, 280, 360, 394, 491, 585, 694, 738, 633, 718, 764, 783, 784, 707, 692, 710, 546, 483, 629, 470, 396, 495, 406, 345, 257,
  83, 98, 93, 216, 319, 369, 381, 600, 734, 683, 698, 733, 762, 760, 781, 763, 625, 654, 687, 488, 517, 604, 481, 404, 429, 401, 328, 251,
  118, 37, 92, 249, 331, 409, 419, 578, 580, 669, 744, 754, 756, 758, 782, 703, 662, 686, 594, 467, 664, 574, 534, 523, 443, 436, 326, 221,
  179, 128, 91, 214, 353, 473, 544, 651, 596, 740, 750, 712, 666, 721, 638, 584, 570, 636, 452, 532, 619, 487, 494, 506, 423, 385, 317, 197,
  144, 79, 135, 217, 333, 464, 601, 644, 737, 742, 700, 728, 665, 643, 536, 607, 550, 645, 592, 416, 552, 442, 447, 593, 439, 341, 304, 192,
  100, 37, 132, 215, 323, 533, 608, 720, 755, 752, 749, 761, 622, 562, 520, 652, 521, 612, 558, 513, 514, 469, 415, 432, 336, 312, 274, 174,
  84, 89, 101, 210, 288, 400, 605, 696, 602, 566, 647, 659, 673, 610, 591, 547, 567, 595, 524, 516, 492, 472, 405, 374, 305, 292, 231, 195,
  76, 88, 82, 189, 268, 355, 502, 676, 611, 465, 597, 449, 617, 663, 563, 525, 490, 530, 670, 461, 474, 420, 359, 327, 290, 254, 235, 131,
  78, 37, 37, 165, 252, 349, 391, 482, 468, 475, 545, 599, 709, 539, 576, 667, 616, 620, 587, 413, 379, 365, 315, 308, 276, 240, 227, 139,
  37, 37, 37, 151, 225, 313, 408, 582, 650, 581, 500, 637, 609, 618, 661, 485, 457, 453, 440, 383, 351, 297, 278, 269, 242, 224, 198, 110,
  37, 37, 37, 116, 140, 286, 357, 397, 505, 630, 588, 575, 522, 504, 395, 412, 507, 446, 410, 373, 346, 300, 255, 243, 199, 136, 113, 107,
  37, 37, 37, 37, 111, 229, 301, 322, 299, 306, 332, 352, 316, 325, 330, 343, 337, 318, 293, 303, 279, 218, 188, 157, 145, 90, 77, 37,
  37, 37, 37, 37, 37, 80, 94, 134, 185, 187, 153, 203, 190, 201, 191, 193, 159, 171, 175, 208, 150, 104, 99, 85, 37, 37, 37, 37, 37
)

# Load precomputed data: lambda optimal
lambda_data <- matrix(lambda_precomputed, nrow = 10, ncol = 10, byrow = TRUE)
rownames(lambda_data) <- colnames(lambda_data) <- as.character(0:9) # Assign row and column names
print(lambda_data)

# Load precomputed data: accuracy matrix
accuracy_data <- matrix(accuracy_precomputed, nrow = 10, ncol = 10, byrow = TRUE)
rownames(accuracy_data) <- colnames(accuracy_data) <- as.character(0:9)
print(accuracy_data)

# Load precomputed data: beta rank coefficients
beta_rank_matrix <- matrix(beta_ranks, nrow = 28, byrow = TRUE) # Reshape for visualization (28x28 pixels)
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

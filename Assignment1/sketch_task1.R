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
  cv_model <- cv.glmnet(x = data_train$x, y = data_train$y, alpha = 0, family = "binomial", nfolds = 10, standardize = FALSE)
  
  lambda_optimal <- cv_model$lambda.min
  
  # Fit the model with the optimal lambda
  model <- glmnet(
    x = data_train$x,
    y = data_train$y,
    alpha = 0,
    lambda = lambda_optimal, 
    family = "binomial",
    standardize = FALSE
  )
  
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
visualize_beta_ranks <- function(beta_ranks) {
  beta_rank_matrix <- matrix(beta_ranks, nrow = 28, byrow = TRUE) # Reshape for visualization (28x28 pixels)
  
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

result_ab <- train_and_evaluate(digit_a, digit_b, train_nist, test_nist)
sprintf("Accuracy for %d vs %d: %f", digit_a, digit_b, result_ab$accuracy)
sprintf("Optimal lambda for %d vs %d: %f", digit_a, digit_b, result_ab$lambda)

plot_beta_coef(result_ab$lambda, digit_a, digit_b, train_nist)
# Some conclusion:
# This show the magnitude and direction of the coefficients, indicating which 
# pixels contribute more to the classification of digits 4 and 9.

# Heatmap for beta coefficient
beta_absolute <- abs(result_ab$beta_values)
beta_ranks <- rank(beta_absolute, ties.method = "average") # Rank betas; high absolute values get high ranks
visualize_beta_ranks(beta_ranks)


# Time-consuming! (3 mins) -> Solved by setting fixed values
# Precomputed beta_rank_matrix with the provided data
beta_ranks_49 <- c(
  93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93,
  93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93,
  93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 198, 206, 195, 93, 220, 217, 199, 202, 93, 93, 93, 93, 93,
  93, 93, 93, 93, 93, 93, 93, 93, 93, 223, 239, 234, 210, 263, 290, 273, 288, 311, 607, 343, 257, 245, 225, 226, 205, 188, 93, 93,
  93, 93, 93, 93, 203, 232, 219, 214, 227, 244, 241, 259, 385, 389, 475, 615, 516, 414, 536, 433, 408, 363, 298, 323, 342, 297, 209, 93,
  93, 93, 93, 197, 213, 246, 340, 377, 459, 467, 533, 411, 320, 636, 393, 537, 687, 269, 416, 367, 526, 464, 657, 670, 489, 303, 200, 93,
  93, 93, 93, 208, 264, 518, 394, 296, 554, 506, 362, 652, 488, 560, 741, 760, 773, 751, 749, 585, 697, 432, 753, 679, 601, 339, 287, 93,
  93, 93, 93, 251, 372, 674, 565, 482, 317, 293, 703, 710, 783, 775, 748, 762, 782, 655, 779, 664, 722, 678, 480, 633, 735, 512, 248, 93,
  93, 93, 186, 305, 479, 619, 558, 662, 476, 330, 235, 593, 568, 777, 778, 727, 772, 765, 704, 548, 446, 676, 648, 421, 604, 528, 252, 93, 
  93, 93, 192, 299, 469, 383, 502, 577, 352, 504, 551, 626, 594, 677, 781, 780, 730, 574, 327, 306, 427, 572, 483, 539, 608, 417, 250, 93,
  93, 93, 215, 265, 466, 388, 387, 578, 503, 445, 540, 557, 473, 595, 776, 599, 353, 646, 668, 542, 374, 620, 628, 754, 696, 470, 258, 93,
  93, 93, 406, 236, 364, 494, 587, 369, 685, 275, 653, 266, 752, 228, 699, 747, 728, 396, 645, 622, 732, 637, 365, 666, 658, 294, 284, 93,
  93, 93, 307, 381, 550, 606, 714, 334, 249, 511, 556, 723, 784, 758, 660, 610, 328, 583, 625, 688, 632, 580, 592, 439, 643, 514, 310, 93, 
  93, 93, 204, 458, 659, 441, 376, 301, 546, 611, 667, 736, 757, 371, 669, 322, 675, 423, 448, 501, 336, 438, 555, 271, 449, 472, 368, 93,
  93, 93, 93, 434, 435, 500, 498, 712, 477, 661, 617, 651, 521, 513, 584, 614, 549, 280, 590, 618, 451, 510, 603, 302, 462, 337, 211, 93,
  93, 93, 93, 419, 315, 373, 561, 425, 724, 742, 316, 698, 654, 324, 707, 553, 319, 544, 672, 683, 329, 605, 532, 515, 390, 270, 253, 189,
  93, 93, 255, 621, 631, 428, 391, 746, 689, 497, 378, 496, 455, 527, 743, 726, 405, 509, 478, 684, 768, 766, 671, 616, 332, 237, 194, 191,
  93, 93, 520, 630, 729, 579, 624, 531, 447, 640, 638, 256, 582, 650, 734, 508, 711, 291, 279, 602, 733, 686, 718, 596, 355, 230, 207, 190,
  247, 93, 430, 647, 600, 566, 681, 348, 292, 313, 642, 538, 767, 700, 682, 612, 725, 384, 468, 644, 522, 341, 535, 491, 358, 426, 229, 187,
  93, 93, 285, 552, 529, 530, 486, 402, 499, 410, 338, 719, 346, 691, 701, 755, 673, 333, 490, 721, 564, 569, 463, 314, 351, 453, 222, 93,
  93, 93, 268, 318, 395, 507, 281, 350, 403, 440, 589, 627, 665, 409, 276, 375, 694, 484, 272, 397, 461, 420, 424, 304, 380, 429, 93, 93,
  93, 93, 212, 242, 485, 543, 444, 361, 656, 262, 635, 581, 492, 570, 717, 243, 641, 471, 588, 412, 399, 382, 349, 415, 360, 240, 93, 93,
  93, 93, 93, 238, 366, 386, 431, 325, 505, 623, 456, 401, 286, 442, 586, 609, 457, 639, 454, 597, 370, 326, 524, 460, 331, 277, 261, 93,
  93, 93, 93, 233, 308, 517, 613, 740, 562, 437, 545, 519, 407, 523, 738, 534, 690, 474, 404, 649, 573, 379, 278, 295, 231, 283, 260, 93,
  93, 93, 93, 93, 547, 567, 708, 705, 680, 398, 737, 634, 571, 576, 493, 713, 598, 392, 443, 274, 254, 218, 312, 309, 201, 282, 93, 93, 
  93, 93, 193, 93, 345, 300, 663, 715, 764, 744, 422, 692, 739, 591, 695, 771, 693, 716, 756, 731, 495, 418, 450, 356, 216, 224, 93, 93,
  93, 93, 93, 221, 413, 487, 289, 706, 745, 759, 770, 750, 763, 761, 769, 774, 720, 629, 709, 702, 525, 436, 452, 335, 196, 93, 93, 93,
  93, 93, 93, 93, 93, 93, 354, 575, 559, 357, 563, 541, 481, 347, 344, 267, 321, 359, 465, 400, 93, 93, 93, 93, 93, 93, 93, 93
)
    
result_49 <- list(accuracy = 0.977986348122867, lambda = 21.9758474587424)
percentage_49 <- (result_49$accuracy)*100
plot_beta_coef(result_49$lambda, digit_a, digit_b, train_nist)
visualize_beta_ranks(beta_ranks_49)

# Optional analysis - Comparing all digit pairs ------------------------------------
digits <- 0:9

# Initialize structures to store results
lambda_optimals <- matrix(0, nrow = 10, ncol = 10, dimnames = list(digits, digits))
accuracy_matrix <- matrix(0, nrow = 10, ncol = 10, dimnames = list(digits, digits))
beta_aggregate <- numeric(784) # Image length 28x28

# Iterate over all unique pairs of digits -> This execution takes a while
for(i in digits) {
  for(j in digits) {
    # Symmetric matrix
    if(j > i) {
      result <- train_and_evaluate(i, j, train_nist, test_nist)
      accuracy_matrix[i+1, j+1] <- result$accuracy
      lambda_optimals[i+1, j+1] <- result$lambda
      beta_aggregate <- beta_aggregate + abs(result$beta_values) # Using absolute values for simplicity
      
      print(paste(i, "vs", j))
      print(paste("Accuracy: ", accuracy_matrix[i+1, j+1]))
      print(paste("Optimal lambda: ", lambda_optimals[i+1, j+1]))
      print(paste("Beta aggregate: ", beta_aggregate))
    }
  }
}

# Average beta values
beta_aggregate <- beta_aggregate/(length(digits)*(length(digits)-1)/2)
beta_ranks <- rank(beta_aggregate, ties.method = "average") # Rank betas; high absolute values get high ranks
visualize_beta_ranks(beta_ranks)

print(lambda_optimals)
print(accuracy_matrix)

# Getting some general insight
average_accuracy <- sum(accuracy_matrix[accuracy_matrix > 0]) / 45 # 45 models are evaluated

highest_accuracy <- max(accuracy_matrix[accuracy_matrix > 0])
highest_position <- which(accuracy_matrix == highest_accuracy, arr.ind = TRUE) - 1 # Index start at 1, digits goes 0-9

lowest_accuracy <- min(accuracy_matrix[accuracy_matrix > 0])
lowest_position <- which(accuracy_matrix == lowest_accuracy, arr.ind = TRUE) - 1


# Precomputed data: lambda, accuracy, and beta_ranks coefficients
lambda_precomputed <- c(
  0, 11.72903, 39.492636, 24.853003, 10.164498, 16.669416, 7.167558, 4.786700, 29.91104, 16.311870,
  0, 0, 4.386601, 9.245058, 10.907331, 4.542014, 7.198158, 5.834560, 24.04870, 5.250782,
  0, 0, 0, 49.861094, 33.663612, 15.672550, 10.926998, 19.236063, 34.30107, 5.962494,
  0, 0, 0, 0, 4.527115, 39.707292, 4.517932, 20.517036, 52.98839, 13.550726,
  0, 0, 0, 0, 0, 9.765175, 15.969006, 12.924441, 24.75766, 20.023574,
  0, 0, 0, 0, 0, 0, 21.776718, 4.110943, 59.91032, 23.640739,
  0, 0, 0, 0, 0, 0, 0, 10.752439, 11.15123, 8.774185,
  0, 0, 0, 0, 0, 0, 0, 0, 10.33513, 35.950445,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 21.651250,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

# Load precomputed data: lambda optimal
lambda_data <- matrix(lambda_precomputed, nrow = 10, ncol = 10, byrow = TRUE)
rownames(lambda_data) <- colnames(lambda_data) <- as.character(0:9) # Assign row and column names
print(lambda_data)

accuracy_precomputed <- c(
  0, 0.9996938, 0.9925422, 0.9940961, 0.9971919, 0.9896266, 0.9931395, 0.9974579, 0.9936233, 0.9965301,
  0, 0, 0.9979857, 0.9972532, 0.9987382, 0.9968699, 0.9992197, 0.9974164, 0.9900031, 0.9977911,
  0, 0, 0, 0.9786718, 0.9897925, 0.9872064, 0.9900728, 0.9930824, 0.9816164, 0.9889521,
  0, 0, 0, 0, 0.9970370, 0.9745047, 0.9980466, 0.9925574, 0.9718103, 0.9911082,
  0, 0, 0, 0, 0, 0.9935622, 0.9947723, 0.9941003, 0.9935854, 0.9773038,
  0, 0, 0, 0, 0, 0, 0.9895760, 0.9967421, 0.9759462, 0.9923077,
  0, 0, 0, 0, 0, 0, 0, 0.9996759, 0.9939920, 0.9979757,
  0, 0, 0, 0, 0, 0, 0, 0, 0.9943219, 0.9801639,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0.9890240,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

# Load precomputed data: accuracy matrix
accuracy_precomputed <- matrix(accuracy_precomputed, nrow = 10, ncol = 10, byrow = TRUE)
rownames(accuracy_precomputed) <- colnames(accuracy_precomputed) <- as.character(0:9)
print(accuracy_precomputed)

# Getting some general insight
average_accuracy <- sum(accuracy_precomputed[accuracy_precomputed > 0]) / 45 # 45 models are evaluated

highest_accuracy <- max(accuracy_precomputed[accuracy_precomputed > 0])
percentage_best <- highest_accuracy*100
highest_position <- which(accuracy_precomputed == highest_accuracy, arr.ind = TRUE) - 1 # Index start at 1, digits goes 0-9

lowest_accuracy <- min(accuracy_precomputed[accuracy_precomputed > 0])
percentage_lowest <- lowest_accuracy*100
lowest_position <- which(accuracy_precomputed == lowest_accuracy, arr.ind = TRUE) - 1


beta_ranks_precomputed <- c(
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

# Load precomputed data: beta rank coefficients
visualize_beta_ranks(beta_ranks_precomputed)


# Add References -------------------------------------------------------------------

# Returns the names of all packages loaded in the current R session
knitr::write_bib(.packages(), "prueba.bib")
# Reference for a specific package
toBibtex(citation("glmnet"))

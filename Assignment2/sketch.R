# Required library
library(png)
# Load data
load(file = "qmnist_nist.RData")
# Visualization helper
show_digit <- function(x, col = gray(255:1 / 255), ...) {
  l <- sqrt(length(x))
  image(matrix(as.numeric(x), nrow = l)[, l:1], col = col, ...)
}

## Classifier ------------------------------------------------------------------
# Create average images
avg_train_images <- sapply(0:9, function(d) {
  colMeans(train_nist$px[train_nist$digit == d, ])
})
# Classifier function
classifier <- function(vec_img) {
  which.min(colMeans((avg_train_images - vec_img)^2)) - 1
}
# Visualize average train images
par(mfrow = c(2, 5), mar = rep(0, 4))
for (d in 1:10) {
  show_digit(avg_train_images[, d], axes = FALSE)
}

## Create test images to upload to the app -------------------------------------
# Save images from the test dataset using writePNG()
for (i in 0:9) {
  # Matrix with 0-1 entries
  img_vec <- test_nist$px[which(test_nist$digit == i)[1], ] / 255
  img_mat <- matrix(as.numeric(img_vec), nrow = 28, ncol = 28,
                    byrow = TRUE) # Saves it with the right orientation
  # Save image
  writePNG(image = img_mat, target = paste0("test-", i, ".png"))
}

## Check that the reading is fine ----------------------------------------------
# Read image
test_d <- 7 # Change me
test_img <- readPNG(paste0("test-", test_d, ".png"))
# Vectors with pixel values
vec_with_original_img <- test_nist$px[which(test_nist$digit == test_d)[1], ] # Imagen original, solo sirve para luego comprobar que se han creado bien
vec_with_read_img <- c(255 * t(test_img)) # Use scale 0-255 and flatten the
# rotated image so that the layout is comparable to $px
# The same!
max(abs(vec_with_original_img - vec_with_read_img)) # 0 -> indica que no hay diferencia entre la original y la creada
par(mfrow = c(1, 1))
show_digit(vec_with_original_img, axes = FALSE)


show_digit(vec_with_read_img, axes = FALSE)
# Classify
classifier(vec_with_read_img) # Success!

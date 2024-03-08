# Handwritten Digit Recognition App

This application is designed for applying machine learning techniques to identify and predict handwritten digits from image files.

## What is handwritten digit recognition?

Handwritten digit recognition is the process of identifying and interpreting human handwriting. This task can be particularly challenging due to the diverse styles and imperfections in human handwriting. This app aims to tackle these challenges by employing a variety of machine learning models.

## About MNIST dataset

This dataset contains over 60.000 grayscale images of handwritten digits (from 0 to 9), stored as collections of pixel values. It is divided into two subsets: training set with 30.950 images and a testing set with 30.405 examples.


## Our approach

**Model training**. 

We employ the following classifiers to predict digits from your uploaded PNG images:

- *Average Image Classifier*: This model identifies digits by comparing them against the average images for each digit.
- *KNN (K-Nearest Neighbors)*: Model that classifies digits based on similarity to known examples.
- *Random Forest*: An ensemble model that uses multiple decision trees to improve prediction accuracy.

**Model evaluation**. 

The performance of our trained model is assessed using the reserved test set. The evaluation metric is the accuracy, defined as the proportion of the number of correctly identified digits to the total number of digits in the test set. This metric provides a straightforward measure of our models' effectiveness.

## Interacting with the app

For exploring the capabilities of the machine learning techniques:

1. Navigate to the "Analyze Digit" tab.
2. Upload a PNG image of a handwritten digit.
3. Select a classification model.
4. Click "Make Prediction" to see the model's prediction and the accuracy achieved at the testing set.

Furthermore, on the "Model Performance" tab is done a brief comparison of the models over the testing set.

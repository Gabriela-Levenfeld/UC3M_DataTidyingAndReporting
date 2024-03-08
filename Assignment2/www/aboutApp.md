# Handwritten Digit Recognition App

This application is designed for an academic purpose in applying machine learning techniques to identify and predict handwritten digits from image files.

## What is handwritten digit recognition?

Handwritten digit recognition is the process of identifying and interpreting human handwriting. This task can be particularly challenging due to the diverse styles and imperfections in human handwriting. This app aims to tackle these challenges by employing a variety of machine learning models.

## About MNIST dataset

This dataset contains more than 60.000 images. It contains both the training (30950) and testing (30405) datasets, storing the images as a collections of pixel values.

## Our approach

**Model training**. 

We utilize the following classifiers to predict digits from your uploaded PNG images:

- *Average Image Classifier*: This simple yet effective model compares your image to the average digit images.
- *KNN (K-Nearest Neighbors)*: A model that classifies digits based on similarity to known examples.
- *Random Forest*: An ensemble model that uses multiple decision trees to improve prediction accuracy.

**Model evaluation**. 

The performance of our trained model is assessed using the reserved test set. The evaluation metric is the accuracy, defined as the proportion of the number of correctly identified digits to the total number of digits in the test set.

## How to use the app?

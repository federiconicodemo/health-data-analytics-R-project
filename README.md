# Health Data Analytics: Statistical Learning in R

![R](https://img.shields.io/badge/R-4.2%2B-blue.svg)
![License](https://img.shields.io/badge/License-GPLv3-blue.svg)

> **Project Note**
> This repository contains the final project for the **Health Data Analytics** course at the University of Salerno (A.Y. 2024/2025), taught by Prof. Fabio Postiglione. The primary goal is not to maximize predictive performance but to demonstrate the correct application and interpretation of various statistical learning models on biomedical data.

## Project Overview

This project applies a series of statistical learning techniques to address **classification** and **regression** problems using two health-related datasets. The entire analysis was conducted in **R** and RStudio.

1.  **Classification Task**: Predicting survival and the histological stage of the disease using a dataset of patients with Primary Biliary Cirrhosis (PBC).
2.  **Regression Task**: Modeling public health impact based on air quality and meteorological data.

## Techniques and Models Used

### Part I: Exploratory Data Analysis (EDA)
-   Distribution analysis of variables (numeric and categorical).
-   Correlation matrices (Spearman and Pearson).
-   Statistical hypothesis testing (Shapiro-Wilk, Fisher's Exact Test, Kruskal-Wallis).
-   Principal Component Analysis (PCA).
-   Handling of imbalanced data and "ceiling effects" via undersampling.

### Part II: Classification (Cirrhosis Dataset)
-   **k-Nearest Neighbors (k-NN)**, including the weighted version (Kernelized k-NN).
-   **Naive Bayes Classifier**.
-   **Logistic Regression** (binomial and multinomial), with **Elastic Net** regularization.

### Part III: Regression (Air Quality Dataset)
-   **Multiple Linear Regression**.
-   **Ridge Regression** and **Lasso Regression**.
-   **Quantile Regression**.
-   **k-Nearest Neighbors (k-NN) Regression**.
-   **Polynomial Regression**.
-   **Linear Models with Interactions** and model selection techniques (AIC, BIC, Stepwise).

## Datasets

1.  **Cirrhosis Prediction Dataset**: [Link to Kaggle](https://www.kaggle.com/fedesoriano/cirrhosis-prediction-dataset)
2.  **Air Quality and Health Impact Dataset**: [Link to Kaggle](https://www.kaggle.com/dsv/8675842)

## Setup and Usage

### Requirements
You will need a working installation of **R** and **RStudio**.

### Steps to Run

1.  **Clone the Repository**
    Open your terminal and run the following command:
    ```bash
    git clone https://github.com/federiconicodemo/health-data-analytics-R-project.git
    cd health-data-analytics-R-project
    ```

2.  **Install Required Packages**
    Open your R or RStudio console and run the following command to install all necessary packages:


3.  **Run the Analysis**
    Once the packages are installed, open the project in RStudio and run the script files to reproduce the analysis.


## License

This project is licensed under the GNU General Public License v3.0. See the `LICENSE` file for more details.

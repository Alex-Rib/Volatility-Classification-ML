# Market Volatility and Exogenous Factors: A Comparative Analysis on the CAC40, DAX, and S&P500

![R](https://img.shields.io/badge/R-4.0%2B-blue)
![Machine Learning](https://img.shields.io/badge/Machine_Learning-Classification-orange)
![Finance](https://img.shields.io/badge/Finance-Market_Prediction-green)
![Tidyverse](https://img.shields.io/badge/Tidyverse-Enabled-9cf)
![Status](https://img.shields.io/badge/Status-Educational-yellow)

An analytical project investigating whether a relationship exists between stock market volatility (CAC40, DAX, S&P500), weather conditions in the host cities of these indices (Paris, Frankfurt, and New York), and the calendar.

## 📊 Description

This project combines weather and calendar data with financial market data to train machine learning models, aiming to determine whether it is possible to predict volatility regimes using these exogenous factors. 

### Analyzed Indices
- **CAC40** (France) - Paris
- **DAX** (Germany) - Frankfurt  
- **S&P500** (USA) - New York

### Analysis Period
January 2010 - January 2025

## Installation

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Required R Packages

The script automatically installs missing packages, but you can also install them manually:

```r
install.packages(c(
  "httr", "jsonlite", "dplyr", "lubridate", "tidyquant", 
  "TTR", "PerformanceAnalytics", "caret", "tibble", "ggplot2",
  "tidyr", "xgboost", "randomForest", "gbm", "pROC", "purrr", "scales"
))
```

## 📝 Usage

### Step 1: Data Preparation

First, run the data preparation script:

```bash
Rscript data.frame.R
```

This script:
- Fetches historical weather data via the Open-Meteo API
- Downloads stock market data via Yahoo Finance
- Computes volatility using the Garman-Klass method
- Creates the train/test splits
- Saves the datasets into the `DATA/` directory

### Step 2: Modeling and Analysis

Next, run the main analysis script:

```bash
Rscript volatility_and_weather.R
```

This script:
- Loads the preprocessed data
- Trains 6 different models (GLM, GLMNet, RPART, Random Forest, gbm, XGBoost)
- Evaluates performance metrics (AUC, ROC curves, confusion matrix)
- Generates visualizations
- Saves all outputs into the `RESULTATS/` directory

## 📂 Project Structure

```
volatility/
├── README.md
├── data.frame.R              # Data preparation and preprocessing
├── volatility_and_weather.R  # Modeling and analysis pipeline
├── DATA/                     # Generated datasets (git-ignored)
│   ├── CAC40.rds
│   ├── DAX.rds
│   ├── SP500.rds
│   └── *_train.rds / *_test.rds
└── RESULTATS/                # Generated plots and figures (git-ignored)
    ├── CAC40/
    ├── DAX/
    └── SP500/
```

## 🔍 Methodology

### Features Used
- **Volatility**: Computed using the Garman-Klass estimator
- **Weather**: Temperature, precipitation
- **Temporal**: Day of the week, month, season

### Trained Models
1. Logistic Regression (GLM)
2. Regularized Logistic Regression (GLMNet)
3. Decision Tree (RPART)
4. Random Forest
5. Gradient Boosting Machine (GBM)
6. XGBoost

### Validation Protocol
- 80/20 time-series split
- Rolling-window cross-validation (time series)
- Performance metrics: AUC, Sensitivity, Specificity, Accuracy

## 📈 Results

The generated outputs include:
- ROC curves for each model
- Performance comparison charts (AUC comparison)
- Performance metric heatmaps
- Feature importance plots
- Historical volatility timeline plots

##  Data Sources

- **Weather Data**: [Open-Meteo Archive API](https://open-meteo.com/)
- **Market Data**: Yahoo Finance (retrieved via the `tidyquant` package)

##  Author

Alexandre R. - Université Paris Cité

# Time Series Forecasting for Tax Revenue

## Table of Contents
* [Overview](#overview)
* [Technologies Used](#technologies-used)
* [Methodology](#methodology)
  * [Data Preprocessing](#data-preprocessing)
  * [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
  * [Models Implemented](#models-implemented)
* [Results & Model Performance](#results--model-performance)
* [Key Findings](#key-findings)
* [Future Enhancements](#future-enhancements)
* [Contributors](#contributors)
* [Contact](#contact)

## Overview
This project explores time series forecasting techniques to predict **quarterly state and local government tax revenue** in the U.S. from **2009 to 2023**. The dataset is sourced from the **U.S. Census Bureau**, and various regression and time series models were applied to analyze trends and predict future values.

## Technologies Used
- **R** (forecast, zoo, ggplot2, tseries libraries)
- **Time Series Analysis** (ARIMA, Auto-ARIMA, Quadratic Trend, Exponential Model)
- **Model Evaluation Metrics** (RMSE, MAPE)

## Methodology
### Data Preprocessing
- Removed incomplete records 
- Used **quarterly** data from **2009-2023**
- Partitioned dataset into **training (2009-2019) and validation (2020-2023)**

### Exploratory Data Analysis (EDA)
- Identified **trends and seasonality**
- Used **autocorrelation (ACF) and differencing** for stationarity assessment

### Models Implemented
- **Quadratic Trend Model**
- **Quadratic Trend + Seasonality**
- **Exponential Model**
- **ARIMA (1,1,1)(1,1,1)**
- **Auto-ARIMA**

## Results & Model Performance
| Model | RMSE (Validation) | MAPE (Validation) |
|--------|-----------------|-----------------|
| ARIMA (1,1,1)(1,1,1) | **5179.97** | **1.22%** |
| Auto-ARIMA | **5218.36** | **1.24%** |
| Quadratic Regression | 15376.13 | 2.58% |
| Quadratic + Seasonality | 15201.87 | 2.69% |
| Exponential Model | 18680.78 | 3.05% |
| Naive Forecast | 20251.23 | 2.61% |

## Key Findings
- **ARIMA (1,1,1)(1,1,1)** provided the most **accurate forecast** with the lowest **RMSE and MAPE**.
- **Quadratic models** captured long-term trends but lacked adaptability to fluctuations.
- **Exponential models** underperformed due to high variance.
- **Auto-ARIMA** performed well but did not significantly outperform manually tuned ARIMA.

## Future Enhancements
- **Incorporate external factors** (e.g., economic indicators, policy changes)
- **Test deep learning models** like LSTM for time series forecasting
- **Deploy an interactive dashboard** for real-time trend analysis

## Contributors
- **Vinmathi Iyappan**

## Contact
📧 **Email:** [vinmathi.iyappan@gmail.com](mailto:vinmathi.iyappan@example.com)  
🔗 **LinkedIn:** [YourLinkedInProfile](https://linkedin.com/in/yourprofile)  
🖥 **GitHub:** [YourGitHubProfile](https://github.com/yourprofile)


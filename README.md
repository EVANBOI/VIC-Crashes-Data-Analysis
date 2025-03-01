# Data Analysis on Fatal Victorian Road Crashes  

## Project Overview  
This project analyzes road crash data provided by VICRoads to identify key factors contributing to fatal accidents. The ultimate goal is to support an information campaign aimed at reducing road fatalities.  

## Data Analysis Stages  

### Exploratory Data Analysis (EDA)  
**Objective:**  
Generate visualizations and summary statistics to uncover patterns and relationships within the dataset.  

**Key Findings:**  
- The dataset is highly imbalanced, with fatal accidents accounting for only **2%** of cases.  
- Potential predictors for further analysis include:  
  - Vehicle color  
  - Driver age  
  - Vehicle manufacturing year  

### Statistical Modelling  
**Approach:**  
Logistic regression is used to identify significant predictors of fatal accidents. This involves:  
- **Hypothesis testing** to assess statistical significance (*p-values below 5% indicate significance*).  
- Using a **deviance table** to evaluate whether increasing model complexity improves predictive performance.  

### Predictive Modelling  
**Methodology:**  
A variety of predictive models were tested, including:  
- **K-Nearest Neighbors (KNN)**  
- **Random Forests**  
- **Logistic Regression with Lasso and Ridge Regularisation**  

**Evaluation:**  
The **Gradient Boosting Machine (GBM)** model was selected due to its superior **AUC score**, achieving an **80.7% prediction accuracy** in identifying high-risk fatal accident scenarios.  

## Key Findings & Implications  
The most significant predictors of fatal road crashes include:  
- **Driver’s sex**  
- **Road surface condition** at the time of the crash  
- **Vehicle manufacturing year**  
- **Vehicle type**  
- **Total number of occupants**  
- **Whether a seatbelt was worn**  

This analysis provides valuable insights that can inform **policy development and targeted interventions** to reduce road fatalities.  

## Technical Details  
**Tools Used:**  
- R

**Challenges & Solutions:**  
- **Imbalanced Data:**  
  - Since only **2%** of cases were fatal accidents, traditional random sampling would lead to an over or under-representation of fatal crashes in the training and test sets.  
  - To address this, **stratified sampling** was used to ensure a more balanced distribution.
  
---

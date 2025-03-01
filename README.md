# Data Analysis on Fatal Victorian Road Crashes 
## Project Overview
The goal of this project is to analyse road crash data provided by VICRoads and identify the key factors that drive fatal accidents with the end aim to target an information campaign. 
## Data Analysis Stages 
### Exploratory Data Analysis (EDA)
Objective: Create visualisations and tables in hopes of identifying patterns and relationships present in the provide dataset.
Outcomes:
* Determined that the data provided is imbalanced as 2% of the data are fatal accidents.
* Identification of potential predictors, i.e. colour of vehicle, age of driver, manufacturing year of vehicle for more detail analysis.
### Statistical Modelling 
Approach: Application of logistic regression to ascertain significant predictors of fatal accidents. This is achieved by performing hypothesis tests, and checking if the p-values are above the 5% significance level. A confirmatory test in the form of a deviance table is used to determine whether there is a significant improvement in the model when increasing the model complexity.
### Predictive Modelling 
Methodology: A range of predictive models were tested, including K-nearest neighbours, Random forests, logistic regression with lasso and ridge.
Evaluation: The GBM model was selected due to its superior AUC scores, indicating high accuracy in predicting scenarios with a high risk of fatalities. The model achieved a prediction accuracy of 80.7%.
## Key Findings & Implications
Significant Predictors of fatal road crashes:
* Sex of driver
* Surface condition of crash
* Manufacturing year of vehicle
* Type of vehicle
* Total number of occupants
* Not wearing seatbelt
Performing an analysis of the data provides a foundation for informed decision making and policy development aimed at reducing road fatalities.
## Technical Details
Tool Used: R
Main Challenges: The data is imbalanced, hence to obtain a sample of the data, stratified sampling was considered over the traditional random sampling suhc the number of fatal accidents will not be over/under represented in the training and test sets. 

 


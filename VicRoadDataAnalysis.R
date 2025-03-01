#Packages Used 
library(dplyr)
library(ggplot2)
library(bestglm)
library(glmnet)
library(caret)
library(pROC)
library(class)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
#Importing Data set 
Vic_crashes <- read.csv("C:/Users/Evan/Downloads/VicRoadFatalData.csv")

#Obtaining Elementary Data 
str(Vic_crashes)
summary(Vic_crashes)

#Proportion of fatal accidents 
Fatal_prop <- Vic_crashes %>% group_by(fatal) %>% 
  summarise( proportions = n() / nrow(Vic_crashes))                                                                         

#To ensure the predictors are not over/under-represented we calculate the proportion of the fatal accidents instead of determining the count.

#Exploring the relationship road geometry and speed zone have on the proportion of fatal accidents
crash_condition_prop <- Vic_crashes %>% group_by(SPEED_ZONE, ROAD_GEOMETRY) %>% 
  summarise(count = n(), fatality = sum(fatal == "TRUE")) %>% 
  mutate(FatalityProportion = fatality / count )

ggplot(data = crash_condition_prop, mapping = aes(x = SPEED_ZONE, y = FatalityProportion, fill = ROAD_GEOMETRY)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Proportion of Fatal Accidents") +
  xlab("Speed Zone") +
  ggtitle("The Relationship Road Geometry and Speed Zone have on the Proportion of Fatal Accidents") + 
  theme_minimal()

#Fatality Rate By Time of Day
Vic_crashes$HOUR_OF_DAY <- 
  as.numeric(format(strptime(Vic_crashes$ACCIDENTTIME, "%H:%M:%S"), "%H"))

Time_Day_Prop <- Vic_crashes %>% group_by(DAY_OF_WEEK, HOUR_OF_DAY) %>% 
  summarise(count = n(), fatality = sum(fatal == "TRUE")) %>% 
  mutate(FatalityProportion = fatality/count)

  ggplot(data = Time_Day_Prop, mapping = aes(x = HOUR_OF_DAY, 
                                             y = FatalityProportion, 
                                             color = DAY_OF_WEEK, 
                                             group = DAY_OF_WEEK)) +
    geom_line() + 
    geom_point() +
    ylab("Proportion of Fatal Accidents") + 
    xlab("Time Of Day (24 Hour Time)") + 
    labs(color = "Day of Week") + 
    ggtitle("Proportion of Fatal Accidents by Time of Day") +
    scale_colour_discrete(labels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                     "Thursday", "Friday", "Saturday")) +
    theme_minimal()
  
#Sex, Age Relationship 
#grouped bar chart

  
#Exploring the relationship between sex and the proportion of fatalities
sex_group_prop <- Vic_crashes %>% 
  group_by(SEX) %>% 
  summarise(count = n(), fatality = sum(fatal == "TRUE")) %>% 
  mutate(FatalityProportion = fatality / count)

#males are risk takers resulting in them having a higher proportion to crashes

ggplot(data = sex_group_prop, aes(x = SEX, y = FatalityProportion, fill = SEX)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(FatalityProportion)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Proportion of Fatal Accidents by Sex",
       x = "Sex",
       y = "Proportion of Fatal Accidents") +
  scale_fill_discrete(name = "Sex") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none") 

#Exploring the relationship between age and the proportion of fatalities
age_group_prop <- Vic_crashes %>%
  group_by(Age.Group) %>%
  summarise(count = n(), fatality = sum(fatal == "TRUE")) %>% 
  mutate(FatalityProportion = fatality / count)

ggplot(data = age_group_prop, aes(x = Age.Group, y = FatalityProportion, fill = Age.Group)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = scales::percent(FatalityProportion)), 
            position = position_dodge(width = 0.9), vjust = -0.5) + 
  labs(title = "Proportion of Fatal Accidents by Age Group",
       x = "Age Group",
       y = "Proportion of Fatal Accidents") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none") 



#Exploring the relationship between vehicle colour and the proportion of accidents
colour_prop <- Vic_crashes %>% group_by(VEHICLE_COLOUR) %>% 
  summarise(count = n(), fatality = sum(fatal == "TRUE")) %>% 
  mutate(FatalityProportion = fatality / count)

ggplot(data = colour_prop, mapping = aes(x = VEHICLE_COLOUR, y = FatalityProportion, fill = VEHICLE_COLOUR)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = scales::percent(FatalityProportion)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Proportion of Fatal Accidents by Vehicle Colour",
       x = "Colour", 
       y = "Proportion of Fatal Accidents") + 
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.position = "none")


#Exploring the relationship between the manufacture year of vehicle and the proportion of accidents
#surprisingly the most recently manufactured cars correspond to higher fatality rate

manu_prop <- Vic_crashes %>% group_by(VEHICLE_YEAR_MANUF) %>% 
  summarise(count = n(), fatality = sum(fatal == "TRUE")) %>% 
  mutate(FatalityProportion = fatality / count)

ggplot(data = manu_prop, mapping = aes(x = VEHICLE_YEAR_MANUF, y = FatalityProportion)) + 
  geom_line(color = "blue") + geom_point(color = "Red") +
  labs(title = "Relationship between the Manufacturing Year of the Vehicle and the Fatality Proportion",
       x = "Manufacturing Year",
       y = "Proportion of Fatal Accidents") 



#Building a GLM 

#Excluding Variables
#They are insignificant and are encompassed by other variables
Vic_crashes_clean <- Vic_crashes[, !(names(Vic_crashes) %in% c("DRIVER_ID", 
                                                               "VEHICLE_ID", 
                                                               "OWNER_POSTCODE", 
                                                               "LICENCE_STATE", 
                                                               "ACCIDENT_NO", 
                                                               "FUEL_TYPE", 
                                                               "ACCIDENTTIME", 
                                                               "AGE", 
                                                               "ACCIDENTDATE",
                                                               "VEHICLE_MAKE",
                                                               "VEHICLE_BODY_STYLE",
                                                               "ATMOSPH_COND",
                                                               "ROAD_SURFACE_TYPE",
                                                               "LIGHT_CONDITION"))]

#Transforming the data set                                                                                                                              "ATMOSPH_COND"))]
Vic_crashes_clean$fatal <- as.numeric(Vic_crashes_clean$fatal)
Vic_crashes_clean$VEHICLE_COLOUR <- as.factor(Vic_crashes_clean$VEHICLE_COLOUR)
Vic_crashes_clean$SEX <- as.factor(Vic_crashes_clean$SEX)
Vic_crashes_clean$ROAD_GEOMETRY <- as.factor(Vic_crashes_clean$ROAD_GEOMETRY)
Vic_crashes_clean$DAY_OF_WEEK <- as.factor(Vic_crashes_clean$DAY_OF_WEEK)
Vic_crashes_clean$Age.Group <- as.factor(Vic_crashes_clean$Age.Group)
Vic_crashes_clean <- Vic_crashes_clean %>% relocate(fatal, .after = HOUR_OF_DAY)

Vic_crashes_clean$VEHICLE_TYPE




#Lasso Regression 

#stratified sampling
set.seed(35)
train_index <- createDataPartition(Vic_crashes_clean$fatal, p = 0.75, list = FALSE) 

train_set <- Vic_crashes_clean[train_index, ]
test_set <- Vic_crashes_clean[-train_index, ]

x <- model.matrix(fatal ~ ., data = train_set)[, -1]
y <- train_set$fatal

test_set$fatal

#The data has been stratified properly
prob <- sum(train_set$fatal == 1)/nrow(train_set)
pro <- sum(Vic_crashes_clean$fatal == 1)/nrow(Vic_crashes_clean)
proba <- sum(test_set$fatal == 1)/nrow(test_set)
print(prob)
print(pro)
print(proba)
#This indicates the proportions are very similar


#Model under Lasso Regression
set.seed(115)
lasso.mod <- glmnet(x, y, alpha = 1, family = "binomial",
                    standardize = TRUE)

#Finding the best lambda within one standard error via cross validation
cv.out_lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial", standardize = TRUE)

print(cv.out_lasso)

final_lasso_mod <- glmnet(x,y, alpha =1, lambda = cv.out_lasso$lambda.1se, family = "binomial")


#Determining the coefficients that were not set to 0
coefficients <- coef(final_lasso_mod)

predictor_names <- colnames(model.matrix(fatal ~ ., data = train_set))

summary(coefficients)

coefficients_mat <- as.matrix(coefficients)

predictor_names[coefficients_mat[, 1] != 0]

#Further Cleaning the Data 

#Adding new variables
new_Vic_crashes_clean <- Vic_crashes %>% mutate(AGE_Over_70 = ifelse(AGE > 70, 1, 0),
                                                Not_At_Inter = ifelse(ROAD_GEOMETRY == "Not at intersection", 1, 0),
                                                Male_SEX = ifelse(SEX == "M", 1, 0),
                                                Belt_not_worn = ifelse(HELMET_BELT_WORN == "Seatbelt not worn", 1, 0))

#These variables were minimised to 0 by lasso regression
new_Vic_crashes_clean <-  new_Vic_crashes_clean[, !(names(new_Vic_crashes_clean) %in% c("Age.Group",
                                                                          "HELMET_BELT_WORN",
                                                                          "ROAD_GEOMETRY",
                                                                          "SEX",
                                                                          "ROAD_SURFACE_TYPE",
                                                                          "DAY_OF_WEEK", 
                                                                          "VEHICLE_COLOUR",
                                                                          "DRIVER_ID", 
                                                                          "VEHICLE_ID", 
                                                                          "OWNER_POSTCODE", 
                                                                          "LICENCE_STATE", 
                                                                          "ACCIDENT_NO", 
                                                                          "FUEL_TYPE", 
                                                                          "ACCIDENTTIME", 
                                                                          "AGE", 
                                                                          "ACCIDENTDATE",
                                                                          "VEHICLE_MAKE",
                                                                          "VEHICLE_BODY_STYLE",
                                                                          "LIGHT_CONDITION",
                                                                          "ATMOSPH_COND",
                                                                          "HOUR_OF_DAY"))]

#Transforming Variables
new_Vic_crashes_clean$fatal <- as.numeric(new_Vic_crashes_clean$fatal)
new_Vic_crashes_clean$VEHICLE_TYPE <- as.factor(new_Vic_crashes_clean$VEHICLE_TYPE)
new_Vic_crashes_clean$ACCIDENT_TYPE <- as.factor(new_Vic_crashes_clean$ACCIDENT_TYPE)
new_Vic_crashes_clean$SURFACE_COND <- as.factor(new_Vic_crashes_clean$SURFACE_COND)
new_Vic_crashes_clean <- new_Vic_crashes_clean %>% relocate(fatal, .after = everything())


#Subset Selection 
best_glm <- bestglm(new_Vic_crashes_clean, IC = "BIC", family = binomial,
                         method = "forward")

#Using this to better prevent overfitting

#Best Model under forward subset selection
best_glm$Subsets

#Model selection criteria for scaled deviance
predict_col <- new_Vic_crashes_clean[, !names(new_Vic_crashes_clean) 
                                     %in% c("fatal", "VEHICLE_TYPE", 
                                            "ACCIDENT_TYPE", "SURFACE_COND")]
scale_predictors <- scale(predict_col)

#Add them back into the data set
scale_dataset <- cbind.data.frame(scale_predictors, 
                                  new_Vic_crashes_clean[c("VEHICLE_TYPE", 
                                                          "ACCIDENT_TYPE", "SURFACE_COND", 
                                                          "fatal")])

#BEST GLM
best_fatal_glm <- glm(fatal ~ SPEED_ZONE + ACCIDENT_TYPE + Belt_not_worn +
                      VEHICLE_TYPE + Male_SEX + AGE_Over_70 + TOTAL_NO_OCCUPANTS + SURFACE_COND +
                      Not_At_Inter + VEHICLE_YEAR_MANUF ,family = "binomial", 
                      data = scale_dataset)

summary(best_fatal_glm)

#satisfies scaled deviance test
print(anova(best_fatal_glm, test = "Chi"))

best_predict <- predict(best_fatal_glm, type = "response")

#sanity check
roc(scale_dataset$fatal, best_predict, plot = TRUE, legacy.axes = TRUE, percent = FALSE,
         xlab = "False Positive Rate", ylab = "True Positive Rate", 
         col = "blue", lwd = 4, print.auc = TRUE)


#Evaluating the Lasso Model 
final_lasso_mod_5 <- glmnet(x,y, alpha =1, lambda = cv.out_lasso$lambda.min, family = "binomial")
set.seed(100)
lasso_predict <- predict(final_lasso_mod_5, type = "response", newx = model.matrix(fatal ~ ., data = test_set)[, -1])

response <- ifelse(lasso_predict > 0.01699333, 1, 0)
#Does not represent the best threshold

confusion_matrix <- table(Actual = test_set$fatal, Predicted = response)
print(confusion_matrix)

roc_info <- roc(test_set$fatal, lasso_predict, plot = TRUE, legacy.axes = TRUE, percent = FALSE,
    xlab = "False Positive Rate", ylab = "True Positive Rate", 
    col = "blue", lwd = 4, print.auc = TRUE)
par(pty = "s")

roc_df <- data.frame( tpp = roc_info$sensitivities*100, fpp = (1 - roc_info$specificities)*100,
                      thresholds = roc_info$thresholds)

#Evaluating the Ridge Model 
ridge.mod <- glmnet(x, y, alpha = 0, family = "binomial", standardize = TRUE)

cv_out_ridge <- cv.glmnet(x, y, alpha = 0, family = "binomial", standardize = TRUE)

final_ridge_mod <- glmnet(x,y, alpha = 0, lambda = cv_out_ridge$lambda.min, family = "binomial")
set.seed(130)
ridge_predict <- predict(final_ridge_mod, type = "response", newx = model.matrix(fatal ~., data = test_set)[, -1])

response_2 <- ifelse(ridge_predict > 0.01699333, 1, 0)

confusion_matrix_2 <- table(Actual = test_set$fatal, Predicted = response_2)
print(confusion_matrix_2)

plot.roc(test_set$fatal, ridge_predict, add = TRUE, col = "red", lwd = 4, print.auc = TRUE,
         print.auc.y=0.4)
legend("bottomright", legend = c("Lasso Regression", "Random Forest"), col = c("blue", "red"), lwd = 4)





#Splitting the scaled data set
predict_col_2 <- Vic_crashes_clean[, !names(Vic_crashes_clean) 
                                         %in% c("fatal", "VEHICLE_TYPE","SURFACE_COND",
                                                "Age.Group", "HELMET_BELT_WORN",
                                                "VEHICLE_COLOUR", "DAY_OF_WEEK",
                                                "ACCIDENT_TYPE", "ROAD_GEOMETRY", 
                                                "SEX")]
#scale non-categorical data sets
scale_predictors_2 <- scale(predict_col_2)
  
scaled_dataset_2 <- cbind.data.frame(scale_predictors_2, 
                                       Vic_crashes_clean[c("VEHICLE_TYPE", 
                                                               "ACCIDENT_TYPE", "SURFACE_COND",
                                                               "Age.Group", "HELMET_BELT_WORN",
                                                               "VEHICLE_COLOUR", "DAY_OF_WEEK", "ROAD_GEOMETRY", 
                                                               "SEX", 
                                                               "fatal")])

scaled_dataset_2$VEHICLE_TYPE <- as.factor(scaled_dataset_2$VEHICLE_TYPE)
scaled_dataset_2$ACCIDENT_TYPE <- as.factor(scaled_dataset_2$ACCIDENT_TYPE)  
scaled_dataset_2$SURFACE_COND <- as.factor(scaled_dataset_2$SURFACE_COND)  
scaled_dataset_2$HELMET_BELT_WORN <- as.factor(scaled_dataset_2$HELMET_BELT_WORN)  
scaled_dataset_2$VEHICLE_COLOUR <- as.factor(scaled_dataset_2$VEHICLE_COLOUR)
scaled_dataset_2$DAY_OF_WEEK <- as.factor(scaled_dataset_2$DAY_OF_WEEK)
scaled_dataset_2$ROAD_GEOMETRY <- as.factor(scaled_dataset_2$ROAD_GEOMETRY)
scaled_dataset_2$SEX <- as.factor(scaled_dataset_2$SEX)

set.seed(42)
#Partitioning the dataset
train_indices <- createDataPartition(scaled_dataset_2$fatal, p = 0.75, list = FALSE)
train <- scaled_dataset_2[train_indices, ]
test <- scaled_dataset_2[-train_indices, ]
  
prob_1 <- sum(train$fatal == 1)/nrow(train)

#Evaluating Logistic Regression Model  
logistic_glm <- glm(fatal ~., family = "binomial", data = train)
set.seed(15)
pred_logistic <- predict(logistic_glm, newdata = test , type = "response")
  
response_3 <- ifelse(pred_logistic > 0.01707333, 1, 0)
confusion_matrix_3 <- table(Actual = test$fatal, Predicted = response_3) 
print(confusion_matrix_3)
  
roc_info_2 <- roc(test$fatal, pred_logistic, plot = TRUE, legacy.axes = TRUE, percent = FALSE,
                    xlab = "False Positive Rate", ylab = "True Positive Rate", 
                    col = "blue", lwd = 4, print.auc = TRUE)


#Evaluating the KNN model 
train.X <- lapply(train[, 1:13, drop = FALSE], as.numeric)
train.X <- as.data.frame(train.X)
test.X <- lapply(test[, 1:13, drop = FALSE], as.numeric)
test.X <- as.data.frame(test.X)

set.seed(50)
knn.pred <- knn(train = train.X, test = test.X, cl = factor(train$fatal), k = 1)
confusion_matrix_4 <- table(knn.pred, test$fatal)
#By reducing the K parameter, the AUC slightly increased

print(confusion_matrix_4)


k <- factor(knn.pred, ordered = TRUE)

plot.roc(test$fatal, k, add = TRUE, print.auc = TRUE, print.auc.y = 0.4, 
    col = "red", lwd = 4)
legend("bottomright", legend = c("Logistic Regression", "KNN"), col = c("blue", "red"), lwd = 4)

#Evaluating the classification tree model 
set.seed(197)
tree.vic_crashes <- rpart(fatal ~., train, 
                          control = rpart.control(cp = 0.002))
#Any cp above 0.003 only has two terminal nodes and hence doesn't provide enough information


rpart.plot(tree.vic_crashes, box.palette = "GnRd", yesno = 2, split.border.col = 1, split.round = 1)
pred_tree <- predict(tree.vic_crashes, test, type = "vector")

summary(tree.vic_crashes)
t <- factor(pred_tree, ordered = TRUE)
roc_info_3 <- roc(test$fatal, t, plot = TRUE, legacy.axes = TRUE, percent = FALSE,
                  xlab = "False Positive Rate", ylab = "True Positive Rate", 
                  col = "blue", lwd = 4, print.auc = TRUE)

#Evaluating the random forest model 
set.seed(99)
forest.vic_crashes <- randomForest(factor(fatal) ~., data = train, mtry= 3, ntree = 500)
pred_forest <- predict(forest.vic_crashes, newdata = test, type = "prob")

predictions <- as.data.frame(pred_forest)
predictions[,2]
plot.roc(test$fatal, predictions[,2], add = TRUE, print.auc = TRUE, print.auc.y = 0.4, col = "red", lwd = 4)


#Evaluating the bagging model 
set.seed(101)
bag.vic_crashes <- randomForest((factor(fatal)) ~., data = train, mtry = 13, ntree = 500)
pred_bag <- predict(bag.vic_crashes, newdata = test, type = "prob")
predictions_2 <- as.data.frame(pred_bag)

b <- factor(pred_bag, ordered = TRUE)
plot.roc(test$fatal, predictions_2[,2], add = TRUE, print.auc = TRUE, print.auc.y = 0.3, col = "green", lwd = 4)


#Evaluating the boosting model 
set.seed(550)
boost.vic_crashes <- gbm(fatal ~., data = train, distribution = "bernoulli", 
                         n.tree = 500, interaction.depth = 5)

pred_boost <- predict(boost.vic_crashes, newdata = test, n.trees = 500, type = "response")

response_4 <- ifelse(pred_boost > 0.017, 1, 0)
confusion_matrix_4 <- table(Actual = test$fatal, Predicted = response_4) 
print(confusion_matrix_4)

plot.roc(test$fatal, pred_boost, add=TRUE, print.auc = TRUE, print.auc.y = 0.2, col = "black", lwd = 4)
legend("bottomright", legend = c("Classification Trees", "Random Forest", "Bagging", "Boosting"), 
       col = c("blue", "red", "green", "black"), lwd = 4)

#Task 3 
drivers_data <- read.csv("C:/Users/Evan/Downloads/Drivers_Eval.csv")

#BOOSTING MODEL IS CHOSEN AS IT HAS BEST AUC

#Creating New Dataset
df <- Vic_crashes %>% select(1:15, 27) %>% mutate(DRIVER_ID = factor(DRIVER_ID),
                                                  SEX = factor(SEX),
                                                  Age.Group = factor(Age.Group),
                                                  LICENCE_STATE = factor(LICENCE_STATE),
                                                  HELMET_BELT_WORN = factor(HELMET_BELT_WORN),
                                                  VEHICLE_ID = factor(VEHICLE_ID),
                                                  VEHICLE_BODY_STYLE = factor(VEHICLE_BODY_STYLE),
                                                  VEHICLE_MAKE = factor(VEHICLE_MAKE),
                                                  VEHICLE_TYPE = factor(VEHICLE_TYPE),
                                                  FUEL_TYPE = factor(FUEL_TYPE),
                                                  VEHICLE_COLOUR = factor(VEHICLE_COLOUR),
                                                  fatal = as.numeric(fatal))
  



#SELECTING RELEVANT VARIABLES

predictors_df <- data.frame(df [, c("AGE", "SEX", "HELMET_BELT_WORN", "FUEL_TYPE",
                                     "TOTAL_NO_OCCUPANTS", "VEHICLE_TYPE", "VEHICLE_YEAR_MANUF",
                                    "fatal")])
set.seed(100)
#Fitting Model with Data
boost_model <- gbm(fatal ~., data = predictors_df, distribution = "bernoulli", n.tree = 500, 
                   interaction.depth = 5)

#Variable Importance
var_importance <- summary(boost_model)
print(var_importance)

#Manipulating the Drivers Evaluation Data
driver_eval <- data.frame(drivers_data[, c("AGE", "SEX", "HELMET_BELT_WORN", "FUEL_TYPE",
                                           "TOTAL_NO_OCCUPANTS", "VEHICLE_TYPE", "VEHICLE_YEAR_MANUF")])


#Making Predictions
pred_eval <- predict(boost_model, newdata = driver_eval, n.trees = 500,
                      type = "response")

pred <- driver_eval[order(pred_eval, decreasing = TRUE)[1:10000],]

fatal_drivers <- as.data.frame(cbind(drivers_data$DRIVER_ID, pred))

#Returning the 2500 drivers that are most likely to be involved in a fatal accident
fatal_id <- as.data.frame(fatal_drivers[1:2500,1])

write.csv(fatal_id, "FATAL_DRIVERS_EVAL.csv", row.names = FALSE)







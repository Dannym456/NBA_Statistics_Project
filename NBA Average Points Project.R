##Francisco Danny
##          3775581
##December 3, 2022
##STA 6244
##Dr. Florence George

##Legend: Wt:Weight, G:Games, FG:Field goals per game, 

library(dplyr)
library(stringr)
library(tidyverse)
library(caret)
library(olsrr)
library(Hmisc)
library(ggcorrplot)
library(caret)
library(pROC) 

set.seed(123)


NBA_stats = read.csv('C:/Users/danny/Desktop/STA_Project/NBA_Stats_1946-2023.csv')
  ##We want to remove the columns that don't matter to us (Number, League, Height, Age)
  ##The reason we are removing Height and age is because of they are stable throughout the years.
  ##We also want to remove the most recent season since the season is not over yet.
  NBA_stats = NBA_stats[,-which(names(NBA_stats) %in% c("Rk","Lg", "Ht", "Age", "MP", "TOV.",
                                                      "ORB.", "FT.FGA", "ORtg"))]
  NBA_stats = NBA_stats[-1,]
  NBA_stats = NBA_stats[-(44:77),]
  
  ##Splitting up our season column because it isn't in a valid format.
  Prefix = NBA_stats[c("SeasonPre", "SeasonPost")] = str_split_fixed(NBA_stats$Season, '-', 2)
  NBA_stats["Season"] = as.integer(Prefix[,1])
  NBA_stats = NBA_stats[,-which(names(NBA_stats) %in% c("SeasonPre", "SeasonPost"))]
  ##We want to add in the High or Low score indicators if a game is over 105 points; this would be
  ##for our Logistic Regression model
  
  ##Correlation matrix for our data set
  ##Comment out type = "lower" if you want to see the full correlation matrix
  ggcorrplot(cor(NBA_stats),  hc.order = TRUE,
             type = "lower",
             outline.color = "white",
             ggtheme = ggplot2::theme_gray,
             colors = c("#6D9EC1", "white", "#E46726"))

  ##Creating our binary variable for when we do the logistic model; 1 for games over 100 points and 
  ##0 for games under 100 points
NBA_stats = NBA_stats %>%
  mutate(ScoreValue = if_else(PTS >= 100, "1", "0"))

NBA_stats$ScoreValue = as.numeric(NBA_stats$ScoreValue)


# attach(NBA_stats)
  sapply(NBA_stats, class)
  
  ##Let try to find the best model
All_Model = lm(PTS ~ ., data = NBA_stats)
  step(All_Model)
  ##This has been the most successful model thus far; other than our perfect model which can't be used
  ##PTS ~ X3P + FG + FT
Points_Model = lm(PTS ~ X3P + FG + FT, data = NBA_stats)
  summary(Points_Model)
  ##Command below is very helpful for figuring out if there are any multicollinearity issues with our
  ##variables.
  car::vif(Points_Model)
  
  ##Perfect model in terms of R-squared and adjusted R-squared, but multicollinearity is present
  ##for many of the independent variables. Could also be a risk for overfitting.
Perfect_Model = lm(PTS ~ Season + G + FG + FGA + X3P + FT + DRB + TRB + AST + 
                    TOV + PF + FT. + Pace + eFG., data = NBA_stats)
  summary(PerfectModel)
  stepAIC(PerfectModel)
  car::vif(PerfectModel)

  ##Logistic Regression Model 
  NBA_stats$ScoreValue = as.numeric(NBA_stats$ScoreValue)
Log_Points_Model = glm(formula = ScoreValue ~  X3P + FG + FT, data = NBA_stats)
  summary(Log_Points_Model)
  car::vif(Log_Points_Model)
  

  ##Lets add in the regression line to each of these graphs##
  ## Linear Model for average 3 pointers made 
  TPoint_LM = lm(PTS ~ X3P)
  plot(PTS ~ X3P)
  abline(TPoint_LM)
  ## Linear Model for average total rebounds
  Rebound_LM = lm(PTS ~ TRB)
  plot(PTS ~ TRB)
  abline(Rebound_LM)
  ## Linear Model for average field goal percentage
  FG_LM = lm(PTS~FG)
  plot(PTS~FG)
  abline(FG_LM)
  ## Linear Model for average free throw percentage
  FT_LM = lm(PTS~FT)
  plot(PTS~FT)
  abline(FT_LM)

  ##Testing for Assumptions:
  ##Linearity: We can see from the graph that our linearity assumption is valid.
  ##Normality: We can see from the graph that our normality assumption is valid.
  ##Equal Variance: Equal Variance assumption appears to be valid as well.
plot(Perfect_Model)

#We want to test for most optimal model
k = ols_step_all_possible(Points_Model)
  ##We can use this to plot our stuffs
  plot(k$rsquare,type="l")
  plot(k$adjr,type="l")
  plot(k$cp,type="l")
  plot(k$aic, type="l")

#####################################################################################################
  
  ##Training and Testing##
  ##Lets start off by splitting out data set into training and testing sets
  ##We will work with a 70:30 split
  
Train_Index = sample(1:nrow(NBA_stats), .70*nrow(NBA_stats))
  Train_Index

  Train_Data = NBA_stats[TrainIndex, ]
  Test_Data = NBA_stats[-TrainIndex, ]
  
  ##Let's begin predicting our scores
  ##Our RMSE here is 0.056; This section is specifically for our Points Model
Predict_1 = predict(Points_Model, test.data)
  Predict_1
Rmse_1 = RMSE(predict1, test.data$PTS)
  Rmse_1

  ##Just like we expected, our RMSE is really low since our model was perfect based off our R^2 and
  ##adjusted R^2 score; This section is specifically for our Perfect Model
Predict_2 = predict(Perfect_Model, test.data)
  Predict_2
Rmse_2 = RMSE(Predict_2, test.data$PTS)
  Rmse_2
  
  ##Plotting our Prediction scores 
plot(Predict_1, test.data$PTS,xlab = "Prediction", ylab = "Test Values")
  abline(Points_Model)
plot(Predict_2, test.data$PTS,  xlab = "Prediction", ylab = "Test Values")
  abline(Points_Model)

    
  ##Training for our logistic model
Train_Prob=predict(Log_Points_Model, newdata = train.data, type="response") 
  Predicted_Classes = ifelse(TrainProb > 0.5, "1", "0") 
  table(Predicted=Predicted_Classes,Actual=train.data$ScoreValue) ##confusion matrix 
  mean(Predicted_Classes==train.data$ScoreValue)  ##accuracy rate 
  ##Testing for our logistic model
Test_Prob=predict(Log_Points_Model,newdata = test.data, type="response") 
  Predicted_Classes_Test = ifelse(TestProb > 0.5, "1" , "0") 
  table(Predicted=Predicted_Classes_Test,Actual=test.data$ScoreValue) ##confusion matrix 
  mean(Predicted_Classes_Test==test.data$ScoreValue)

  ##Confusion Matrix for Training on logistic model
cmat = table(Predicted=Predicted_Classes,Actual=train.data$ScoreValue)
  cfm = as_tibble(cmat)
  plot_confusion_matrix(cfm, 
                      target_col = "Actual",
                      prediction_col = "Predicted",
                      counts_col = "n",
                      palette = "Oranges")
  
  ##Are under curve for Training logistic model
Train_Roc <- roc(train.data$ScoreValue ~ TrainProb, data = train.data) 
  plot(Train_Roc, asp = "F")  
  auc(Train_Roc) 
  
  ##Confusion Matrix for Testing on logistic model
cmat_test = table(Predicted=Predicted_Classes_Test,Actual= test.data$ScoreValue)
  cfm_test = as_tibble(cmat_test)
  plot_confusion_matrix(cfm_test, 
                        target_col = "Actual",
                        prediction_col = "Predicted",
                        counts_col = "n",
                        palette = "Oranges")

  ##Area under curve for Testing logistic model
Test_Roc <- roc(test.data$ScoreValue ~ TestProb, data = test.data) 
  plot(Test_Roc, asp = "F")  
  auc(Test_Roc) 
  
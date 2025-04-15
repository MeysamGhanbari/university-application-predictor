##callege_project(final)---------------------------------------
##Goal: predict the number of applications received
##Required library

 library("moments")
 library("MASS")
 library("leaps")
 library("glmnet")
 library("tree")
 library("randomForest")
 library("gbm")
 library("xgboost")
 library("ggplot2")

##Set directory---------------------------------------------------

 getwd()  
 setwd("E:/Masters/Data Science/Data Science Course/Farzad minoyi_tosee/Data Sceince/project")
 
##Read Data from file---------------------------------------------
 
 college_data <- read.csv("college.csv")
 View(college_data)

##Data Understnding----------------------------------------------------
##Step 1: relevant  data?----------------------------------------------
##Step 2: data sources ?-----------------------------------------------
##Data set variables definition
 
 colnames(college_data)
 
#Convert categorical variables to factors
 
 college_data$Private <- factor(college_data$Private)
 summary(college_data)
 head(college_data)
 sum(is.na(college_data))
 
#Remove college's name
 college_data <- college_data[, -1]
 View(college_data)
 
##Continuous variables distribution
 dim(college_data)
 par(mar = c(1.5, 1.5, 1.5, 1.5))
 par(mfrow = c(4, 5))
 for (i in 2 : 18) {
      hist(college_data[, i], xlab = "", main = paste("Histogram of", names(college_data)[i]))
 }
 par(mfrow = c(1, 1))
 boxplot(college_data$Apps) 
 
##Correlation Analysis
 cor_table <- round(cor(college_data[,c(2 :18)]),2)
 View(cor_table)
 
 par(mfrow = c(4, 4))
 for (i in 3 : 18) {
      plot(college_data[,i], college_data$Apps, xlab = "", main = paste("Apps vs.", names(college_data)[i]))
 }
 par(mfrow = c(1, 1))
 
 
##Categorical variable 
 table(data2$Private)
 
##Divide Dataset into Train and Test----------------------------------
 set.seed(12345)
 train_cases <- sample(1 : nrow(data2), 0.7 * nrow(data2))
 train <- data2[train_cases, ] 
 test <- data2[-train_cases, ] 
 dim(train) 
 dim(test) 
 summary(train)
 summary(test)

##Bulding prediction model--------------------------------------------
##Model 1 : Tranditional Linear regression----------------------------
 m1 <- lm(Apps ~ ., data = train)
 summary(m1)   
 m1_1 <- lm(Apps ~ Private + Accept + Top10perc + Top25perc + P.Undergrad + Outstate +
              Room.Board + Expend + Grad.Rate, data = train)
 summary(m1_1)   
   
##Check Assumption of regression
#Normality of residuals
 hist(m1_1$residuals, probability = TRUE, breaks = 25)
 lines(density(m1_1$residuals), col = "red")   
 
##QQ_plot
 qqnorm(m1_1$residuals, main = "QQ_plot of residuals", pch = 20)
 qqline(m1_1$residuals, col = "red")   
 
##Test for skewness and kurtosis   
 moments :: jarque.test(m1_1$residuals)
            anscombe.test(m1_1$residuals)   

#Note: Residual are not Normally distributed!
            
##Diagnostic plots
 plot(m1_1)
 
##Check multicollinearity
 car :: vif(m1)
 car :: vif(m1_1)

##Test Model 1------------------------------------------------
#Model : m1_1
#prediction
 pred_m1_1 <- predict(m1_1, test)
 pred_m1_1 

##Absolute Error----------------------------------------------
 abs_err_m1_1 <- abs(pred_m1_1 - test$Apps)
 summary(abs_err_m1_1) 
  sd(abs_err_m1_1) 
 par(mar = c(4.5, 4, 4, 3) + 0.1)
 hist(abs_err_m1_1, breaks = 15) 
 boxplot(abs_err_m1_1) 
 
##Model 2 : Box_Cox Transformation----------------------------
 box_results <- boxcox(Apps ~ ., data = train, lambda = seq(-5, 5, 0.1))
 box_results <- data.frame(box_results$x, box_results$y)
 box_results
 lambda <- box_results[which(box_results$box_results.y == max(box_results$box_results.y)), 1] 
 lambda  

 boxcox_Apps <- (train$Apps ^ lambda - 1) / lambda
 train$boxcox_Apps <- boxcox_Apps 
 train 
 
 m2 <- lm(boxcox_Apps ~ ., data = train[, -which(names(train) == "Apps")])
 summary(m2) 
 m2_1 <- lm(boxcox_Apps ~ Private + Accept + Top10perc + P.Undergrad + Room.Board + Books +
              S.F.Ratio + Expend + Grad.Rate, data = train[-which(rownames(train) == 251|
                                                                  rownames(train) == 21|
                                                                  rownames(train) == 367|
                                                                  rownames(train) == 275|
                                                                  rownames(train) == 628),
                                                           -which(names(train) == "Apps")])
 summary(m2_1)
 
##Check assumption of regression
 hist(m2_1$residuals, probability = T, breaks = 25)
 lines(density(m2_1$residuals), col = "red") 
 qqnorm(m2_1$residuals, main = "QQ_plot of residuals", pch = 20) 
 qqline(m2_1$residuals, col = "red") 
 jarque.test(m2_1$residuals)
 anscombe.test(m2_1$residuals)  
 plot(m2_1)
 car :: vif(m2_1)
 
##Problem : Heteroscedasticity
 
##Test Model 2 -----------------------------------------------------------
#Model : m2_1
#prediction
 pred_m2_1 <- predict(m2_1, test[, -which(names(train) == "Apps")])
 pred_m2_1 
 
##Absolute Error----------------------------------------------
 abs_err_m2_1 <- abs(pred_m2_1 - test$Apps)
 summary(abs_err_m2_1) 
 sd(abs_err_m2_1) 
 hist(abs_err_m2_1, breaks = 15) 
 boxplot(abs_err_m2_1) 
 
 


   
 
 
 
 
 
 
 
 
 
  
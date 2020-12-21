library(forecast)
# Set directory of the project
setwd("C:/Users/stepa/Google Drive (stepan.houdek1@gmail.com)/VŠE/Pátý semestr/MIS 315.01 (Machine Learning for Managerial Decision Making)/Final_project")
?read.csv()
fifaData <- read.csv(file = 'Final_Project_Data_Set_2021.csv', row.names = 1)
# Drop names column
fifaData <- fifaData[,-1]
?read.csv()

# a) Drop gk players
fifaData <- fifaData[is.na(fifaData$gk),]


# Delete gk column
fifaData <- fifaData[,-ncol(fifaData)]

# Categorical variables to ordered factors
fifaData$work_rate_att <- factor(fifaData$work_rate_att, levels=c("Low","Medium","High"))
fifaData$work_rate_def <- factor(fifaData$work_rate_def, levels=c("Low","Medium","High"))
fifaData$preferred_foot <- factor(fifaData$preferred_foot)

str(fifaData$preferred_foot)
str(fifaData$work_rate_att)
str(fifaData$work_rate_def)


# b) Analyze your predictors/independent variables using techniques shown in the class. 5 points
for (i in 1:ncol(fifaData)){
  str(fifaData[,i])
}

# Calculate mean of all columns
for (i in 1:ncol(fifaData)) {
  if(is.na(mean(fifaData[,i]))){print(paste("Variable", i, "is categorical"))} else{
    print(paste(names(fifaData)[i],
                
                mean(fifaData[,i])))
  }
}


# c) Take the first 1000 observation as the training set and use the rest as the test set. 5 points
fifaTrain <- fifaData[1:1000,]
fifaTest <- fifaData[1001:nrow(fifaData),]

# colnames
#skill_moves+work_rate_att+work_rate_def+preferred_foot+crossing+finishing+heading_accuracy+short_passing+volleys+curve+free_kick_accuracy+long_passing+ball_control+acceleration+sprint_speed+agility+reactions+balance+shot_power+jumping+stamina+strength+long_shots+aggression+interceptions+positioning+penalties+composure+marking+standing_tackle+sliding_tackle
# d) Fit a multiple regression model to predict “eur_value”. Use only the training set to fit the regression model. 5 points
lm.fit=lm(eur_value~.,data=fifaTrain)

#e) Analyze your estimated regression models. Comment on coefficients, adjusted R square and F statistic of the model. 5 points
summary(lm.fit)
# There's 33 coeficients in the model. The columns 'ID' and 'name' are omited since they don't effect the eur_value. P-value is below 0.1, meaning that the model yields significant statistical results. R-squared is equal to 0.5407, that means that the model explains approximately 54 % of variability. 
# STILL NEED TO COMMENT ON F-STATISTIC

#f) Predict “eur_value” in the test set using the regression model obtained in (d). Calculate the mean square error of the test set (MSE). 5 pointsf) Predict “eur_value” in the test set using the regression model obtained in (d). Calculate the mean square error of the test set (MSE). 5 points
testPredict <- predict(lm.fit, newdata = fifaTest)

accuracy(testPredict, fifaTest$eur_value)
# RMSE is equal to 6804580. DOUBLE CHECK IS RMSE == MSE!

#g) Fit a Ridge model and a Lasso model to predict “eur_value”. Use only the training set to fit these regression models. Determine the lambda parameter using cross-validation. 5 points
# Ridge

x=model.matrix(eur_value~.,fifaTrain)[,-1]
y=fifaTrain$eur_value
library(glmnet)
grid <- 10^seq(10,-2, length=100)
ridge.mod <- glmnet(x,y,alpha=0, lambda=grid)

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test=y[test]
# Plotting the mean squared error and lambda
cv.out <- cv.glmnet(x[train,], y[train],aplha=0)

windows()
plot(cv.out)

best.lam <- cv.out$lambda.min
log(best.lam)



# Laso
laso.mod <- glmnet(x,y,alpha=1, lambda=grid)

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test=y[test]
# Plotting the mean squared error and lambda
cv.out <- cv.glmnet(x[train,], y[train],aplha=1)


windows()
plot(cv.out)

best.lam <- cv.out$lambda.min
log(best.lam)
laso.pred <- predict(laso.mod,s=best.lam, newx=x[test,])  
mean((laso.pred-y.test)^2)

#h) Analyze your Lasso Model. Compare your Lasso Model with the multiple regression model estimated in (d). 10 points


# i) Predict “eur_value” in the test set using the Ridge model and the Lasso model obtained in (g). Calculate MSEs of these models only using the test set. 5 points
ridge.pred <- predict(ridge.mod,s=best.lam, newx=x[test,])  
mean((ridge.pred-y.test)^2)

laso.pred <- predict(laso.mod,s=best.lam, newx=x[test,])  
mean((laso.pred-y.test)^2)



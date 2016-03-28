
# cross validation

'/
1. use the training set
2. split into training test set
3. build a model on the training set
4. evaluate on the test set
5. repeat and average the estimated errors

Used for:

1. picking variables to include in a model
2. picking type of prediction function to use
3. picking parameters in the prediction function
4. comparing different predictors


'

# K-fold cross validation
# split the data into k segments, build a test & training set for each segment (aka folds)

'/


'


# Leave one-out
# leave one observation out, predict based on other n-1 observations
# repeat, the next time leaving a different observation out
# etc. etc.




# Preprocess?

# standardize if there are several outliers
(trainCapAve - mean(trainCapAve))/sd(trainCapAve)

# must apply same standardization to the test set (use the mean and sd of the training set to "standardize" the test set)
preObj <- preProcess(training[,-58],method = c("center","scale"))
testCapAveS <- predict(preObj,testing[,-58])$capitalAve
modelFit <- train(type ~.,data = training,preProcess = c("center","scale"),method = "glm")

# Box-Cox transforms
# to convert continuous data to normal data




# Principle Component Analysis





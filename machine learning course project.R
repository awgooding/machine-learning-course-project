


# Startup Config ----------------------------------------------------------

library(reshape2)
library(ggplot2)
library(caret)
library(plyr)
library(dplyr)
library(GGally)

# Load Data ---------------------------------------------------------------

# read training data
training <- read.csv("input/pml-training.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE,
                     na.strings = c("NA","#DIV/0!"))


# Exploratory Analysis ----------------------------------------------------

# plot distribution of each predictor


# should you split the data by exercise? (same exercise was used: Unilateral Dumbbell Biceps Curl)  by subject? (no)

#d.mean <- apply(training,2,mean,na.rm = TRUE)
#d.means <- colMeans(training[,-160],na.rm = TRUE)

num.col <- data.frame(sapply(training,is.numeric))
names(num.col) <- "is.numeric.data"

names.change.num <- c("X",
                   "raw_timestamp_part_1",
                   "raw_timestamp_part_2",
                   "new_window",
                   "num_window",
                   "classe")
num.col[names.change.num,] <- FALSE
names.data <- rownames(num.col)[num.col$is.numeric.data == TRUE]
names.id.data <- rownames(num.col)[num.col$is.numeric.data == FALSE]

# remove columns with little variation
nsv <- nearZeroVar(training[names.data],saveMetrics = TRUE)
zero.var <- rownames(nsv)[nsv$zeroVar == TRUE]
near.zero.var <- rownames(nsv)[nsv$nzv == TRUE]


names.data.use <- names.data[!(names.data %in% near.zero.var)]


# retaining means and sd for later use
training.mean <- sapply(training[names.data.use],mean,na.rm = TRUE)
training.sd <- sapply(training[names.data.use],sd,na.rm = TRUE)


# centering the data
preObj <- preProcess(training[names.data.use],method = c("center","scale"))
training.centered <- predict(preObj,training[names.data.use])
training.centered <- cbind(training[names.id.data],training.centered)



    
d.melt <- melt(
    training,
    id.vars = c(
        "X",
        "user_name",
        "raw_timestamp_part_1",
        "raw_timestamp_part_2",
        "cvtd_timestamp",
        "new_window",
        "num_window"
    )
)



# plot the distribution

ggplot(d.melt,aes(x = value)) + facet_wrap(~variable,scales = "free_x") + geom_histogram()

# It appears as though standardizing the data would be useful in limiting the affect of outliers in some variables


# Should we use Principal Components Analysis?



# Feature Extraction ----------------------------------


# principal componenets analysis, good if there are lots of correlated predictors

# checking for correlated predictors
M <- abs(cor(training.centered[names.data.use]))
diag(M) <- 0
correlated.predictors <- which(M > 0.9,arr.ind = TRUE)
nrow(correlated.predictors)

# plot correlated columns against each other to verify that they are highly correlated
data.correl <- data.frame(cor(training.centered[,unique(rownames(correlated.predictors))]))
data.correl$var1 <- rownames(data.correl)
data.correl <- melt(data.correl,id.vars = "var1",variable.name = "var2",value.name = "correlation")

g.correl <- ggplot(data = data.correl,
                   aes(x = var1,y = var2,size = abs(correlation),
                       color = correlation)) + 
    geom_point() +
    theme(axis.text.x = element_text(angle = 90,hjust = 0))


# we want to find a new set of multivariate variables that are uncorrelated and explain as much variance as possible
# put all the variables in one matrix, find the best matric with a lower rank (fewer variables) that explains the original data


# pca
# most useful for linear type models...watch out for outliers! may require transformation (logs, Box Cox)
# plot predictors to identify problems

# method 1
preProc <- preProcess(training.centered[,names.data.use],
                      method = "pca",
                      thresh = .95)
preProc$numComp

training.centered.pca <- predict(preProc,training.centered[,names.data.use])




# check number of principal components to compute?
spamPC <- predict(preProc,spam[,-58])
plot(spamPC[,1],spamPC[,2])

trainPC <- predict(preProc,training[,-58])


modelFit <- train(training$type, ~ ., method = "glm",data = trainPC)
# have to use originally calculated PC to predict in test set
testPC <- predict(preProc,testing[,-58])
confusionMatrix(testing$type,predict(modelFit,testPC))

# method 2
modelFit <- train(training$type ~ .,
                  method = "glm",
                  preProcess = "pca",
                  data = training)

confusionMatrix(testing$type,
                predict(modelFit,testing))



# Random Forests --------------------------------------

# bootstrap samples
# at each split, bootstrap variables
# care should be taken to avoid overfitting (rfcv function)

# pros: accuracy
# cons: speed, interpretability, overfitting


modFit <- train(Species ~.,data = training,method = "rf",prox = TRUE)

# to view a specific tree
getTree(modFit$finalModel,k = 2)

# to check if you are right

pred <- predict(modFit,testing)
testing$predRight <- pred==testing$Species
table(pred,testing$Species)

qplot(Petal.Width,Petal.Length, colour = predRight, data = testing)
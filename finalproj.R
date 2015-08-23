#Load necessary libraries 
library(caret)
library(data.table)
library(ggplot2)
library(randomForest)

# set seed for consistency
set.seed(3086)

# load testing and training data sets
setInternet2(TRUE)
trainURL <- https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
testURL <- https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

training <- fread(trainURL)
testing <- fread(trestURL)

# Find columns with any missing values or NAs
anyMissing <- sapply(training, function(x) any(is.na(x) | x == ""))

# Find predictor variables -- Belt, arm, forearm and dumbbell vars.
anyPredictors <- !anyMissing & grepl("belt| [(^fore)]arm | dumbbell | forearm", 
                                     names(anyMissing))

# Create vector to include all predictor candidates
predictionVars <- names(anyMissing)[anyPredictors]

# subset the data sets into prediction variables and classe 
training <- training[, c("clasee", predictionVars), with = FALSE]
testing <- testing[, c("classe", predictionVars), with = FALSE]

# Create 70% training partition and 30% training partition for the model
indice <- createDataPartition(y=training$classe, p=0.70, list=FALSE)
trainModel <- training[indice, ]
probeModel <- training[-indice, ]

# Train model with random forests and 7-fold cross validation 
modRandomForest <- train(classe~ .,data=trainModel,method='rf'
                trControl=trainControl(method="cv",number=7)
                prox=TRUE,allowParallel=TRUE)

# Display model
print(modRandomForest)

#Display sample error 
print(modRandomForest$finalModel)

# Find erorrs on training and test data through predictions 

predictTrain <- predict(modRandomForest, newdata=trainModel)
confusionMatrix(predictTrain,trainModel$classe)

predictTest <- predict(modRandomForest, newdata=testModel)
confusionMatrix(predictTest,probeModel$classe)

# Write files to text files to for testing, COURSERA Code 
setwd("C:/Users/Rakesh Chatrath/Workspace/Coursera/practmachinelearn")
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
answers <- predict(modRandomForest,newdata=testing)
pml_write_files(answers)

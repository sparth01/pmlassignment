# set working directory
setwd("~/Desktop/ml/courserapml")

# install required packages
install.packages("caret")
install.packages("sqldf")
install.packages("plyr")
install.packages("dplyr")
library(caret)
library(sqldf)
library(plyr)
library(dplyr)

# import training data
wlatraindata1 = read.csv("pml-training.csv", header = TRUE)

# create two partitions of training and test data set split by classe variable
inTrain <- createDataPartition(y=wlatraindata1$classe, p=0.75, list=FALSE)

wlatrain <- wlatraindata1[inTrain,]
wlatraintest <- wlatraindata1[-inTrain,]

#remove columns where value is na more than 500 times
wlatrain1 <- wlatrain %>% select_if(~sum(!is.na(.)) > 500)
# write the output to a temp. file
write.csv(wlatrain1, file="pml_training_clean.csv")
# read the file again by setting na to column values that are blank
wlatrain2 <- read.csv("pml_training_clean.csv",na.strings = "", header=TRUE)

#again remove columns where value is na more than 500 times
wlatrain3 <- wlatrain2 %>% select_if(~sum(!is.na(.)) > 500)
write.csv(wlatrain3, file="pml_training_clean3.csv")

#set seed
set.seed(333)
#select columns that matter after profiling. Initial set of columns contain sequence numbers, 
#name and timestamp that will not help with prediction

wlatrain4 <- wlatrain3[,8:61]

#create crossvalidation training control
fitControl <- trainControl(## 10-fold CV
  method = "cv",number = 10)

# select carefully the right columns after exploring data from belt, arm, dumbbell, forearm sensors
wlatrain5 <- wlatrain4[,c("roll_belt","pitch_belt", "yaw_belt", "total_accel_belt", 
                          "roll_arm","pitch_arm","yaw_arm","total_accel_arm", "roll_dumbbell",
                          "pitch_dumbbell","yaw_dumbbell", "roll_forearm", "pitch_forearm", 
                          "yaw_forearm", "classe")]

#Try out different models and select the right model
# In this case, I have used gradient boosting machine model using the training set and 
# applying crossvalidation
gbmFit1 <- train(classe ~ ., data = wlatrain5, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

# create a similar dataset from training

wlatraintest1 <- wlatraintest[,c("roll_belt","pitch_belt", "yaw_belt", "total_accel_belt", 
                       "roll_arm","pitch_arm","yaw_arm","total_accel_arm", 
                       "roll_dumbbell","pitch_dumbbell","yaw_dumbbell", "roll_forearm", 
                       "pitch_forearm", "yaw_forearm")]

# predict
predict(gbmFit1, newdata = wlatraintest1)

# create a second dataset to add the predicted value and compare with actuals
wlatraintest2 <- wlatraintest[,c("X","roll_belt","pitch_belt", "yaw_belt", 
                                 "total_accel_belt", 
                                 "roll_arm","pitch_arm","yaw_arm","total_accel_arm", 
                                 "roll_dumbbell","pitch_dumbbell","yaw_dumbbell", 
                                 "roll_forearm",
                                 "pitch_forearm", "yaw_forearm","classe")]

# create one data set that has both predicted and actual value
wlatraintest3 <- cbind(wlatraintest2, predict(gbmFit1, newdata=wlatraintest1))

# write the output, rename the column and write again
write.csv(wlatraintest3, file="pml_traintest3.csv")

colnames(wlatraintest3)[17] <- "predicted_classe"

write.csv(wlatraintest3, file="pml_traintest3.csv")

# plot the output
plot(wlatraintest3$predicted_classe,wlatraintest3$classe,xlab=predicted,ylab=actual)

# check the out of sample error with confusion matrix. Accuracy was 0.937
confusionMatrix(wlatraintest3$predicted_classe, wlatraintest3$classe)

# plot the output
plot(wlatraintest3$predicted_classe,wlatraintest3$classe,xlab="predicted",ylab="actual")

# read testing file
wlatest <- read.csv(file="pml-testing.csv", header=TRUE)

wlatest1 <- wlatest[,c("roll_belt","pitch_belt", "yaw_belt", 
                                 "total_accel_belt", 
                                 "roll_arm","pitch_arm","yaw_arm","total_accel_arm", 
                                 "roll_dumbbell","pitch_dumbbell","yaw_dumbbell", 
                                 "roll_forearm",
                                 "pitch_forearm", "yaw_forearm")]

#predict the classe variable and write back as one file
wlatest1predicted <- cbind(wlatest1, predict(gbmFit1,newdata=wlatest1))

#write test results now
write.csv(wlatest1predicted,"pml-testing-result.csv")

setwd("~/Desktop/ml/courserapml")

install.packages("caret")
install.packages("sqldf")
install.packages("plyr")
install.packages("dplyr")
library(caret)
library(sqldf)
library(plyr)
library(dplyr)


wlatraindata1 = read.csv("pml-training.csv", header = TRUE)

inTrain <- createDataPartition(y=wlatraindata1$classe, p=0.75, list=FALSE)

wlatrain <- wlatraindata1[inTrain,]
wlatraintest <- wlatraindata1[-inTrain,]


#wlatrain1 <- wlatrain %>% select_if(~sum(!is.na(.)) > 0)
wlatrain1 <- wlatrain %>% select_if(~sum(!is.na(.)) > 500)
write.csv(wlatrain1, file="pml_training_clean.csv")
wlatrain2 <- read.csv("pml_training_clean.csv",na.strings = "", header=TRUE)

wlatrain3 <- wlatrain2 %>% select_if(~sum(!is.na(.)) > 500)
write.csv(wlatrain3, file="pml_training_clean3.csv")

set.seed(333)
wlatrain4 <- wlatrain3[,8:61]

fitControl <- trainControl(## 10-fold CV
  method = "cv",number = 10)

wlatrain5 <- wlatrain4[,c("roll_belt","pitch_belt", "yaw_belt", "total_accel_belt", 
                          "roll_arm","pitch_arm","yaw_arm","total_accel_arm", "roll_dumbbell",
                          "pitch_dumbbell","yaw_dumbbell", "roll_forearm", "pitch_forearm", 
                          "yaw_forearm", "classe")]

gbmFit1 <- train(classe ~ ., data = wlatrain5, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)


wlatraintest1 <- wlatraintest[,c("roll_belt","pitch_belt", "yaw_belt", "total_accel_belt", 
                       "roll_arm","pitch_arm","yaw_arm","total_accel_arm", 
                       "roll_dumbbell","pitch_dumbbell","yaw_dumbbell", "roll_forearm", 
                       "pitch_forearm", "yaw_forearm")]

predict(gbmFit1, newdata = wlatraintest1)

wlatraintest2 <- wlatraintest[,c("X","roll_belt","pitch_belt", "yaw_belt", 
                                 "total_accel_belt", 
                                 "roll_arm","pitch_arm","yaw_arm","total_accel_arm", 
                                 "roll_dumbbell","pitch_dumbbell","yaw_dumbbell", 
                                 "roll_forearm",
                                 "pitch_forearm", "yaw_forearm","classe")]

wlatraintest3 <- cbind(wlatraintest2, predict(gbmFit1, newdata=wlatraintest1))

write.csv(wlatraintest3, file="pml_traintest3.csv")

colnames(wlatraintest3)[17] <- "predicted_classe"

write.csv(wlatraintest3, file="pml_traintest3.csv")

plot(wlatraintest3$predicted_classe,wlatraintest3$classe,xlab=predicted,ylab=actual)

confusionMatrix(wlatraintest3$predicted_classe, wlatraintest3$classe)

plot(wlatraintest3$predicted_classe,wlatraintest3$classe,xlab="predicted",ylab="actual")




####Predict building


set.seed(123)#random seed
fitControl <- trainControl(method = "repeatedcv", 
                           verboseIter = TRUE, allowParallel = TRUE)


## knn
knnFit <- caret::train(BUILDINGID~Max,
                data = train,
                method = "knn",
                metric = "Accuracy",
                tuneGrid = expand.grid(k=1:5),
                trControl = fitControl)
              



plot(knnFit)
predict.knn <- predict(knnFit, validation)
postResample(predict.knn , validation$BUILDINGID)
confusionMatrix(validation$BUILDINGID , predict.knn)
save(knnFit, file = "knnFit.rda")



## SVM
svmFit <- caret::train(BUILDINGID~Max,
                            data = train,
                            method = "svmLinear",
                            metric = "Accuracy",
                            trControl = fitControl)

plot(svmFit)
predict.svm <- predict(svmFit , validation)
postResample(predict.svm , validation$BUILDINGID)
confusionMatrix(validation$BUILDINGID , predict.svm)
save(svmFit, file = "svmFit.rda")

validation.pred <- validation
validation.pred$Build.pred <- predict.knn

############## Data sets by building ###########

# Building 0

building.0.val <- filter(validation,BUILDINGID %in% "0")

building.0.tr <- filter(train,BUILDINGID %in% "0")

#type of FLOOR variable numeric/factor
building.0.val$FLOOR <- as.numeric(building.0.val$FLOOR)
building.0.tr$FLOOR <- as.numeric(building.0.tr$FLOOR)

building.0.val$FLOOR <- as.factor(building.0.val$FLOOR)
building.0.tr$FLOOR <- as.factor(building.0.tr$FLOOR)

#type of BUILDING variable numeric/factor
building.0.val$BUILDINGID <- as.numeric(building.0.val$BUILDINGID)
building.0.tr$BUILDINGID <- as.numeric(building.0.tr$BUILDINGID)

building.0.val$BUILDINGID <- as.factor(building.0.val$BUILDINGID)
building.0.tr$BUILDINGID <- as.factor(building.0.tr$BUILDINGID)

# Building 1

building.1.val <- filter(validation,BUILDINGID %in% "1")

building.1.tr <- filter(train,BUILDINGID %in% "1")

#type of FLOOR variable numeric/factor
building.1.val$FLOOR <- as.numeric(building.1.val$FLOOR)
building.1.tr$FLOOR <- as.numeric(building.1.tr$FLOOR)

building.1.val$FLOOR <- as.factor(building.1.val$FLOOR)
building.1.tr$FLOOR <- as.factor(building.1.tr$FLOOR)

#type of BUILDING variable numeric/factor
building.1.val$BUILDINGID <- as.numeric(building.1.val$BUILDINGID)
building.1.tr$BUILDINGID <- as.numeric(building.1.tr$BUILDINGID)

building.1.val$BUILDINGID <- as.factor(building.1.val$BUILDINGID)
building.1.tr$BUILDINGID <- as.factor(building.1.tr$BUILDINGID)


# Building 2

building.2.val <- filter(validation,BUILDINGID %in% "2")

building.2.tr <- filter(train,BUILDINGID %in% "2")

#type of FLOOR variable numeric/factor
building.2.val$FLOOR <- as.numeric(building.2.val$FLOOR)
building.2.tr$FLOOR <- as.numeric(building.2.tr$FLOOR)

building.2.val$FLOOR <- as.factor(building.2.val$FLOOR)
building.2.tr$FLOOR <- as.factor(building.2.tr$FLOOR)

#type of BUILDING variable numeric/factor
building.2.val$BUILDINGID <- as.numeric(building.2.val$BUILDINGID)
building.2.tr$BUILDINGID <- as.numeric(building.2.tr$BUILDINGID)

building.2.val$BUILDINGID <- as.factor(building.2.val$BUILDINGID)
building.2.tr$BUILDINGID <- as.factor(building.2.tr$BUILDINGID)


####Predict floor in building 0
#knn

knn.floor0 <- caret::train(FLOOR~Max,
                       data = building.0.tr,
                       method = "knn",
                       metric = "Accuracy",
                       tuneGrid = expand.grid(k=1:5),
                       trControl = fitControl)

plot(knn.floor0)
predict.knn.floor0 <- predict(knn.floor0, building.0.val)
postResample(predict.knn.floor0 , building.0.val$FLOOR)
confusionMatrix(building.0.val$FLOOR , predict.knn.floor0)
save(knn.floor0, file = "knn.floor0.rda")

#SVM
svmFit.0 <- caret::train(FLOOR~Max,
                         data = building.0.tr,
                         method = "svmLinear",
                         metric = "Accuracy",
                         trControl = fitControl)

plot(svmFit.0)
predict.svm.f0 <- predict(svmFit.0 , building.0.val)
postResample(predict.svm.f0 , building.0.val$FLOOR)
confusionMatrix(building.0.val$FLOOR , predict.svm.f0)
save(svmFit.0, file = "svmFit.0.rda")

building.0.val$Floor_pred <- predict.svm.f0

#Predict latitud for building 0
rf.Lat.0 <- train(LATITUDE~ Max + FLOOR, 
               data = building.0.tr, method="rf", trControl=fitControl, 
               tuneLength=20)

save(rf.Lat.0, file = "rf.Lat.0.rda")

rfclass.lat.0 <- predict(rf.Lat.0, newdata = building.0.val)

rfclass.lat.0

####Predict floor in building 1

#SVM
svmFit.1 <- caret::train(FLOOR~Max,
                       data = building.1.tr,
                       method = "svmLinear",
                       metric = "Accuracy",
                       trControl = fitControl)

plot(svmFit.1)
predict.svm.f1 <- predict(svmFit.1 , building.1.val)
postResample(predict.svm.f1 , building.1.val$FLOOR)
confusionMatrix(building.1.val$FLOOR , predict.svm.f1)
save(svmFit.1, file = "svmFit.1.rda")

building.1.val$Floor_pred <- predict.svm.f1

#Predict latitud for building 1
rf.Lat.1 <- train(LATITUDE~ Max + FLOOR, 
                  data = building.1.tr, method="rf", trControl=fitControl, 
                  tuneLength=4)

save(rf.Lat.1, file = "rf.Lat.1.rda")

rfclass.lat.1 <- predict(rf.Lat.1, newdata = building.1.val)

rfclass.lat.1

rf.Lat.1

####Predict floor in building 2
#KNN

knn.floor2 <- caret::train(FLOOR~Max,
                           data = building.2.tr,
                           method = "knn",
                           metric = "Accuracy",
                           tuneGrid = expand.grid(k=1:5),
                           trControl = fitControl)

plot(knn.floor2)

predict.knn.floor2 <- predict(knn.floor2, building.2.val)
postResample(predict.knn.floor2 , building.2.val$FLOOR)
confusionMatrix(building.2.val$FLOOR , predict.knn.floor2)
save(knn.floor2, file = "knn.floor2.rda")

#SVM
svmFit.2 <- caret::train(FLOOR~Max,
                         data = building.2.tr,
                         method = "svmLinear",
                         metric = "Accuracy",
                         trControl = fitControl)

plot(svmFit.2)
predict.svm.f2 <- predict(svmFit.2 , building.2.val)
postResample(predict.svm.f2 , building.2.val$FLOOR)
confusionMatrix(building.2.val$FLOOR , predict.svm.f2)
save(svmFit.2, file = "svmFit.2.rda")

building.2.val$Floor_pred <- predict.svm.f2
# Deep Analytics and Visualization - Task 3 - Evaluate Techniques for Wifi Locationing
# Done by Lucas Dantas de Araujo

pacman::p_load(readr, dplyr, lubridate, 
               parallel, doParallel, ggmap, 
               scatterplot3d, caTools, caret, parallel)

setwd("D:/Lucas/UBIQUM/Modulo_3/WiFi")

# Parallel processing

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)



#### * Data import validation  #####
validation <- read_csv("validationData.csv", na = c("N/A"))
validation <- select(validation, -c(SPACEID,RELATIVEPOSITION,USERID,PHONEID,TIMESTAMP))

#### * Data import training ####

train <- read_csv("trainingData.csv", na = c("N/A"))
train <- select(train, -c(SPACEID,RELATIVEPOSITION,USERID,PHONEID,TIMESTAMP))



#Type attribute changing 
train$BUILDINGID <- as.factor(train$BUILDINGID)
train$FLOOR <- as.factor(train$FLOOR)

#Type attribute changing 
validation$BUILDINGID <- as.factor(validation$BUILDINGID)
validation$FLOOR <- as.factor(validation$FLOOR)

#remove problematic WAPS
#TRAINING
train$WAP268 <- NULL
train$WAP323 <- NULL

#VALIDATION
validation$WAP268 <- NULL
validation$WAP323 <- NULL

#training dataset remove 1 value columns
uniquelength <- sapply(train, function(x) length(unique(x)))
train <- subset(train, select=uniquelength>1)

#validation  dataset remove 1 value columns
uniquelength <- sapply(validation,function(x) length(unique(x)))
validation <- subset(validation, select=uniquelength>1)

#identifying WAPS training
Waps_tr <- grep("WAP", names(train), value = TRUE)

#identifying WAPS VALIDATION
Waps_VD <- grep("WAP", names(validation), value = TRUE)

#Identifying common Waps
Waps_rd <- intersect(Waps_tr, Waps_VD)

#Are the Common waps in the training dataset? 
x <- names(train[Waps_tr]) %in% Waps_rd == FALSE

#keep the common Waps
train <- train[-which(x)]

#Are the Common waps in the training dataset? 
y <- names(validation[Waps_VD]) %in% Waps_rd == FALSE

#keep the common Waps
validation <- validation[-which(y)]



#### Change 100 to -105####
#train dataset
train[Waps_rd][ train[Waps_rd] == 100 ] <- -105

#validation dataset
validation[Waps_rd][ validation[Waps_rd] == 100 ] <- -105



#Maximun IRRS per instance
#training
Maximum.tr <- apply(train[Waps_rd], 1, function(x) names(which.max(x)))
train$Max <- Maximum.tr

#validation
Maximum.vd <- apply(validation[Waps_rd], 1, function(x) names(which.max(x)))
validation$Max <- Maximum.vd

#test


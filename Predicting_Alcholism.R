---
  title: "Predicting Alcholism"
output: html_document
---
  


#Read & Clean Data

alc <- read.csv("spor.csv")
str(alc)

alc$alc <- as.factor(alc$alc)
str(alc)

#Check for NAs

sapply(alc, function(x) sum(is.na(x)))





#Randomize and separate into test and train data

library(caret)
set.seed(1)
trainIndex <- createDataPartition(alc$alc, p=0.9, list = FALSE)

#Create test and train data
alc_train <- alc[trainIndex,]
alc_test <- alc[-trainIndex,]

#Check to ensure they are the same length
bool <- length(alc_train) == length(alc_test)
bool



##KSVM Model

#kvsm model - vanilladot

library(kernlab)

alc_classifier <- ksvm(alc ~., data = alc_train, kernel = "vanilladot")
alc_classifier

alc_predictions <- predict(alc_classifier, alc_test)
head(alc_predictions)
table(alc_predictions, alc_test$alc)

agreement <- alc_predictions == alc_test$alc
table(agreement)
prop.table(table(agreement))

#False = 0.296875, True = 0.701325


#kvsm - rbfdot

alc_classifier_rbf <- ksvm(alc ~., data = alc_train, kernel = "rbfdot")

alc_predictions_rbf <- predict(alc_classifier_rbf, alc_test)

agreement_rbf <- alc_predictions_rbf == alc_test$alc
table(agreement_rbf)
prop.table(table(agreement_rbf))

#False = 0.296875, True = 0.703125. Same as Vanilladot. 


#ANN Model

#Normalize
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

#Turn factors into integers
alc2 <- as.data.frame(model.matrix(~.-1,data=alc))
ResultCol <- alc2$alc1
alc2$alc1 <- NULL
alc2 <- cbind(alc2, ResultCol)

#normalize data
alc_n <- as.data.frame(lapply(alc2,normalize))
library(neuralnet)

alc_n_train <- alc2[trainIndex,]
alc_n_test <- alc2[-trainIndex,]

a <- as.formula(paste('ResultCol~',paste(colnames(alc_n_train[1:18]),collapse = '+')))

ann_model <- neuralnet(formula=a,data=alc_n_train,hidden = 5)
model_results <- compute(ann_model,alc_n_test[1:18])
predicted_alc <- model_results$net.result
cor(predicted_alc,alc_n_test$ResultCol)

#Correlation = 0.28, ANN has low accuracy


##KNN model


alcohol <- alc$alc
alc$alc <- NULL
alc <- cbind(alc,alcohol)

alc_n1 <- alc2[1:18]

alc_n1_train <- alc_n1[trainIndex,]
alc_n1_test <- alc_n1[-trainIndex,]
alc_n1_train_labels <- alc[trainIndex,18]
alc_n1_test_labels <- alc[-trainIndex,18]

library(class)
library(gmodels)

knn_pred <- knn(train = alc_n1_train,test = alc_n1_test,cl=alc_n1_train_labels,k=25)

CrossTable(x=alc_n1_test_labels,y=knn_pred,prop.chisq = FALSE)

accuracy <- (3 + 33) / (3 + 33 + 21 + 7)
accuracy
#.5625



##Regression model

library(lmtest)

alc_model <- glm(alcohol ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + studytime + activities + romantic + famrel  + freetime + goout + health + absences + grades, data = alc,family = binomial)
summary(alc_model)

alc_model2 <- glm(alcohol ~ sex + famsize + Pstatus + studytime + famrel  + goout + health + absences  , data = alc,family = binomial)
summary(alc_model2)

lm_prediction <- ifelse(predict(alc_model2,newdata = alc[1:17]) >0.5,1,0)
summary(alc$alcohol == lm_prediction)

alc_model3 <- glm(alcohol ~ sex + famsize + Pstatus + studytime + famrel  + goout + health + absences + goout:absences, data = alc,family = binomial)
summary(alc_model3)


##Combined model KNN

predictions <- as.data.frame <- NA
predictions$ksvm <- predict(alc_classifier,alc)
predictions <- as.data.frame(predictions$ksvm)
predictions$knn <- knn(train = alc_n1_train, test = alc_n1, cl=alc_n1_train_labels, k=25)
predictions$lm <- predict(alc_model3, newdata=alc[1:17])
predictions$lm  <- normalize(predictions$lm)

predictions$`predictions$ksvm` <- as.integer(as.character(predictions$`predictions$ksvm`))
predictions$knn <- as.integer(as.character(predictions$knn))

p_train <- predictions[trainIndex,]
p_test <- predictions[-trainIndex,]

p_train_labels <- alc[trainIndex,18]
p_test_labels <- alc[-trainIndex,18]

p_pred <- knn(train = p_train, test = p_test, cl = p_train_labels, k = 20)

CrossTable(x=p_test_labels, y=p_pred, prop.chisq = FALSE)

confusionMatrix(p_pred, p_test_labels, positive = "1")
# 0.77 Accuracy


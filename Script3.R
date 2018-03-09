library(nnet)
require(nnet)
library(randomForest)
library(Metrics)
library(glmnet)
library(tibble)
library(dplyr)
gc()
str(train)

############Splitting
sample_rows<-sample(nrow(train),nrow(train)*0.75)
testing<-train[-sample_rows,]
training<-train[sample_rows,]
###########Building the regression model

train$TripType2 <- relevel(train$TripType, ref = "999")
mod <- multinom(TripType2 ~ .-VisitNumber -TripType, data = train)
summary(mod)
testing$TripType2<-NULL
View(testing)

############### Random forest
model_rf<-randomForest(TripType~.-VisitNumber,training)
model_rf$importance
testing$pred<-predict(model_rf,testing)
mean(testing$TripType==testing$pred)
#ACCURACY = 68%
View(training)


############# lasso
x <- train[,-c(1,2,3)]
y <- train[,3] # Only class

# Fitting the model (Lasso: Alpha = 1)
set.seed(999)
cv.lasso <- cv.glmnet(x, y, family='multinomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
plot(cv.lasso)
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, s=cv.lasso$lambda.min)
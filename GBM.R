library(gbm)
library(Metrics)
library(pROC)

training_gbm<-training
testing_gbm<-testing
class(training_gbm$TripType)
gbm_mod <- gbm(formula = TripType ~.-VisitNumber,data = training,
                    distribution = "multinomial", 
                    n.trees = 200)
summary(gbm_mod)

gbm_pred <- predict(gbm_mod,n.trees=200, newdata=testing,type='response')
gbm_pred_best <- apply(gbm_pred, 1, which.max)
length(gbm_pred_best)
mean(gbm_pred_best==as.numeric(testing$TripType)) #accuracy=48.47%
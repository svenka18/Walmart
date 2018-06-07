library(randomForest)
library(caret)
library(e1071)
library(pROC)
############Splitting
sample_rows<-sample(nrow(train_c),nrow(train_c)*0.75)
testing<-train_c[-sample_rows,]
training<-train_c[sample_rows,]
names(testing)
###########Building model

mod<-randomForest(TripType~.-VisitNumber,training) #oob-error=31.61%
View(mod$importance)
print(mod)
pred<-predict(mod,testing[,-1],type="class")

#Confusion matrix
cm<-confusionMatrix(pred,testing$TripType)
#accuracy
mean(pred==testing$TripType) #accuracy=68.07%
plot(mod)
#oob error
err<-mod$err.rate
#oob accuracy
1-err[nrow(err),"OOB"] #accuracy=68.1%. both values are almost same
#auc
str(testing)
pred_prob<-predict(mod,testing[,-1],type ='prob')
auc(testing$TripType,pred_prob)  #default
multiclass.roc(testing$TripType,pred_prob)

###########Grid 
#lets split the data into 3 partitions : train, validation, test
s<-sample(1:3,size=nrow(training),prob = c(0.7,0.15,0.15),replace = TRUE)
training_grid<-training[s==1,]
validation_grid<-training[s==2,]
testing_grid<-training[s==3,]

# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(training_grid) * 0.2, 4)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(training_grid) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

#create an empty list to store all models
models<-list()

#execute gri seacrh
for(i in 1:nrow(hyper_grid)){
  #train the model
  models[[i]] <- randomForest(formula = TripType ~ .-VisitNumber, 
                        data = training_grid,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  }


#evaluate models based on accuracy
#create emty vector to store accuracy values
accuracy<-c()

for(i in 1:length(models)){
  model<-models[[i]]
  pred<-predict(model,validation_grid)
  accuracy[i]<-mean(pred==validation_grid$TripType)
}
  
best_accuracy<-which.min(accuracy) #67.2%


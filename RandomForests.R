library(randomForest)
library(caret)

############Splitting
sample_rows<-sample(nrow(train),nrow(train)*0.75)
testing<-train[-sample_rows,]
training<-train[sample_rows,]
names(training)
###########Building model

mod<-randomForest(TripType~.-VisitNumber,training) #oob-error=31.61%
mod$importance
testing$pred<-predict(mod,testing,type="class")
View(testing)
str(testing)
testing$pred<-as.factor(testing$pred)
confusionMatrix(testing$pred,testing$TripType)
mean(testing$pred==testing$TripType) #accuracy=68.23%
plot(mod)

###########Grid 
#lets split the data into 3 partitions : train, validation, test
s<-sample(1:3,size=nrow(train),prob = c(0.7,0.15,0.15),replace = TRUE)
training1<-train[s==1,]
validation<-train[s==2,]
testing1<-train[s==3,]

# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(training1) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(training1) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

#create an empty list to store all models
models<-list()

#execute gri seacrh
for(i in 1:nrow(hyper_grid)){
  #train the model
  models[[i]] <- randomForest(formula = TripType ~ .-VisitNumber, 
                        data = training1,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  }


#evaluate models based on accuracy
#create emty vector to store accuracy values
accuracy<-c()

for(i in 1:length(models)){
  model<-models[[i]]
  pred<-predict(model,validation)
  accuracy[i]<-mean(pred==validation$TripType)
}
  
best_model<-models[[]]
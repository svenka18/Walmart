library(dummies)
library(randomForest)
library(caret)
library(e1071)
library(pROC)
names(testing)
#Splitting
sample_rows<-sample(nrow(train_c),nrow(train_c)*0.75)
testing<-train_c[-sample_rows,]
training<-train_c[sample_rows,]

#PCA should not be done with target and identifiers
train_pca<-training[,-c(1,2)]
test_pca<-testing[,-c(1,2)]
#PCA requires all variables to be numeric
str(train_pca)
dim(train_pca)
View(train_pca)
#convert categorical variables into numeric using one hot encoding
#create a dummy data frame
new_train_pca <- dummy.data.frame(train_pca, names = "Weekday")
new_test_pca <- dummy.data.frame(test_pca, names = "Weekday")
str(new_train_pca)
dim(new_train_pca)
#principal component analysis
prin_comp <- prcomp(new_train_pca, scale. = T)
names(prin_comp)

#outputs the mean of variables
prin_comp$center
#outputs the standard deviation of variables
prin_comp$scale
prin_comp$rotation

#Let’s look at first 4 principal components and first 5 rows.
prin_comp$rotation[1:5,1:4]
#don’t need to multiply the loading with data. the matrix x has the principal component score vectors 
dim(prin_comp$x)
View(prin_comp$x)
#compute standard deviation of each principal component
std_dev <- prin_comp$sdev
#compute variance
pr_var <- std_dev^2
#check variance of first 10 components
pr_var[1:10]

#To compute the proportion of variance explained by each component, 
#we simply divide the variance by sum of total variance.
#proportion of variance explained
prop_var_exp <- pr_var/sum(pr_var)
prop_var_exp[1:20]

#scree plot
plot(prop_var_exp, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
#cumulative scree plot
plot(cumsum(prop_var_exp), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
#It requires 60 PC's to explain almost 98% variance in data.
#Hence we reduced features from 108 to 60

#transform test into PCA
testing_RF <- predict(prin_comp, newdata = new_test_pca)
testing_RF <- as.data.frame(testing_RF)
#select the first 60 components
testing_RF <- testing_RF[,1:60]
dim(testing_RF)
str(testing_RF)

#Random forests on PCA
training_RF <- data.frame(TripType = training$TripType, prin_comp$x)
View(training_RF)
#we are interested in first 60 PCs
training_RF <- training_RF[,1:61]
dim(training_RF)
str(training_RF)
#train RF
mod_rf<-randomForest(TripType~.,training_RF) #oob-error=31.61%
View(mod_rf$importance)
print(mod_rf)
pred_rf<-predict(mod_rf,testing_RF,type="class")

#Confusion matrix
cm<-confusionMatrix(testing_RF$pred,testing_RF$TripType)
#accuracy
mean(pred_rf==testing$TripType) #accuracy=68.7%
plot(mod)
#oob error
err<-mod$err.rate
#oob accuracy
1-err[nrow(err),"OOB"] #accuracy=67.9%. both values are almost same
#auc
str(testing)
pred_prob<-predict(mod,testing,type ='prob')
auc(testing$TripType,pred_prob)  #default
multiclass.roc(testing$TripType,pred_prob)
library(dplyr)
require(dplyr)
library(zoo)
require(zoo)
require(tidyr)

train<-read.csv("C:/Users/saipr/Desktop/Sai Priya/kaggle/Walmart/train.csv")
test<-read.csv("C:/Users/saipr/Desktop/Sai Priya/kaggle/Walmart/test.csv")
length(unique(train$FinelineNumber))
#lets look at the structure of training data
str(train)
str(test)
View(train2)
train$Upc<-as.character(train$Upc)
test$Upc<-as.character(test$Upc)
#lets examine missing values
sapply(train, function(x) sum(is.na(x))) 
sapply(test, function(x) sum(is.na(x))) 

#there are few missing values in Upc and fineline number


##################### CLEANING
#Looks like data is from a contiguous block of time, maybe a month or so, ordered by visit number.
#With train and test data sampled from there-in. so lets bind the train and test and arrange it by Visitnumber

train<-bind_rows(train,test)
train <- train %>% arrange(VisitNumber)

#drop all NULL description records if they appeared in a VisitNumber that 
#had other non-NULL descriptions associated with it
dnull<-train %>% filter(DepartmentDescription=="NULL") %>% select(VisitNumber) 
dnnull<-train %>% filter(DepartmentDescription!="NULL") %>% select(VisitNumber) 
count(distinct(dnnull))
distinct(setdiff(dnnull,dnull))
t<-intersect(dnnull,dnull) 
distinct(t)
train %>% filter(VisitNumber==87093) %>% group_by(DepartmentDescription) %>% summarise(n())
#there are few records like that, so we simply drop records with NULL description if it also has non-null description
t<-train %>% filter(VisitNumber %in% t$VisitNumber & DepartmentDescription=="NULL")
dim(t)
train <- anti_join(train,t,by=c("VisitNumber","DepartmentDescription"))
dim(train)

#replace fineline with 9999 and upc with -1 for records with missing fineline and Upc
train$Upc<-ifelse(is.na(train$Upc),-1,train$Upc)
train$FinelineNumber<-ifelse(is.na(train$FinelineNumber),9999,train$FinelineNumber)
sapply(train, function(x) sum(is.na(x))) 

#we have duplicates of same dept desc: Mens wear and MENSWEAR
train$DepartmentDescription[train$DepartmentDescription=="MENSWEAR"]<-"MENS WEAR"


###################### Summary Statisitcs

#no of distinct triptypes per visit number: = 1
distinct_trip_types_per_visitnumber<-train %>% group_by(VisitNumber) %>% summarise(distinct_trip_types=n_distinct(TripType))
View(distinct_trip_types_per_visitnumber)

#no of distinct weekdays per visitnumber : = 1
distinct_weekdays_per_visitnumber<-train %>% group_by(VisitNumber) %>% summarise(no_of_distinct_weekdays=n_distinct(Weekday))
View(distinct_weekdays_per_visitnumber)

#which type has more no of distinct customers: type 8
train %>% group_by(TripType) %>% summarise(no_of_customers=n_distinct(VisitNumber)) %>% arrange(desc(no_of_customers)) %>% summarise(first(TripType),max_no_of_visits=max(no_of_customers))

#in type 8, which dept has more no of customers: DSD Grocery
train %>% filter(TripType==8) %>% group_by(DepartmentDescription) %>% summarise(no_of_customers=n_distinct(VisitNumber)) %>% mutate(rank=rank(desc(no_of_customers))) %>% filter(rank==1)

#which dept has max no of visits : DSD Grocery
train %>% group_by(DepartmentDescription) %>% summarise(no_of_customers=n_distinct(VisitNumber)) %>% mutate(rank=rank(desc(no_of_customers))) %>% filter(rank==1)

#what is the most likely triptype for each weekday : type 8
t<-train %>% group_by(Weekday,TripType) %>% summarise(no_of_customers=n_distinct(VisitNumber)) %>% mutate(rank=rank(desc(no_of_customers))) %>% filter(rank==1)
View(t)

#Can different visit numbers have same upc? yes
#does all the records with same upc belong to same visit number? no
#this is because same product can be bought by different customers
train %>% group_by(Upc) %>% summarise(n_distinct(VisitNumber))

#does all the records with same upc belong to same dept? no
t<-train %>% group_by(Upc) %>% summarise(n_distinct(DepartmentDescription))

#what is the likely triptype for each dept
length(levels(train$DepartmentDescription)) #69
t<-train %>% group_by(DepartmentDescription,TripType) %>% summarise(no_of_customers=n_distinct(VisitNumber)) %>% mutate(rank=rank(desc(no_of_customers))) %>% filter(rank==1)
View(t)
levels(train$DepartmentDescription) %in% t$DepartmentDescription 

#what is the likely triptype for each fineline
train$FinelineNumber<-as.factor(train$FinelineNumber)
length(levels(train$FinelineNumber)) #5196
train %>% group_by(FinelineNumber,TripType) %>% summarise(no_of_customers=n_distinct(VisitNumber)) %>% mutate(rank=rank(desc(no_of_customers))) %>% filter(rank==1)

#which trip type has more no of returns : 999
train %>% filter(ScanCount<0) %>% group_by(TripType) %>% summarise(no_of_returns=sum(ScanCount)) %>% mutate(rank=rank(no_of_returns)) %>% filter(rank==1)

length(unique(train1$VisitNumber))

length(unique(train$FinelineNumber))

##################### Feature Engineering
View(train)
train %>% filter(Upc==88679300501 & VisitNumber==8)
train %>% filter(FinelineNumber==115 & VisitNumber==46)


#is a upc/visitnumber combination duplicated? yes
train %>% group_by(VisitNumber,Upc) %>% summarise(n=n()) %>% filter(n>1)
#so we can group by VisitNumber/upc and add the scancounts to make it a single record
t<-train %>% group_by(VisitNumber,Upc) %>% summarise(ScanCount_New=sum(ScanCount))
train<-train %>% inner_join(t,by=c("VisitNumber","Upc"))
train<-train %>% distinct(VisitNumber,Upc,.keep_all = TRUE)
dim(train)
View(train)

#no of items purchased 
train$PurchaseCount<-ifelse(train$ScanCount_New>=0,train$ScanCount_New,0)
#no of items returned
train$ReturnCount<-ifelse(train$ScanCount_New<0,train$ScanCount_New,0)
train$ReturnCount<-abs(train$ReturnCount)
t<-train %>% group_by(VisitNumber) %>% summarise(total_purchase=sum(PurchaseCount))
train<-train %>% inner_join(t,by="VisitNumber")
t<-train %>% group_by(VisitNumber) %>% summarise(total_returns=sum(ReturnCount))
train<-train %>% inner_join(t,by="VisitNumber")

#is a visitnumber/fineline number cmbination duplicated?
train %>% group_by(VisitNumber,FinelineNumber) %>% summarise(n=n()) %>% filter(n>1)
t<-train %>% group_by(VisitNumber,FinelineNumber) %>% summarise(N_Fineline=sum(PurchaseCount))
train<-train %>% inner_join(t,by=c("VisitNumber","FinelineNumber"))

#is a visitnumber/Dept cmbination duplicated?
train %>% group_by(VisitNumber,DepartmentDescription) %>% summarise(n=n()) %>% filter(n>1)
t<-train %>% group_by(VisitNumber,DepartmentDescription) %>% summarise(N_Dept=sum(PurchaseCount))
train<-train %>% inner_join(t,by=c("VisitNumber","DepartmentDescription"))
t<-train %>% group_by(VisitNumber,DepartmentDescription) %>% summarise(N_Dept_Returns=sum(ReturnCount))
train<-train %>% inner_join(t,by=c("VisitNumber","DepartmentDescription"))

train<-distinct(train,.keep_all = TRUE)
dim(train) #1262448

#so now we have how many prodcuts are bought by each customer under each dept, each fineline and upc

#now filter the most frequent fineline products for every visitnumber
t<-train %>% group_by(VisitNumber,FinelineNumber) %>% arrange(desc(N_Fineline)) %>% group_by(VisitNumber) %>% summarise(VNum=first(VisitNumber),Frequent_Fineline=first(FinelineNumber),N_Freq_Fineline=max(N_Fineline))
train <- train %>% left_join(t,by=c("VisitNumber"="VNum"))
View(train)
names(train)
#remove all unwanted columns including upc and finelinenumber. we dont want that anymore
train<-train[,-16]
train<-train[,-c(4,5,7,8,9,10,13)]
train<-distinct(train,.keep_all = TRUE)

dim(train)
length(unique(train$VisitNumber)) #191348

train1 <- train[,-8]
train2 <- train[,-7]

#spread department description
train1 <- spread(train1,DepartmentDescription,N_Dept)
train2 <- spread(train2,DepartmentDescription,N_Dept_Returns)
dim(train2)
View(train)
length(unique(train2$VisitNumber)) #191348
names(train3)
train3 <- inner_join(train1,train2,by=c("TripType","VisitNumber","Weekday","total_purchase", "total_returns","Frequent_Fineline","N_Freq_Fineline"))
View(train3)
dim(train3)

#replace the Na's in department dummy variables with 0
t<-sapply(train3[,c(2,8:143)],function(x){ifelse(is.na(x),0,x)})
class(t)
t<-data.frame(t)
train3 <- train3[,-c(8:143)]
train3 <- train3 %>% inner_join(t,by="VisitNumber")
sapply(train3, function(x) sum(is.na(x))) 
dim(train3)

#convertign datataypes
train3$TripType<-as.factor(train3$TripType)
train3$Frequent_Fineline<-as.factor(train3$Frequent_Fineline)
str(train3)
sapply(train3, function(x) sum(is.na(x))) 
View(train3)
names(train3)
train3<-train3[,-c(76:143)]
train3<-train3[,-c(6,7)]


dnnull<-NULL
dnull<-NULL
t<-NULL
train1<-NULL
train2<-NULL
train<-NULL
test<-NULL
write.csv(train3,"C:/Users/saipr/Desktop/Sai Priya/kaggle/Walmart/train3.csv")

#split into train and test
test<-train3 %>% filter(is.na(TripType))
length(unique(test$VisitNumber)) #95674
#Train
train<-train3 %>% filter(!is.na(TripType))
length(unique(train$VisitNumber)) #95674


##############################################################


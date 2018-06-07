library(dplyr)
library(tidyr)
train<-read.csv("C:/Users/saipr/Desktop/Sai Priya/kaggle/Walmart/train.csv")
test<-read.csv("C:/Users/saipr/Desktop/Sai Priya/kaggle/Walmart/test.csv")
length(unique(train$FinelineNumber))
length(unique(train$DepartmentDescription))
length(unique(test$FinelineNumber))
length(unique(test$DepartmentDescription))

#lets look at the structure of training data
str(train)
str(test)
View(train)
View(test)
train$TripType<-as.factor(train$TripType)
#lets examine missing values
sapply(train, function(x) sum(is.na(x))) 
sapply(test, function(x) sum(is.na(x))) 

#drop fineline and upc
train<-train[,-c(4,7)]

#distinct dept description
dept<-unique(train$DepartmentDescription)
View(dept)
train$DepartmentDescription<-as.character(train$DepartmentDescription)
#grouping of departments
train$DepartmentDescription<-ifelse(train$DepartmentDescription=="MENS WEAR" | 
                                      train$DepartmentDescription=="LADIESWEAR" |
                                      train$DepartmentDescription=="GIRLS WEAR, 4-6X  AND 7-14" |
                                      train$DepartmentDescription=="MENSWEAR" |
                                      train$DepartmentDescription=="BOYS WEAR" |
                                      train$DepartmentDescription=="PLUS AND MATERNITY" |
                                      train$DepartmentDescription=="SLEEPWEAR/FOUNDATIONS" |
                                      train$DepartmentDescription=="SWIMWEAR/OUTERWEAR" |
                                      train$DepartmentDescription=="BRAS & SHAPEWEAR","Clothing",train$DepartmentDescription)
train$DepartmentDescription<-ifelse(train$DepartmentDescription=="PERSONAL CARE" | 
                                      train$DepartmentDescription=="HEALTH AND BEAUTY AIDS" |
                                      train$DepartmentDescription=="BEAUTY"
                                      ,"BEAUTY",train$DepartmentDescription)

train$DepartmentDescription<-ifelse(train$DepartmentDescription=="PHARMACY OTC" | 
                                      train$DepartmentDescription=="PHARMACY RX" |
                                      train$DepartmentDescription=="OPTICAL - FRAMES" |
                                      train$DepartmentDescription=="OPTICAL - LENSES"
                                    ,"PHARMACY",train$DepartmentDescription)


train$DepartmentDescription<-ifelse(train$DepartmentDescription=="INFANT CONSUMABLE HARDLINES" | 
                                      train$DepartmentDescription=="INFANT APPAREL"
                                    ,"INFANT",train$DepartmentDescription)

train$DepartmentDescription<-ifelse(train$DepartmentDescription=="FROZEN FOODS" | 
                                      train$DepartmentDescription=="SEAFOOD" |
                                      train$DepartmentDescription=="MEAT - FRESH & FROZEN" 
                                    ,"FROZEN FOODS",train$DepartmentDescription)

train$DepartmentDescription<-ifelse(train$DepartmentDescription=="ACCESSORIES" | 
                                      train$DepartmentDescription=="JEWELRY AND SUNGLASSES"
                                    ,"ACCESSORIES",train$DepartmentDescription)

train$DepartmentDescription<-ifelse(train$DepartmentDescription=="PLAYERS AND ELECTRONICS" | 
                                      train$DepartmentDescription=="ELECTRONICS" |
                                      train$DepartmentDescription=="WIRELESS"
                                    ,"ELECTRONICS",train$DepartmentDescription)

train$DepartmentDescription<-ifelse(train$DepartmentDescription=="LADIES SOCKS" | 
                                      train$DepartmentDescription=="SHOES"
                                    ,"SHOES",train$DepartmentDescription)
length(unique(train$DepartmentDescription))

#no of depts visited for each visit number
a<-train %>% group_by(VisitNumber) %>% summarise(no_of_depts=n_distinct(DepartmentDescription))
train<-inner_join(train,a,by="VisitNumber")
dim(train)

#no of times each department was visited by each visit number
b<-train %>% group_by(VisitNumber,DepartmentDescription) %>% summarise(no_of_dept_visits=n())
train<-inner_join(train,b,by=c("VisitNumber","DepartmentDescription"))
dim(train)

#no of purchase
c<-train %>% group_by(VisitNumber) %>% filter(ScanCount>=0) %>% summarise(no_of_purchase=sum(ScanCount))
train<-left_join(train,c,by="VisitNumber")
dim(train)
train$no_of_purchase<-ifelse(is.na(train$no_of_purchase),0,train$no_of_purchase)

#no of returns
d<-train %>% group_by(VisitNumber) %>% filter(ScanCount<0) %>% summarise(no_of_returns=-1*sum(ScanCount))
train<-left_join(train,d,by="VisitNumber")
dim(train)
train$no_of_returns<-ifelse(is.na(train$no_of_returns),0,train$no_of_returns)

#no of purchase from each department
e<-train %>% group_by(VisitNumber,DepartmentDescription) %>% summarise(no_of_dept_purchase=sum(ScanCount))
train<-inner_join(train,e,by=c("VisitNumber","DepartmentDescription"))
dim(train)
str(train)
#transforming the dataset
#no of times each department was visited by each visit number
names(train)
train_a<-train[,-c(4,10)]
train_a<-distinct(train_a,.keep_all = TRUE)
train_a<-spread(train_a,DepartmentDescription,no_of_dept_visits,fill = 0)
dim(train_a)
View(train_a)
str(train_a)
length(unique(train$VisitNumber))

#View(train[train$VisitNumber==67559,])

#no of purchase from each department
train_b<-train[,-c(4,7)]
train_b<-distinct(train_b,.keep_all = TRUE)
train_b<-spread(train_b,DepartmentDescription,no_of_dept_purchase,fill=0)
dim(train_b)
View(train_b)

# train_a<-data.frame(train_a)
# train_b<-data.frame(train_b)
train_c<- inner_join(train_a,train_b,by=c("TripType","VisitNumber","Weekday","no_of_depts","no_of_purchase","no_of_returns"))
names(train_c)
dim(train_c)
View(train_c)
str(train_c)

names(train_c) <- gsub(" ", "_", names(train_c))
names(train_c) <- gsub("-", "_", names(train_c))
names(train_c) <- gsub(",", "", names(train_c))
names(train_c) <- gsub("/", "_", names(train_c))
train_c$one_hr_photo.x<-train_c$`1_HR_PHOTO.x`
train_c$one_hr_photo.y<-train_c$`1_HR_PHOTO.y`
train_c$`1_HR_PHOTO.x`<-NULL
train_c$`1_HR_PHOTO.y`<-NULL
##################################################





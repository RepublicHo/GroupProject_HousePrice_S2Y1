library(ggplot2)
library(mice)
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
#install.packages("xgboost")
library(xgboost)
#install.packages("VIM")
library(VIM)
#install.packages("corrplot")
library(corrplot)
#install.packages("Hmisc")
library(Hmisc)
#install.packages("scales")
library(scales)
#install.packages("randomForest")
library(randomForest)


#import data
train<- read.csv("train.csv")
#train
test<- read.csv("test.csv") 

#typeof(train)
#typeof(test)


test$SalePrice <- NA
train_char <- train[,sapply(train,is.character)]
#train_char
#the character data in train 
train_int <- train[,sapply(train,is.integer)]
train_int
#train_int
#the integer data in train 
test_char <- test[,sapply(test,is.character)]
#test_char
test_int <- test[,sapply(test,is.integer)]
#test_int

# We think it's not efficient to analyze all the variables provided
# Thus, we need to select some significant variables,
# process the data properly and base on them to predict

#Shihao QI

#first show number of NA in each features
aggr(train_int, prop = F, number = T)
aggr(train_char[, 1:38], prop = F, number = T) #connot show all 42 features in one pic, so delete 4 columns
#and these four columns have not contain NA

train_char[is.na(train_char)]<-"None" #change NA in character features
tips<-mice(train_int, method='pmm')  #change NA in integer features
train_int<-complete(tips)
train3<-complete(tips) # make a copyer to store

train1<-bind_cols(train_char, train_int)
train2<-train1

aggr(train[, 1:10], prop = F, number = T)

trainis=matrix(0, nrow = length(train$Id), ncol=2)
names=as.vector(unlist(colnames(train_int)))

for (im in 1:37){   # a auxiliary visualize
  trainis[, 1]=train3[, im]
  trainis[, 2]=train3$SalePrice
  clu<-kmeans(trainis, 10)
  corcluster=cor(trainis, use="pairwise.complete.obs")
  plot(x=trainis[,1], y=trainis[,2], col=clu$cluster, main=names[im], xlab = corcluster
       ,ylab="SalaryPrice")
}

train_int<-as.data.frame(train_int)
corint<-cor(train_int, use = "pairwise.complete.obs")   #visualize relationship between
cortestint<-rcorr(as.matrix(train_int), type=c("pearson")) #sale-price with integer features
cor<-as.matrix(sort(corint[, 'SalePrice']))
corpres<-names(which(apply(cor, 1, function(x) abs(x)>0.55)))

corint<-corint[corpres, corpres]
corrplot(corint, method = c("number"), type=c("lower"),tl.pos = "lt", tl.col = "black",
         tl.cex=0.7, cl.cex=.6, number.cex=.8) 
#important value
set.seed(2000)
rfresult<-randomForest(x=train_char[1:1460, 1:42], y=train$SalePrice, ntree=300,
                       importance=TRUE)
imporan<-importance(rfresult)
imporan1<-data.frame(variables=row.names(imporan), importance1=imporan[,1])
imporan1<-imporan1[order(imporan1$importance1, decreasing = T), ]
ggplot(imporan1[1:20,], aes(x=reorder(variables, importance1), y=importance1, fill=importance1)) + 
  geom_bar(stat = 'identity') + labs(x = 'Variables', y= 'importances for variables') + coord_flip() + theme(legend.position="none")
#visualize relationship between character features and sale-price

imporall<-randomForest(x=train1[1:1460, 1:79], y=train1$SalePrice, ntree=300, importance = T)
imporall2<-importance(imporall)
imporal1<-data.frame(variables=row.names(imporall2), importances=imporall2[,1])
imporal1<-imporal1[order(imporal1$importances, decreasing = T),]
ggplot(imporal1[1:20,], aes(x=reorder(variables, importances), y=importances, fill=importances)) + 
  geom_bar(stat = 'identity') + labs(x = 'Variables', y= 'importances for variables') + coord_flip() + theme(legend.position="none")



#Zhejun HE After there is no NA.

#typeof(train$MSSubClass)
# While for MSSubClass, it should be factor instead of integer
# we firstly considerd that 
# Since it's just a label, and does not have any meaning as integer
# But we need to use xgb.DMatrix, so it should remain to be integer
#str(test$MSSubClass)
#train$MSSubClass <- as.factor(train$MSSubClass)
#test$MSSubClass <- as.factor(test$MSSubClass)
#train$MSSubClass
#test$MSSubClass


train$MSSubClass<-as.numeric(train$MSSubClass)
test$MSSubClass<-as.numeric(test$MSSubClass)

#str(train$MSSubClass)
#str(test$MSSubClass)
#train$MSSubClass


# Some Analyses on time

year<-train$YrSold
month<-train$MoSold
time<-(year-2006)*12+month  # month starting from Jan, 2006
price<-train$SalePrice
#time

# Price~house age
# 1. Analysis
age<- year - train$YearBuilt
#age
Price_age<-data.frame(age,price)
ggplot(Price_age,aes(x=age,y=price))+
  geom_smooth(method='glm')+geom_point(size=1)+xlab("Age")+
  ylab("Price")+ggtitle("Price-HouseAge")


# Hence, the price has something to do with house age
# then we need to convert built time to age

# 2. To prepare data
train$YearBuilt=train$YrSold-train$YearBuilt
test$YearBuilt=test$YrSold-test$YearBuilt


#typeof(all$YrSold)
#typeof(all$MoSold)
# Yearsold, monthsold as integer do not make sense
# they should be factor (categorical variables)
# Also, there was a economic crisis during that 5 years 
# Hence, time series works here 

# Price~time 2006-2010 
# 1. Analysis


Price_YM <- data.frame(time,price)
ggplot(Price_YM,aes(x=time,y=price))+
  geom_bar(stat='summary', fun.y = "median", fill='white')+xlab("Time")+
  geom_line(stat='summary',size = 0.1, fun.y = "median",color="red", group = 1)+
  ylab("Median Price")+ggtitle("Price-time")
# we can get from the graph that housing price has something to do with years & months
# Hence, we need to analyze their relation



# Price-year
Price_Year<-data.frame(year,price)
ggplot(Price_Year,aes(x=year,y=price))+
  geom_bar(stat='summary', fun.y = "median", fill='#00abff')+xlab("Year")+
  geom_line(stat='summary',size = 0.1, fun.y = "median",color="red", group = 1)+
  ylab("Median Price")+ggtitle("Price-Year")


# 2. To prepare data, divide them into two groups
# before financial crisis of 2008
train$YrSold[train$YrSold %in% c(2006,2007)] <- 2
test$YrSold[test$YrSold %in% c(2006,2007)] <- 2
# after financial crisis of 2008
train$YrSold[train$YrSold %in% c(2008,2009,2010)] <- 1
test$YrSold[!test$YrSold %in% c(2008,2009,2010)] <- 1

train$YrSold


# Price~month
Price_Month<-data.frame(month,price)
ggplot(Price_Month,aes(x=month,y=price))+
  geom_bar(stat='summary', fun.y = "median", fill='#00abff')+xlab("Month")+
  geom_line(stat='summary',size = 0.1, fun.y = "median",color="red", group = 1)+
  ylab("Median Price")+ggtitle("Price-Month")
Price_Month
# it could be concluded that price is relatively low during April and May
#                                     relatively high in other months


# 2. To prepare data(divide Month into several factors)
train$MoSold[train$MoSold %in% c(1,2,3,6,7,8,10,12)] <- 2
test$MoSold[test$MoSold %in% c(1,2,3,6,7,8,10,12)] <- 2
train$MoSold[train$MoSold %in% c(4,5)] <- 1
test$MoSold[test$MoSold %in% c(4,5)] <- 1
train$MoSold[train$MoSold %in% c(9,11)] <- 3
test$MoSold[test$MoSold %in% c(9,11)] <- 3

#train$MoSold

train$MoSold
test$MoSold
# Price~square feet (basement area+living area square feet)
# 1. Analysis
Area <- train$GrLivArea + train$TotalBsmtSF
Price_Area <- data.frame(Area, price)
ggplot(Price_Area,aes(x=Area,y=price))+
  geom_smooth(method='glm')+geom_point(size=1)+xlab("Square feet")+
  ylab("Price")+ggtitle("Price-Feet")
# But there is no need to take some extreme situations into account


#Price_Area <- data.frame(Area, price)
#Price_Area<-Price_Area[Area<=7500,]  # delete some outliers
#ggplot(Price_Area,aes(x=Area,y=price))+
  geom_smooth(method='gam')+geom_point(size=1)+xlab("Square feet")+
  ylab("Price")+ggtitle("Price-Feet")

# 2. To prepare data
# (1) Add TotalBsmtSF to GrLivArea
train$GrLivArea<-train$GrLivArea + train$TotalBsmtSF
test$GrLivArea<-test$GrLivArea + test$TotalBsmtSF

# (2) Delete TotalBsmtSF column
train<-subset(train,select = -c(TotalBsmtSF))
test<-subset(test,select = -c(TotalBsmtSF))


# (3) Delete some outliers in train
train<-train[train$GrLivArea<=7500,]


Area <- train$GrLivArea + train$TotalBsmtSF
Price_Area <- data.frame(Area, price)
ggplot(Price_Area,aes(x=Area,y=price))+
  geom_smooth(method='glm')+geom_point(size=1)+xlab("Square feet")+
  ylab("Price")+ggtitle("Price-Feet")

# Price~neighborhood
# Since in American, neighborhood is a significant factor in house choosing
# categorical variable 
# 1. Analysis
neighborhood<-train$Neighborhood
Price_Neighborhood<-data.frame(neighborhood,price)

ggplot(Price_Neighborhood,aes(x=neighborhood,y=price))+
  geom_bar(stat='summary', fun.y = "mean", fill='blue')

# 2. To prepare data
train$Neighborhood[train$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 3
test$Neighborhood[test$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 3

train$Neighborhood[train$Neighborhood %in% c('Blmngtn', 'ClearCr', 'CollgCr', 
    'Crawfor', 'Gilbert', 'NWAmes','OldTown','SawyerW','Somerst','Timber','Veenker')] <- 2
test$Neighborhood[test$Neighborhood %in% c('Blmngtn', 'ClearCr', 'CollgCr', 
    'Crawfor', 'Gilbert', 'NWAmes','OldTown','SawyerW','Somerst','Timber','Veenker')] <- 2

train$Neighborhood[train$Neighborhood %in% c('Blueste','BrkSide','Edwards','NAmes','NPkVill',
    'Mitchel','Sawyer','MeadowV', 'IDOTRR', 'BrDale','SWISU')] <- 1
test$Neighborhood[test$Neighborhood %in% c('Blueste','BrkSide','Edwards','NAmes','NPkVill',
    'Mitchel','Sawyer','MeadowV', 'IDOTRR', 'BrDale','SWISU')] <- 1

# According to the class of the neighborhood
train$Neighborhood<-as.numeric(train$Neighborhood)
test$Neighborhood<-as.numeric(test$Neighborhood)
train$Neighborhood


train$ExterQual<-as.character(train$ExterQual)
train$ExterQual[train$ExterQual=="Ex"]<-"5"
train$ExterQual[train$ExterQual=="Gd"]<-"4"
train$ExterQual[train$ExterQual=="TA"]<-"3"
train$ExterQual[train$ExterQual=="Fa"]<-"2"
train$ExterQual[train$ExterQual=="Po"]<-"1"
train$ExterQual<-as.numeric(train$ExterQual)


test$ExterQual<-as.character(test$ExterQual)
test$ExterQual[test$ExterQual=="Ex"]<-"5"
test$ExterQual[test$ExterQual=="Gd"]<-"4"
test$ExterQual[test$ExterQual=="TA"]<-"3"
test$ExterQual[test$ExterQual=="Fa"]<-"2"
test$ExterQual[test$ExterQual=="Po"]<-"1"
test$ExterQual<-as.numeric(test$ExterQual)




train$BsmtQual <- as.character(train$BsmtQual)
train$BsmtQual[train$BsmtQual == "Ex"] <- "5"
train$BsmtQual[train$BsmtQual == "Gd"] <- "4"
train$BsmtQual[train$BsmtQual == "TA"] <- "3"
train$BsmtQual[train$BsmtQual == "Fa"] <- "2"
train$BsmtQual[train$BsmtQual == "Po"] <- "1"
train$BsmtQual[is.na(train$BsmtQual)] <- "0"
train$BsmtQual <- as.numeric(train$BsmtQual)
train$BsmtQual


test$BsmtQual <- as.character(test$BsmtQual)
test$BsmtQual[test$BsmtQual == "Ex"] <- "5"
test$BsmtQual[test$BsmtQual == "Gd"] <- "4"
test$BsmtQual[test$BsmtQual == "TA"] <- "3"
test$BsmtQual[test$BsmtQual == "Fa"] <- "2"
test$BsmtQual[test$BsmtQual == "Po"] <- "1"
test$BsmtQual[is.na(test$BsmtQual)] <- "0"
test$BsmtQual <- as.numeric(test$BsmtQual)
test$BsmtQual


train$BsmtFinType1 <- as.character(train$BsmtFinType1)

train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "0"
train$BsmtFinType1[train$BsmtFinType1 == "Unf"] <- "1"
train$BsmtFinType1[train$BsmtFinType1 == "LwQ"] <- "2"
train$BsmtFinType1[train$BsmtFinType1 == "Rec"] <- "3"
train$BsmtFinType1[train$BsmtFinType1 == "BLQ"] <- "4"
train$BsmtFinType1[train$BsmtFinType1 == "ALQ"] <- "5"
train$BsmtFinType1[train$BsmtFinType1 == "GLQ"] <- "6"
train$BsmtFinType1 <- as.numeric(train$BsmtFinType1)
#train$BsmtFinType1

test$BsmtFinType1[is.na(test$BsmtFinType1)] <- "0"
test$BsmtFinType1[test$BsmtFinType1 == "Unf"] <- "1"
test$BsmtFinType1[test$BsmtFinType1 == "LwQ"] <- "2"
test$BsmtFinType1[test$BsmtFinType1 == "Rec"] <- "3"
test$BsmtFinType1[test$BsmtFinType1 == "BLQ"] <- "4"
test$BsmtFinType1[test$BsmtFinType1 == "ALQ"] <- "5"
test$BsmtFinType1[test$BsmtFinType1 == "GLQ"] <- "6"
test$BsmtFinType1 <- as.numeric(test$BsmtFinType1)
#test$BsmtFinType1

train$Foundation <- as.character(train$Foundation)
train$Foundation[train$Foundation == "NA"] <- "0"
train$Foundation[train$Foundation == "BrkTil"] <- "1"
train$Foundation[train$Foundation == "CBlock"] <- "2"
train$Foundation[train$Foundation == "PConc"] <- "3"
train$Foundation[train$Foundation == "Slab"] <- "4"
train$Foundation[train$Foundation == "Stone"] <- "5"
train$Foundation[train$Foundation == "Wood"] <- "6"
train$Foundation <- as.numeric(train$Foundation)


test$Foundation <- as.character(test$Foundation)
test$Foundation[test$Foundation == "NA"] <- "0"
test$Foundation[test$Foundation == "BrkTil"] <- "1"
test$Foundation[test$Foundation == "CBlock"] <- "2"
test$Foundation[test$Foundation == "PConc"] <- "3"
test$Foundation[test$Foundation == "Slab"] <- "4"
test$Foundation[test$Foundation == "Stone"] <- "5"
test$Foundation[test$Foundation == "Wood"] <- "6"
test$Foundation <- as.numeric(test$Foundation)


train$FireplaceQu <- as.character(train$FireplaceQu)
train$FireplaceQu[train$FireplaceQu == "Ex"] <- "5"
train$FireplaceQu[train$FireplaceQu == "Gd"] <- "4"
train$FireplaceQu[train$FireplaceQu == "TA"] <- "3"
train$FireplaceQu[train$FireplaceQu == "Fa"] <- "2"
train$FireplaceQu[train$FireplaceQu == "Po"] <- "1"
train$FireplaceQu[is.na(train$FireplaceQu)] <- "0"
train$FireplaceQu <- as.numeric(train$FireplaceQu)


test$FireplaceQu <- as.character(test$FireplaceQu)
test$FireplaceQu[test$FireplaceQu == "Ex"] <- "5"
test$FireplaceQu[test$FireplaceQu == "Gd"] <- "4"
test$FireplaceQu[test$FireplaceQu == "TA"] <- "3"
test$FireplaceQu[test$FireplaceQu == "Fa"] <- "2"
test$FireplaceQu[test$FireplaceQu == "Po"] <- "1"
test$FireplaceQu[is.na(test$FireplaceQu)] <- "0"
test$FireplaceQu <- as.numeric(test$FireplaceQu)
#train$FireplaceQu

train$KitchenQual <- as.character(train$KitchenQual)
train$KitchenQual[train$KitchenQual == "NA"] <- "0"
train$KitchenQual[train$KitchenQual == "Ex"] <- "1"
train$KitchenQual[train$KitchenQual == "Gd"] <- "2"
train$KitchenQual[train$KitchenQual == "TA"] <- "3"
train$KitchenQual[train$KitchenQual == "Fa"] <- "4"
train$KitchenQual[train$KitchenQual == "Po"] <- "5"
train$KitchenQual <- as.numeric(train$KitchenQual)

test$KitchenQual <- as.character(test$KitchenQual)
test$KitchenQual[test$KitchenQual == "NA"] <- "0"
test$KitchenQual[test$KitchenQual == "Ex"] <- "1"
test$KitchenQual[test$KitchenQual == "Gd"] <- "2"
test$KitchenQual[test$KitchenQual == "TA"] <- "3"
test$KitchenQual[test$KitchenQual == "Fa"] <- "4"
test$KitchenQual[test$KitchenQual == "Po"] <- "5"
test$KitchenQual <- as.numeric(test$KitchenQual)

train$MSZoning[train$MSZoning=="C (all)"]<-1
train$MSZoning[train$MSZoning=="FV"]<-2
train$MSZoning[train$MSZoning=="RH"]<-3
train$MSZoning[train$MSZoning=="RL"]<-4
train$MSZoning[train$MSZoning=="RM"]<-5
train$MSZoning=as.numeric(train$MSZoning)


test$MSZoning[test$MSZoning=="C (all)"]<-1
test$MSZoning[test$MSZoning=="FV"]<-2
test$MSZoning[test$MSZoning=="RH"]<-3
test$MSZoning[test$MSZoning=="RL"]<-4
test$MSZoning[test$MSZoning=="RM"]<-5
test$MSZoning=as.numeric(test$MSZoning)

train$GarageType[is.na(train$GarageType)] <- "0"
train$GarageType[train$GarageType=="2Types"]<-1
train$GarageType[train$GarageType=="Attchd"]<-2
train$GarageType[train$GarageType=="Basment"]<-3
train$GarageType[train$GarageType=="BuiltIn"]<-4
train$GarageType[train$GarageType=="CarPort"]<-5
train$GarageType[train$GarageType=="Detchd"]<-6
train$GarageType=as.numeric(train$GarageType)

test$GarageType[is.na(test$GarageType)] <- "0"
test$GarageType[test$GarageType=="2Types"]<-1
test$GarageType[test$GarageType=="Attchd"]<-2
test$GarageType[test$GarageType=="Basment"]<-3
test$GarageType[test$GarageType=="BuiltIn"]<-4
test$GarageType[test$GarageType=="CarPort"]<-5
test$GarageType[test$GarageType=="Detchd"]<-6
test$GarageType=as.numeric(test$GarageType)


train$BldgType[train$BldgType=="1Fam"]<-1
train$BldgType[train$BldgType=="2fmCon"]<-2
train$BldgType[train$BldgType=="Duplex"]<-3
train$BldgType[train$BldgType=="Twnhs"]<-4
train$BldgType[train$BldgType=="TwnhsE"]<-5
train$BldgType=as.numeric(train$BldgType)

test$BldgType[test$BldgType=="1Fam"]<-1
test$BldgType[test$BldgType=="2fmCon"]<-2
test$BldgType[test$BldgType=="Duplex"]<-3
test$BldgType[test$BldgType=="Twnhs"]<-4
test$BldgType[test$BldgType=="TwnhsE"]<-5
test$BldgType=as.numeric(test$BldgType)

train$HouseStyle[train$HouseStyle=="1.5Fin"]<-1
train$HouseStyle[train$HouseStyle=="1.5Unf"]<-2
train$HouseStyle[train$HouseStyle=="1Story"]<-3
train$HouseStyle[train$HouseStyle=="2.5Fin"]<-4
train$HouseStyle[train$HouseStyle=="2.5Unf"]<-5
train$HouseStyle[train$HouseStyle=="2Story"]<-6
train$HouseStyle[train$HouseStyle=="SFoyer"]<-7
train$HouseStyle[train$HouseStyle=="SLvl"]<-8
train$HouseStyle=as.numeric(train$HouseStyle)


test$HouseStyle[test$HouseStyle=="1.5Fin"]<-1
test$HouseStyle[test$HouseStyle=="1.5Unf"]<-2
test$HouseStyle[test$HouseStyle=="1Story"]<-3
test$HouseStyle[test$HouseStyle=="2.5Fin"]<-4
test$HouseStyle[test$HouseStyle=="2.5Unf"]<-5
test$HouseStyle[test$HouseStyle=="2Story"]<-6
test$HouseStyle[test$HouseStyle=="SFoyer"]<-7
test$HouseStyle[test$HouseStyle=="SLvl"]<-8
test$HouseStyle=as.numeric(test$HouseStyle)


#Important character


train$BsmtFinType1  
train$FireplaceQu 
train$ExterQual
train$BsmtQual  
train$Foundation
train$KitchenQual
train$MSZoning
train$GarageType  
train$BldgType   
train$HouseStyle
typeof(train$GarageType)


#Important integer variable
train$GrLivArea
train$OverallQual
train$GarageCars
train$YearBuilt
train$KitchenQual
train$GarageFinish
#YrSold after testing, it's not good

Tratrain<-train[, c("BsmtFinType1","MSSubClass","ExterQual","BsmtQual","Foundation",
      "KitchenQual","MSZoning","GarageType","BldgType","HouseStyle","Neighborhood",
      "GrLivArea","OverallQual","GarageCars","YearBuilt","KitchenQual")]

Tratest<-test[, c("BsmtFinType1","MSSubClass","ExterQual","BsmtQual","Foundation",
     "KitchenQual","MSZoning","GarageType","BldgType","HouseStyle","Neighborhood",
     "GrLivArea","OverallQual","GarageCars","YearBuilt","KitchenQual")]

Tratrain=as.matrix(Tratrain)
Tratest=as.matrix(Tratest)

Tratest




xgbTreeGrid <- expand.grid(
  nrounds = 1000,
  max_depth = 4,
  eta = 0.02,
  gamma = 0,
  colsample_bytree=1,
  subsample = 0.8,
  min_child_weight = c(2, 3))

train_price <- train$SalePrice#set the train's price as the label for the xgb.Dmatrix.

train2 <- xgb.DMatrix(data = Tratrain, label= train_price)#we have to transform our data to a matrix if we want to use xgboost. 
test2 <- xgb.DMatrix(data = Tratest)


#Using GridSearch to tune parameters of xgboost.
GridSearch <-list(
  objective = "reg:linear", #linear regression.
  booster = "gblinear", # because the we choose the reg:linear, gblinear is the best method. gblinear is based on linear model.
  
  eta = c(0.2),#The default is 0.3 and the alias is leanring Rate, the shrinkage step used in the update process. 
  #After each lifting calculation, the algorithm will directly obtain the weight of the new feature.
  max_depth = c(5),#The greater the maximum depth of the tree, the easier it is to over fit,so 5 is ok.
  gamma = 0, #default is 0, we also use 0 here. When a node is split, the node will be split only if the value of the loss function decreases after splitting.
  colsample_bytree=1, #The default is 1, which is used to control the proportion of the number of columns randomly sampled by each tree.
  min_child_weight=c(3),#default is 1, we use 3 here. When its value is large, it can avoid learning local special samples. But if this value is too high, it will lead to under fitting.
  subsample=0.8 #The default is 1. This parameter controls the proportion of random sampling for each tree
)


modle <- xgb.train(data = train2, params=GridSearch, nrounds = 500)


Pridiction <- predict(modle, test2)
test_labels<-c(1461:2919)
test_labels
result <- data.frame(Id = test_labels, SalePrice = (Pridiction))

write.csv(result, file = 'paker1.csv', row.names = F)








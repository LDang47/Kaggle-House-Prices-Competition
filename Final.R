library(dplyr)
library(VIM)
library(mice)
library(corrplot)
library(ggplot2)
library(moments) #to compute skewness and kurtosis
library(psych)
library(keep)
library(purrr)
library(tidyr)
library(DataExplorer)
library(pracma)
library(glmnet)

###
### House price prediction model in R 
###

train.csv <- "C:\\Users\\Alo-Ai day-Toi day\\Desktop\\house-prices-advanced-regression-techniques\\train.csv"
test.csv <- "C:\\Users\\Alo-Ai day-Toi day\\Desktop\\house-prices-advanced-regression-techniques\\test.csv"
train <-read.csv(train.csv, header=TRUE, sep=",", stringsAsFactors = TRUE) #load the data into the train dataframe
test <- read.csv(test.csv, header=TRUE, sep=",", stringsAsFactors = TRUE) #load the data into the test dataframe
str(train)
str(test)

df <- bind_rows(train, test) #combine 2 files: train and test into 1 called df for easy imputation
str(df)

###create_report(df)

##################################################### INSPECTING DATA ###################################################################

### Histogram of Sale Price in train: House price  has a normal ditribution that is skewed towards the left.
hist(train$SalePrice, main="Histogram for House Sale Price", 
     xlab="Passengers", 
     col="gray",
     las=1) 

skewness(train$SalePrice) #1.880941: this shows the data for Sale Price is positively skewed - right-skewed

### Correlation Analysis: 
cor.var <- names(train)[which(sapply(train, is.numeric))]
num.train <- train[, cor.var]
corrmatrix <- corrplot(cor(num.train,use="pairwise.complete.obs"), outline = T, method = "color", tl.col="black", tl.cex=0.55)
corrmatrix #Show all the correlation coefficients

################################################ OUTLIERS ###############################################################

### Pair plot all selected variable: scatter plot
pairs(~ df$LotArea+df$LotFrontage+ df$OverallQual+df$GrLivArea+df$TotalBsmtSF+df$GarageArea+df$X1stFlrSF+ df$X2ndFlrSF+ df$FullBath+df$YearBuilt+ df$SalePrice,
      main = "Scatterplot of House Price Data", pch=20, cex = 0.7, col = "blue")

### Zoom in graphs to identify potential outliers in the training set
plot1 <- subset(df, !is.na(TotalBsmtSF))
plot1 <- ggplot(plot1, aes(TotalBsmtSF, SalePrice)) + geom_point(color = 'blue') + theme_bw()
print(plot1)

plot2 <- subset(df, !is.na(GrLivArea))
plot2 <- ggplot(plot2, aes(GrLivArea, SalePrice)) + geom_point(color = 'blue') + theme_bw()
print(plot2)

### Remove 1 outlier
df <- subset(df, TotalBsmtSF < 6000 | is.na(TotalBsmtSF))

################################################### IMPUTING MISSING VALUE: DF #########################################################
md.pattern(df)
p <- function(x){sum(is.na(x))/length(x)*100} # proportion of data is missing
apply(df,2,p)

aggr_plot <- aggr(df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

### Basement related variables ###
levels(df$BsmtQual)<-c(levels(df$BsmtQual),"None") 
df$BsmtQual[which(is.na(df$BsmtQual))] <- "None"

levels(df$BsmtCond)<-c(levels(df$BsmtCond),"None") 
df$BsmtCond[which(is.na(df$BsmtCond))] <- "None"

levels(df$BsmtExposure)<-c(levels(df$BsmtExposure),"None") 
df$BsmtExposure[which(is.na(df$BsmtExposure))] <- "None"

levels(df$BsmtFinType1)<-c(levels(df$BsmtFinType1),"None") 
df$BsmtFinType1[which(is.na(df$BsmtFinType1))] <- "None"

levels(df$BsmtQual)<-c(levels(df$BsmtQual),"None") 
df$BsmtQual[which(is.na(df$BsmtQual))] <- "None"

levels(df$BsmtFinType2)<-c(levels(df$BsmtFinType2),"None") 
df$BsmtFinType2[which(is.na(df$BsmtFinType2))] <- "None"

df$BsmtFinSF1[which(is.na(df$BsmtFinSF1))] <- 0

df$BsmtFinSF2[which(is.na(df$BsmtFinSF2))] <- 0

df$BsmtUnfSF[which(is.na(df$BsmtUnfSF))] <- 0

df$TotalBsmtSF[which(is.na(df$TotalBsmtSF))] <- 0

df$BsmtFullBath[which(is.na(df$BsmtFullBath))] <- 0

df$BsmtHalfBath[which(is.na(df$BsmtHalfBath))] <- 0

### Garage related variables ###
levels(df$GarageType)<-c(levels(df$GarageType),"None") 
df$GarageType[which(is.na(df$GarageType))] <- "None"

levels(df$GarageFinish)<-c(levels(df$GarageFinish),"None") 
df$GarageFinish[which(is.na(df$GarageFinish))] <- "None"

levels(df$GarageQual)<-c(levels(df$GarageQual),"None") 
df$GarageQual[which(is.na(df$GarageQual))] <- "None"

levels(df$GarageCond)<-c(levels(df$GarageCond),"None") 
df$GarageCond[which(is.na(df$GarageCond))] <- "None"

df$GarageYrBlt[which(is.na(df$GarageYrBlt))] <- 0

df$GarageArea[which(is.na(df$GarageArea))] <- 0

df$GarageCars[which(is.na(df$GarageCars))] <- 0

### NA values mean the property does not have such facilities: Change NA to None ###
levels(df$Alley) <- c(levels(df$Alley), "None")
df$Alley[which(is.na(df$Alley))] <- "None"

levels(df$FireplaceQu)<-c(levels(df$FireplaceQu),"None") 
df$FireplaceQu[which(is.na(df$FireplaceQu))] <- "None"

levels(df$PoolQC)<-c(levels(df$PoolQC),"None") 
df$PoolQC[which(is.na(df$PoolQC))] <- "None"

levels(df$Fence)<-c(levels(df$Fence),"None") 
df$Fence[which(is.na(df$Fence))] <- "None"

levels(df$MiscFeature)<-c(levels(df$MiscFeature),"None") 
df$MiscFeature[which(is.na(df$MiscFeature))] <- "None"

### Inconsistent - Inlcude both None and NA: Change NA to None ###
df$MasVnrType[which(is.na(df$MasVnrType))] <- "None"

df$MasVnrArea[which(is.na(df$MasVnrArea))] <- 0

### Mode Imputation - Utilities: Change NA to most frequent values
df$Utilities[which(is.na(df$Utilities))] <- unique(df$Utilities)[df$Utilities %>%
                                                                     match(unique(df$Utilities)) %>%
                                                                     tabulate() %>%
                                                                     which.max()]


### Mode Imputation - Functional: Change NA to most frequent values
df$Functional[which(is.na(df$Functional))] <-  unique(df$Functional)[df$Functional %>%
                                                                      match(unique(df$Functional)) %>%
                                                                      tabulate() %>%
                                                                      which.max()]

### Mode Imputation - Electrical: Change NA to most frequent values
df$Electrical[which(is.na(df$Electrical))] <- unique(df$Electrical)[df$Electrical %>%
                                                                       match(unique(df$Electrical)) %>%
                                                                       tabulate() %>%
                                                                       which.max()]
### Mode Imputation - KitchenQual: Change NA to most frequent values
df$KitchenQual[which(is.na(df$KitchenQual))] <-  unique(df$KitchenQual)[df$KitchenQual %>%
                                                                       match(unique(df$KitchenQual)) %>%
                                                                       tabulate() %>%
                                                                       which.max()]

### Mode Imputation - Exterior1st and Exterior2nd: : Change NA to most frequent values
df$Exterior1st[which(is.na(df$Exterior1st))] <- unique(df$Exterior1st)[df$Exterior1st %>%
                                                                          match(unique(df$Exterior1st)) %>%
                                                                          tabulate() %>%
                                                                          which.max()]

df$Exterior2nd[which(is.na(df$Exterior2nd))] <- unique(df$Exterior2nd)[df$Exterior2nd %>%
                                                                          match(unique(df$Exterior2nd)) %>%
                                                                          tabulate() %>%
                                                                          which.max()]

### Mode Imputation - SaleType: : Change NA to most frequent values
df$SaleType[which(is.na(df$SaleType))] <- as.factor(unique(df$SaleType)[df$SaleType %>%
                                                                          match(unique(df$SaleType)) %>%
                                                                          tabulate() %>%
                                                                          which.max()])

### Mode Imputation - MSZoning: : Change NA to most frequent values
df$MSZoning[which(is.na(df$MSZoning))] <-  unique(df$MSZoning)[df$MSZoning %>%
                                                                 match(unique(df$MSZoning)) %>%
                                                                 tabulate() %>%
                                                                 which.max()]


### Impute value for LotFrontage using median imputation method
df$LotFrontage[is.na(df$LotFrontage)] <- mean(df$LotFrontage, na.rm = TRUE)

md.pattern(df) ### Df dataset completed imputation

############################################################# FEATURE ENGINEERING ##########################################################
### Create new variables based on existing variables
df$TotalSF <- df$TotalBsmtSF + df$X1stFlrSF + df$X2ndFlrSF

df$Total_Bathrooms <- df$FullBath + 0.5 * df$HalfBath + df$BsmtFullBath + 0.5 * df$HalfBath

df$Total_PorchSF <- df$OpenPorchSF + df$X3SsnPorch + df$EnclosedPorch + df$ScreenPorch + df$WoodDeckSF

df$HouseAge <- df$YrSold - df$YearBuilt

df$YearSinceRemodel <- df$YrSold - df$YearRemodAdd

### Calssify some variables as ordinal factors

df$OverallQual <- factor(df$OverallQual, order = TRUE, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
df$OverallCond <- factor(df$OverallCond, order = TRUE, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

df$ExterQual <- factor(df$ExterQual, order = TRUE, levels = c("Po", "Fa", "TA", "Gd","Ex"))
df$BsmtQual <- factor(df$ExterCond, order = TRUE, levels = c("Po", "Fa", "TA", "Gd","Ex"))
df$BsmtQual <- factor(df$BsmtQual, order = TRUE, levels = c("None", "Po", "Fa", "TA", "Gd","Ex"))
df$BsmtCond <- factor(df$BsmtCond, order = TRUE, levels = c("None", "Po", "Fa", "TA", "Gd","Ex"))

df$BsmtExposure <- factor(df$BsmtExposure, order = TRUE, levels = c("None", "No", "Mn", "Av", "Gd"))
df$BsmtFinType1 <- factor(df$BsmtFinType1, order = TRUE, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
df$BsmtFinType2 <- factor(df$BsmtFinType2, order = TRUE, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
df$HeatingQC <- factor(df$HeatingQC, order = TRUE, levels = c("Po", "Fa", "TA", "Gd","Ex"))
df$KitchenQual <- factor(df$KitchenQual, order = TRUE, levels = c("Po", "Fa", "TA", "Gd","Ex"))
df$FireplaceQu <- factor(df$FireplaceQu, order = TRUE, levels = c("None", "Po", "Fa", "TA", "Gd","Ex"))
df$GarageQual <- factor(df$GarageQual, order = TRUE, levels = c("None", "Po", "Fa", "TA", "Gd","Ex"))
df$GarageCond <- factor(df$GarageCond, order = TRUE, levels = c("None", "Po", "Fa", "TA", "Gd","Ex"))
df$PoolQC <- factor(df$PoolQC, order = TRUE, levels = c("None", "Fa", "TA", "Gd","Ex"))
df$Fence <- factor(df$Fence, order = TRUE, levels = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"))

str(df)

### Classify some vsariables as factors
df$MSSubClass <- as.factor(df$MSSubClass)
df$MoSold <- as.factor(df$MoSold)
df$YrSold <- as.factor(df$YrSold)
df$Condition2 <- as.factor(df$Condition2)
df$HouseStyle <- as.factor(df$HouseStyle)
df$OverallQual <- as.factor(df$OverallQual)
df$OverallCond <- as.factor(df$OverallCond)
df$Utilities <- as.factor(df$Utilities)
df$Functional <- as.factor(df$Functional)
df$RoofMatl <- as.factor(df$RoofMatl)
df$Exterior1st <- as.factor(df$Exterior1st)
df$Exterior2nd <- as.factor(df$Exterior2nd)
df$Heating <- as.factor(df$Heating)
df$Electrical <- as.factor(df$Electrical)
df$MiscFeature <- as.factor(df$MiscFeature)
df$GarageQual <- as.factor(df$GarageQual)
df$PoolQC <- as.factor(df$PoolQC)

str(df)

write.csv(df, file = "full_house_data.csv") # export the imputed data file into a CSV file

#################################################### DATA EXPLORATION ###################################################################
### Quick plot all numerical variables: histogram
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

################################################# REGRESSION MODEL ##############################################
#create the y variable and matrix (capital X) of x variables (will make the code below easier to read + will ensure that all interactoins exist)
df_train <- subset(df, Id <=1210)
df_test <- subset(df, Id >= 1211 & Id <= 1460 )
df_predict <- subset(df, Id>= 1461)

y<-log(df_train$SalePrice)

X<-model.matrix(Id~MSSubClass+
                  MSZoning	+
                  LotShape	+
                  LandContour	+
                  LotConfig	+
                  Condition1	+
                  Condition2	+
                  BldgType	+
                  RoofMatl +
                  HouseStyle	+
                  OverallCond	+
                  YearBuilt*TotalSF	+
                  YearRemodAdd * OverallCond	+
                  RoofStyle	+
                  Exterior1st	+
                  Exterior2nd	+
                  MasVnrType	+
                  ExterQual	+
                  ExterCond	+
                  Foundation	+
                  BsmtQual*OverallQual	+
                  BsmtQual*TotalBsmtSF	+
                  BsmtCond	+
                  BsmtExposure*GrLivArea	+
                  BsmtFinType1	+
                  BsmtFinType2	+
                  TotalBsmtSF	* BsmtQual+
                  Heating	+
                  HeatingQC	+
                  CentralAir	+
                  BsmtFullBath	+
                  FullBath*LotShape	+
                  HalfBath	+
                  BedroomAbvGr	+
                  KitchenAbvGr	+
                  KitchenQual*TotalSF	+
                  Functional*OverallQual	+
                  Functional*YearBuilt +
                  FireplaceQu*YearBuilt	+
                  GarageType	+
                  GarageYrBlt	+
                  GarageFinish	+
                  GarageArea	+
                  GarageQual	+
                  GarageCond	+
                  Neighborhood +
                  WoodDeckSF	+
                  PoolQC	+
                  MoSold	+
                  YrSold*YearBuilt	+
                  SaleCondition	+
                  HouseAge*CentralAir	+
                  YearSinceRemodel +
                  sqrt(BsmtUnfSF)* OverallCond	+
                  GrLivArea+
                  nthroot(GrLivArea,3)*Fireplaces*HouseStyle+
                  sqrt(LotFrontage)*GarageCars	+
                  log(MasVnrArea+1)	+
                  sqrt(Total_PorchSF)	+
                  TotalSF +
                  sqrt(TotalSF)* OverallQual	+
                  log(X1stFlrSF)+
                  log(LotArea)*OverallCond +
                  sqrt(BsmtFinSF1)	+
                  log(TotRmsAbvGrd)	+
                  log(WoodDeckSF+1) +
                  log(BsmtFinSF2+1) +
                  log(X1stFlrSF)*BsmtExposure +
                  log(X1stFlrSF)*ExterQual +
                  log(X2ndFlrSF+1)*YearRemodAdd +
                  log(X2ndFlrSF+1)+
                  log(PoolArea+1), df)[,-1]


X<-cbind(df$Id,X)

# split X into testing, trainig/holdout and prediction as before
X.training<-subset(X,X[,1]<=1210)
X.testing<-subset(X, (X[,1]>=1211 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>=1461)


#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-6,-4),ylim=c(0.01,0.035)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
mean(abs(lasso.testing-df_test$SalePrice)/df_test$SalePrice*100) #calculate and display MAPE

# comparing the performance on the testing set, LASSO is better, so use it for prediction
predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
write.csv(predicted.prices.log.i.lasso, file = "Predicted House Prices LOG INTERACTION LASSO.csv") # export the predicted prices into a CSV file

#ridge (alpha=0)
ridge.fit<-glmnet(x = X.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
mean(abs(ridge.testing-df_test$SalePrice)/df_test$SalePrice*100) 


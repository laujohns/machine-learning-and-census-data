#load necessary packages
library(caTools)
library(caret)

#set working directory
setwd("/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/code")

#load necessary data
final.data<-read.csv("/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets/final.data.csv")

dentist<-readOGR(dsn="/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets",layer="dentists_addresses")
dentist<-as.data.frame(dentist)

dentist_inblock<-readOGR(dsn="/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For app", layer="number dentists in each block AL")
summary(final.data$expenditure)#this dataset was created in QGIS to calculate the number of points (dentists) within each polygon

#explore data
head(final.data)

#number of dentists in each block: check out data
dentist_inblock$n_dentist<-as.numeric(dentist_inblock$PNTCNT)
head(dentist_inblock@data)
summary(dentist_inblock@data$PNTCNT)

#limit dataset to only features that will be included in machine learning algorithms
final.data2<-final.data[,c("geoid","expenditure","percmen5.19","percwom5.19","percwom20.39","percwom40.59","percwom60.plus","percmen20.39","percmen40.59","percmen60.plus","perchs","perchsged","percsomecoll","percbach", "percgrad","percins", "percnoins","perchip", "percwhite","percblack","percasian","percother","income")]
final.data2$income<-as.numeric(final.data2$income)
class(final.data2)
hist(final.data2$expenditure, 
     main="Distribution of Per Capital Block Group Level Expenditure", xlab="Per Capita Expenditure",breaks=30) #see historgram of block-group level expenditure

#check missing values
table(final.data2$expenditure==0)#only 24 blocks missing expenditure data
summary(final.data2$income==0)

#create complete dataset to run algorithms
final.data3<-na.omit(final.data2)#3341
head(final.data3)
summary(final.data3)
ncol(final.data3)
)
#make test and train datasets
#https://edumine.wordpress.com/2015/04/17/how-to-split-a-data-frame-in-r-with-over-a-million-observations-in-above-50-variables/
#http://machinelearningmastery.com/machine-learning-in-r-step-by-step/

#using caret package create test and train datasets
library(caret)
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(final.data3$expenditure, p=0.70, list=FALSE) #70% of data for train and 30% for test
# select 30% of the data for validation
validation <- final.data3[-validation_index,]
write.csv(validation, "test.dontuse.csv")

# use the remaining 70% of data to to train models
dataset <- final.data3[validation_index,]
head(dataset)
write.csv(dataset, "training.data.csv")

#call in test and train dataset
train<-read.csv(file="/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets/Train/training.data.csv")
test<-read.csv(file="/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets/Test dataset/test.dontuse.csv")

#summarize data
summary(train)

#hisogram of expenditure
hist(train$expenditure, breaks=20)
summary(train$expenditure)
table(train$expenditure)
hist(log1p(train$expenditure), breaks=30)#log-transform for LR

#correlation matrix of features
#check for multicollinearity --> should be all less than 1
head(train)
names(train)
features<-train[,c(4:ncol(train))]#create just features dataset
summary(features)
head(features)

#relabel feature names
names(features)
head(features)
names(features)<-c("Males:5-19yrs","Females:5-19yrs","Females:20-39yrs","Females:40-59yrs","Females:60+years",
                                         "Males:20-39yrs","Males:40-59yrs","Males:60+yrs","< High School","High School/GED","Some College",
                                         "Bachelors Degree","Graduate Degree","Health Insurance","No Health Insurance","Hispanic","White","Black/African American",
                                         "Asian","Other Race/Eth","Median HH Income")
names(features)
#call in libraries to create correlation matrix
library(reshape2)
library(ggplot2)

#make correlation matrix
c <- cor(features[2:ncol(features)]) #exclude first two columns
c #see matrix

melted_features <- melt(c)
head(melted_features)
library(ggplot2)
matrix<-ggplot(data = melted_features, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+xlab("Features")+ylab("Features")+
    labs(title = "Correlation Matrix")+
 theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),
       axis.text=element_text(size=12),
       axis.title=element_text(size=12,face="bold"),
      legend.text = element_text(size=9))
matrix
#based on correlation matrix remove: percent black and no health insurance

#normalize income in test and train dataset so between 0 and 1 like rest of data
mean(train$income)
sd(train$income)
min(train$income)
max(train$income)
train$income2=((train$income-1)/(2916-1))#x-min/max-min
summary(train)

mean(test$income)
sd(test$income)
min(test$income)
max(test$income)
test$income2=((test$income-1)/(2916-1))#x-min/max-min
summary(test)

#create data frames for test and train (exlude unimportant variables)
train.LR<-train[,c(3:17,19:23,25)]
test.LR<-test[,c(3:17,19:23,25)]

#############build Linear Regression model###############
#http://www.aaronschlegel.com/linear-regression-r-example/
#https://www.analyticsvidhya.com/blog/2014/12/caret-package-stop-solution-building-predictive-models/
require(caret)
library(caret)
head(train.LR)

#linear regression on training data
lmFit<-train(expenditure~., data=train.LR, method="lm")
warnings()#i think this is a multicollinearity issue
summary(lmFit)
#mean square error
mean(lmFit$residuals^2)

#This allows for estimation of parameter coefficients through resampling methods like cross validation, boosting etc. 
#While using these parameters, then entire data can be used for model building without splitting it
ctrl<-trainControl(method = "cv", number = 10)
lmCVFit<-train(expenditure~., data=train.LR, method="lm", trControl = ctrl, metric="Rsquared")
summary(lmCVFit)#see summary of cross-validation to see how performs (R2~0.7)

###Predict on test set
library(pROC)
pred<-predict(lmFit, test.LR)#predict on test set
postResample(pred,test.LR$expenditure)

#calculate R^2
y <- test.LR$expenditure
1 - sum((y-pred)^2)/sum((y-mean(y))^2)
#0.7016
#calculate MSE= sum((y-pred)^2)/n-p-1 (#predictors)
(sum((y-pred)^2))/(1000)#688.36
#RMSE
sqrt(673.908)#26.23


#plot actual versus predicted
#predicted vs actual plot
plt.2<-data.frame(actual=test$expenditure, pred)
head(plt.2)
ggplot()+geom_point(data=plt.2, aes(x=actual, y=predicted))+
  xlab("Actual")+ylab("Predicted")+
#+ labs(title = "Actual vs. Predicted Estimated Revenue:\nLinear Regression")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=9))+
  annotate("text", x = 350, y = 50, label = "RMSE=25.55\nR-squared=0.71", size=5,col="red")

############# Perform Group Lasso using gglasso and grpreg##################
#http://royr2.github.io/2014/04/15/GroupLasso.html
library(gglasso)
library(RColorBrewer)
library(zoo)
library(grpreg)
names(train.LR)

#create dataset of only x variables
X=train.LR[,c(2:ncol(train.LR))]#x variables
X=as.matrix(X)
summary(X)
names(X)

#create dataset of dependent variable
Y=train.LR[,1]#expenditure
summary(Y)

#Create groups
grp<-c(1,1,1,1,1,1,1,1,2,2,2,2,2,3,4,4,4,4,4,5) #create groups of variables
length(grp)

#group1:age
#group1:age
#group2:education
#group3:insurance
#group4:race
#group5:income

#fit group lasso on training data
fit.gg=gglasso(x=X,y=Y,group=grp,loss='ls')
plot(fit.gg)
coef.mat=fit$beta
print(coef.mat)
summary(fit)


###graph when the groups come into the equation
#Group1 enters the equation
g1=max(which(coef.mat[1,]==0))

#Group 2 enters the equation
g2=max(which(coef.mat[9,]==0))

#Group 3 enters the equation
g3=max(which(coef.mat[14,]==0))

#Group 4 enters the equation
g4=max(which(coef.mat[15,]==0))

#Group 5 enters the equation
g5=max(which(coef.mat[20,]==0))

summary(fit$b0)

#Coefficient Plot. Let's also use some nice colors
cols=brewer.pal(5,name="Set1")
par("mar")
par(mar=c(4,4,4,4))

plot(fit$b0,main="Coefficient vs Step",
     ylab="Intercept",xlab="Step (decreasing Lambda =>)",
     col=cols[1],
     xlim=c(-1,100),
     ylim=c(70,130),
     type="l",lwd=4)
grid()
par(new=T)
x=c(g1,g2,g3,g4,g5)
y=c(fit$b0[g1],fit$b0[g2],fit$b0[g3],fit$b0[g4],fit$b0[g5])

plot(x=x,y=y,pch=13,lwd=2,cex=2,col=cols[-1],
     xlim=c(-1,100),ylim=c(70,130),#can check bounds of fit with summary(fit$b0)
     xaxt='n',yaxt='n',xlab="",ylab="")

lmda=round(fit$lambda[c(g1,g2,g3,g4,g5)],2)
text(x=x-0.5,y=y+0.1,labels=c("Group1","Group2","Group3","Group4","Group5"),pos=3,cex=0.9)
text(x=x-0.5,y=y-0.1,labels=paste("Lambda\n=",lmda),pos=1,cex=0.8)

#The intercept is not penalized and hence is always present in the regression equation. 
#But as the plot above shows, each group enters the regression equation at a particular value of lambda. For example:
coef.mat[,c(1:20)]
#group 1:age
#group2:education
#group3:insurance
#group4:income

#Cross Validation--> use this to choose best lambda that minimizes MSE
fit.cv=cv.gglasso(x=X,y=Y,group=grp,nfolds=10)
plot(fit.cv)
summary(fit.cv)
fit.cv$gglasso.fit

#Pick the best Lambda
head(fit.cv$lambda)
lmbda=fit.cv$lambda.1se
lmbda.2<-fit.cv$lambda.min

(coefs=coef.gglasso(object=fit,s=lmbda))

#predict on test
names(test.LR)
X1=test.LR[,c(2:ncol(test.LR))]#x variables
X1=as.matrix(X1)
summary(X1)
names(X1)
Y1=test.LR[,1]#expenditure
summary(Y1)
summary(test.LR)
grp<-c(1,1,1,1,1,1,1,1,2,2,2,2,2,3,4,4,4,4,4,5)#re-assign groups

pred=gglasso(x=X1,y=Y1,group=grp,loss='ls', lambda = lmbda)#use best lambda and re-run on test set

coef.mat.1=pred$beta
print(coef.mat.1)
summary(pred)

#At best lambda get coefficients and fitted values
plt=cbind(Y1,predict.gglasso(object=fit.gg,newx=X1,s=lmbda,type='link'))

summary(plt)

#plot actual y vs. predicted y
#calculate MSE=sum(Y-Y')^2 and R2
plt<-as.data.frame(plt)

#calculate R^2
1 - sum((plt[,1]-plt[,2])^2)/sum((plt[,1]-mean(plt[,1]))^2)
#0.7016
#calculate MSE= sum((y-pred)^2)/n-p-1 (#predictors)
(sum((plt[,1]-plt[,2])^2))/(1000)#688.36
#RMSE:
sqrt( 691.3806)
#25.956

plt$actual<-plt[,1]
plt$predicted<-plt[,2]

library(ggplot2)
ggplot()+geom_point(data=plt, aes(x=actual, y=predicted))+
  xlab("Actual Dental Expenditure ($)")+ylab("Predicted Dental Expenditure ($)")+ 
  #labs(title = "Actual vs. Predicted Estimated Revenue:\nGroup Lasso")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=9))
  #annotate("text", x = 350, y = 50, label = "RMSE=$26.3\nR-squared=0.0.69", size=5,col="red")

### repeat using grpreg to see how results differ
names(X)<-c("Males:5-19yrs","Females:5-19yrs","Females:20-39yrs","Females:40-59yrs","Females:60+years",
            "Males:20-39yrs","Males:40-59yrs","Males:60+yrs","< High School","High School/GED","Some College",
            "Bachelors Degree","Graduate Degree","Health Insurance","Hispanic","White","Black/African American",
            "Asian","Other Race/Eth","Median HH Income")
fit<-grpreg(X, Y, grp, penalty="grLasso")

#check out lasso trace plot
par(mar = c(4,4,1,9) + 0.1)
plot(fit)
plot(fit, label=TRUE)#variables that enter the model early are the most predictive and 
#variables that enter the model later are less important.
coef(fit, lambda=5)#see coeff at lambda=5
print(fit)

#find best model
cvfit <- cv.grpreg(X, Y, grp, penalty="grLasso")
plot(cvfit$fit, labels=TRUE)
plot(cvfit$fit,legend.loc="topleft", legend=labs)

coef(cvfit)#The coefficients corresponding to the value of λ that minimizes the cross-validation error can be obtained
summary(cvfit$lambda.min)#find best lambda

#use best lambda from cross-validation and fit model on test set
pred<-predict(fit, X1, type="response", lambda=cvfit$lambda.min)#degault=0.001
actual<-test.LR$expenditure

#calculate R2 and MSE
mean((pred-actual)^2) #673.3231
mean((pred-actual)^2)**0.5 #25.95
1 - sum((pred-actual)^2)/sum((actual-mean(actual))^2)#0.70

#Create lasso trace plot
head(X1)
myColors <- c("red", "green", "blue", "yellow", "purple")
plot(cvfit$fit, legend.loc="topleft", col=myColors)
labs <- c("Sex by Age", "Educational Level", "Health Insurance", "Race/Ethnicity",
          "Income")

########## fit random forest model ############
#http://bigcomputing.blogspot.com/2014/10/an-example-of-using-random-forest-in.html
#https://rpubs.com/chengjiun/52658
#https://rstudio-pubs-static.s3.amazonaws.com/64455_df98186f15a64e0ba37177de8b4191fa.html
#http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
library(randomForest)
library(caret)

#inspect training data
summary(train)
names(train)
train.RF<-train[,c(3:17,19:23,25)]
names(train.RF)#need to get rid of no insurance
head(train.RF)

names(test.LR)<-c("Expenditure","Males:5-19yrs","Females:5-19yrs","Females:20-39yrs","Females:40-59yrs","Females:60+years",
                  "Males:20-39yrs","Males:40-59yrs","Males:60+yrs","< High School","High School/GED","Some College",
                  "Bachelors Degree","Graduate Degree","Health Insurance","Hispanic","White","Black/African American",
                  "Asian","Other Race/Eth","Median HH Income")

# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)#10-fold CV with 3 repeats
seed <- 7
metric <- "RSquare"
set.seed(seed)
rf_default <- train(expenditure~., data=train.LR, method="rf", metric=metric, trControl=control, importance=TRUE)
print(rf_default) #RMSE=24.65; RSquared=0.78

#random search tuning
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")#10-fold CV with 3 repeats
seed <- 7
metric <- "RSquared"
set.seed(seed)
rf_random <- train(expenditure~., data=train.RF, method="rf", metric=metric, tunelength=15, trControl=control, importance=T)
print(rf_random) 

#mtry  RMSE      Rsquared 
#8    24.25557  0.7745965

pred.1<-predict(rf_default, newdata=test.LR) #cross-validation looked good, predict on test data
#postResample(test.LR$expenditure,pred.1) #22.75, R^2=0.77
plt1<-as.matrix()

# estimate variable importance
importance <- varImp(rf_default, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

plt.3<-data.frame(actual=test$expenditure,predicted=pred.1) #create pred vs actual dataframe
head(plt.3)

#calculate R^2
1 - sum((plt.3[,1]-plt.3[,2])^2)/sum((plt.3[,1]-mean(plt.3[,1]))^2)
#0.7016
#calculate MSE= sum((y-pred)^2)/n-p-1 (#predictors)
(sum((plt.3[,1]-plt.3[,2])^2))/(1000)
sqrt(523.5434)#22.9

#plot predicted vs. actual
ggplot()+geom_point(data=plt.3, aes(x=actual, y=predicted))+
  xlab("Actual Dental Expenditure ($)")+ylab("Predicted Dental Expenditure ($)")+
  #labs(title = "Actual vs. Predicted Estimated Revenue:\nRandom Forest")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=9))
  annotate("text", x = 350, y = 50, label = "RMSE=$22.9\nR-squared=0.76", size=5,col="red")

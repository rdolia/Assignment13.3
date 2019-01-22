#import train data
train_data <-  read.csv("blogData_train.csv",1)
#a. Read the dataset and identify the right features.
#Columns 51,52,55,56,57,60,61,62,263 to 269, 270 to 276, 277, 278, 279,280, 281
#have been identified pdf attached describing attributes.
train_data1<- train_data[c(51,52,55,56,57,60,61,62,263:269, 270:276, 277, 278, 279,280, 281)]
#51,52,55,61,
#test data collated from 60 data test files.
test_data1<-read.csv('test.csv',1)
test_data1<- test_data1[c(51,52,55,56,57,60,61,62,263:269, 270:276, 277, 278, 279,280, 282)]
#a. Create a linear regression model to predict the number of comments
#in the next 24 hours (relative to base time).
#Model Building
model1 = lm(formula = X1.0.2 ~ .,data = train_data1)
summary(model1)
#. Fine tune the model and represent important features Visualize the
#dataset and make inferences from that.

#based on significance codes we remove all non star attributes.
model2 = lm(formula = X1.0.2~X2.0.1+X2.0.2+X2.0.4+X0.0.15+X10.0.1+X0.0.20+X0.0.222+X0.0.223+X0.0.224+X1.0+X0.0.225,data = train_data1)
summary(model2)
#removing X0.0.222
model3 = lm(formula = X1.0.2~X2.0.1+X2.0.2+X2.0.4+X0.0.15+X10.0.1+X0.0.20+X0.0.223+X0.0.224+X1.0+X0.0.225,data = train_data1)
summary(model3)
#removing x0.0.224
model4 = lm(formula = X1.0.2~X2.0.1+X2.0.2+X2.0.4+X0.0.15+X10.0.1+X0.0.20+X0.0.223+X1.0+X0.0.225,data = train_data1)
summary(model4)
#removing x0.0.225
model5 = lm(formula = X1.0.2~X2.0.1+X2.0.2+X2.0.4+X0.0.15+X10.0.1+X0.0.20+X0.0.223+X1.0,data = train_data1)
summary(model5)
#removing x0.0.223
model6 = lm(formula = X1.0.2~X2.0.1+X2.0.2+X2.0.4+X0.0.15+X10.0.1+X0.0.20+X1.0,data = train_data1)
summary(model6)
#removing X1.0
model7= lm(formula = X1.0.2~X2.0.1+X2.0.2+X2.0.4+X0.0.15+X10.0.1+X0.0.20,data = train_data1)
summary(model7)
#we have therefore fine tuned the model as all attributes have high 
#significance level of ***

#predicting with model7 and test.
colnames(test_data1) <- colnames(train_data1)
predict_1<-predict(model7,test_data1[,-27])
test_data1$Prediction<-predict_1

#Assumptions
plot(model7)

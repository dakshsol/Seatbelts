data()
data("Orange")
head(Orange)
install.packages('mlbench')
library("mlbench")
data('Seatbelts')

is.data.frame(Seatbelts)
df <- data.frame(Seatbelts)
df
is.data.frame(df)
head(df)
names(df)
View(df)
nrow(df)
ncol(df)
colnames(df)
row.names(df)
state.division(df)
summary(df)
str(df)
rowSums(is.na(df))
mean(Seatbelts)
mean(Seatbelts[1])
mean(df[,1])
median(df[,3])
median(df[1,])
min(df$DriversKilled)
max(df$VanKilled)
hist(df, x = DriversKilled)
hist(df$drivers)
hsit(df$PetrolPrice)
hist(df$PetrolPrice)
hist(df$DriversKilled , xlab = 'DriversKilled', ylab ='freq', col ='red')
plot(x= df$drivers ,y= df$kms , type = "l")
plot(x= df$drivers ,y= df$kms , type = "b")
plot(x= df$drivers ,y= df$kms , type = "p")

bx <- boxplot(y = df$law, x = df$kms) 
 set.seed(124)
nrow(df)


trainingIndex <- sample(1:nrow(df),size =0.75*nrow(df))
length(trainingIndex)
trainingset <- df[trainingIndex,]
testingset<- df[-trainingIndex,]
paste("TrainingSize :" , nrow(trainingset),"TestingSize :" ,nrow(testingset))                      


#### linear Regression Model#######
names(df)
model_fit <- lm(formula = DriversKilled ~.,
                data = trainingset)





y_predict <- predict(model_fit,testingset[-1])
testingset[1]
summary(model_fit)
plot(model_fit$residuals)
model_fit$coefficients

predict_and_real_y <-data.frame(cbind(Actual_y = testingset[1] , Predicted_y = y_predict))
cor(predict_and_real_y)



anova(model_fit)
summary(y_predict)



###Decision Tree Regression###
data(cars)
df<-as.data.frame(cars)
is.data.frame(cars)
head(df)
tail(df)
dim(df)
str(df)
summary(df)
hist(df$speed , col = 'red')
hist(df$dist,col='blue')
plot( x= df$speed, y= df$dist , type = 'p' )
plot(x=df$speed,y=df$dist, type = 'l')

set.seed(20)
nrow(df)
trainingIndex <- sample(1 : nrow(df), size = 0.70 * nrow(df))
trainingIndex


length(trainingIndex)
 trainingset <- df[trainingIndex,]
testingset<-df[-trainingIndex,] 


library(rpart)

model_fit<-rpart(formula = speed~.,data =trainingset,
                 control = rpart.control(minsplit = 2))

summary(model_fit)

y_predict<-predict(model_fit,testingset[-1])

predict_and_real_y <- data.frame(cbind(Actual_y = testingset , Predicted_y =y_predict))
cor(predict_and_real_y)

plot(model_fit)
text(model_fit)



install.packages("randomForest")
library(randomForest)



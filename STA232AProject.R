setwd("C:/Users/yanha/Dropbox/Statistics at UC Davis/STA 232A/Final Project")
mpgdata <- read.csv("auto-mpg.txt",sep = "",header = FALSE)
summary(mpgdata)
## Packages
library(tidyverse)
library(scales)
library(GGally)
library(cowplot)
loadNamespace("cowplot")
library(data.table)
library(foreach)
library(MASS)
library(DAAG)
library(caret)
library(Metrics)
library(fitdistrplus)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(visreg)
library(randomForest)

## some missing data (I delete them)
miss_idx <- which(mpgdata$V4 == "?")
mpgdata <- mpgdata[-miss_idx,]
mpgdata$V4 <- as.numeric(mpgdata$V4)
summary(mpgdata)
colnames(mpgdata) <- c("mpg","cylinders","displacement","horsepower","weight","acceleration","year","origin","name")

## cylinders and years should be integer numbers
mpgdata$cylinders <- as.factor(mpgdata$cylinders)
mpgdata$year <- as.factor(mpgdata$year)
mpgdata$origin <- as.factor(mpgdata$origin)
summary(mpgdata)
pairs(mpgdata[,1:7])

################## EDA ###################
# distirbution of all predictors

mpghist<-ggplot(data=mpgdata,aes(mpg, fill=cylinders)) +
  geom_histogram(aes(y =..density..),
                 bins=30,
                 color="black")+
  labs(title="Histogram for mpg",
       x= "mpg")

mpghist
disphist<-ggplot(data=mpgdata,aes(x=displacement))+
  geom_histogram(aes(y =..density..),
                 bins=30,
                 color="black", 
                 fill="dodgerblue") +
  labs(title="Histogram for displacement",
       x= "displacement")+
  geom_density(col=3)


hphist<-ggplot(data=mpgdata,aes(x=horsepower))+
  geom_histogram(aes(y =..density..),
                 bins=30,
                 color="black", 
                 fill="dodgerblue") +
  labs(title="Histogram for horsepower",
       x= "horsepower")+
  geom_density(col=3)


acchist<-ggplot(data=mpgdata,aes(x=acceleration))+
  geom_histogram(aes(y =..density..),
                 bins=30,
                 color="black", 
                 fill="dodgerblue") +
  labs(title="Histogram for acceleration",
       x= "acceleration")+
  geom_density(col=3)

weighthist<-ggplot(data=mpgdata,aes(x=weight))+
  geom_histogram(aes(y =..density..),
                 bins=30,
                 color="black", 
                 fill="dodgerblue") +
  labs(title="Histogram for weight",
       x= "weight")+
  geom_density(col=3)

ggplot(mpgdata, aes(x = acceleration, y = mpg)) +
  geom_point()


figure <- ggarrange(mpghist,disphist,hphist,acchist,weighthist,ncol = 3, nrow = 2)
figure
## Accelration data is normaly distributed. Rest are right skewed.





## Data splitting
# set training and test dataset
set.seed(666)
train.obs <- sample(nrow(mpgsd), 0.75 * nrow(mpgdata))
auto.train <- mpgsd[train.obs, ]
auto.test <- mpgsd[-train.obs, ]
# Boxcox transformation
bc <- boxcox(mpg~.,data=mpgdata)
lambda<-bc$x[which.max(bc$y)]
lambda
IsNonFactor <- which(sapply(mpgdata[,-9], is.factor) == FALSE)
IsNonFactor
mpglog <- mpgdata[,-9]
mpglog$mpg <- log(mpglog$mpg)
mpglogsd <-mpglog
mpglogsd[, c(3,4,5,6)] <- sapply(mpglog[, c(3,4,5,6)], scale)
summary(mpglog)
summary(mpglogsd)
summary(mpgdata)
auto.train2 <- mpglogsd[train.obs, ]
auto.test2 <- mpglogsd[-train.obs, ]


lm_full_log <- lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin,data = auto.train2)
par(mfrow=c(2,2))
summary(lm_full_log)
plot(lm_full_log)
lm_0_log <- lm(mpg~1,data = auto.train2)
model.log <- stepAIC(lm_0_log, scope = list(upper = Response ~ cylinders+displacement+horsepower+weight+acceleration+year+origin, lower = Response ~ 1), direction = c("both"))
model.log2 <- stepAIC(lm_full_log, scope = list(upper = Response ~ cylinders+displacement+horsepower+weight+acceleration+year+origin, lower = Response ~ 1), direction = c("both"))
lm4 <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin,data=auto.train2)
lm5 <- lm(mpg ~ weight + year + cylinders + horsepower + origin + displacement,data=auto.train2)
lm6 <- lm(mpg ~ weight + year + cylinders + horsepower + origin,data=auto.train2)
lm7 <- lm(mpg ~ weight + year + cylinders + horsepower, data=auto.train2)

lm4$coefficients
lm5$coefficients
lm6$coefficients
lm7$coefficients
summary(lm6)
anova(lm6,lm7)

predict_4 <- predict(lm4,auto.train2)
predict_5 <- predict(lm5,auto.train2)
predict_6 <- predict(lm6,auto.train2)
predict_7 <- predict(lm7,auto.train2)
c(rmse(auto.train2$mpg,predict_4),rmse(auto.train2$mpg,predict_5),rmse(auto.train2$mpg,predict_6),rmse(auto.train2$mpg,predict_7))
predict4 <- predict(lm4,auto.test2)
predict5 <- predict(lm5,auto.test2)
predict6 <- predict(lm6,auto.test2)
predict7 <- predict(lm7,auto.test2)
c(rmse(auto.test2$mpg,predict4),rmse(auto.test2$mpg,predict5),rmse(auto.test2$mpg,predict6),rmse(auto.test2$mpg,predict7))
plot(lm6)


# generalized linear model with gamma distribution
auto.train3 <- mpglogsd[train.obs, ]
auto.test3 <- mpglogsd[-train.obs, ]

glm1 <-glm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin,data = auto.train3,family = Gamma(link = "identity"))
glm2 <-glm(mpg ~ cylinders + displacement + horsepower + weight + year + origin, data = auto.train3,family = Gamma(link = "identity"))
glm3 <-glm(mpg ~ cylinders + horsepower + weight + year + origin, data = auto.train3,family = Gamma(link = "identity"))
# Calculate RMSE on training set
predict_g1 <- predict(glm1,auto.train3)
predict_g2 <- predict(glm2,auto.train3)
predict_g3 <- predict(glm3,auto.train3)
c(rmse(auto.train3$mpg,predict_g1),rmse(auto.train3$mpg,predict_g2),rmse(auto.train3$mpg,predict_g3))
# Calculate RMSE on test set
predictg1 <- predict(glm1,auto.test3)
predictg2 <- predict(glm2,auto.test3)
predictg3 <- predict(glm3,auto.test3)
c(rmse(auto.test3$mpg,predictg1),rmse(auto.test3$mpg,predictg2),rmse(auto.test3$mpg,predictg3))
plot(glm2)
## Final best model would be lm6

# Random Forest
tuneGrid <- expand.grid(
  mtry = c(1:7)
)
ctrl <- trainControl(
  method = "cv",
  number = 7,
)
for (nt in seq(100,1000,by=100)){
  model5 <- train(
    mpg ~ .,
    data = auto.train2,
    method = 'rf',
    ntree = nt,
    trControl = ctrl,
    tuneGrid = tuneGrid
  )
  predict_rf<-predict(model5,newdata=auto.test2)
  print(rmse(auto.test2$mpg,predict_rf))
}

## Check the prediction behavior
pa_train <- data.frame(cbind(predict_6,auto.train2$mpg))
colnames(pa_train) <- c("Predicted_mpg","Actual_mpg")
scatplot_train <- ggplot(data=pa_train,aes(x=Actual_mpg,y=Predicted_mpg))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm) +
  labs(title="Actual/Predicted Plot for Training Data",x= "Actual mpg", y="Predicted mpg")
pa_test <- data.frame(cbind(predict6,auto.test2$mpg))
colnames(pa_test) <- c("Predicted_mpg","Actual_mpg")
scatplot_test <- ggplot(data=pa_test,aes(x=Actual_mpg,y=Predicted_mpg))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm) +
  labs(title="Actual/Predicted Plot for Testing Data",x= "Actual mpg", y="Predicted mpg")
scatplot_test
figure2 <- ggarrange(scatplot_train,scatplot_test,ncol = 2, nrow = 1)
figure2

plot(lm6)


### Simpson Paradox and causal relationship
summary(mpgdata)
accdata <- mpgdata[,c(1,2,3,4)]
accdata
acc_clu <- kmeans(accdata[c(2,3,4)], centers = 4)
clu_idx1 <- which(as.vector(acc_clu$cluster) == 1)
clu_idx2 <- which(as.vector(acc_clu$cluster) == 2)
clu_idx3 <- which(as.vector(acc_clu$cluster) == 3)
clu_idx4 <- which(as.vector(acc_clu$cluster) == 4)
ggplot()+
  geom_point(alpha=0.5,data = mpgdata[clu_idx1,c(1,6)],aes(x=acceleration,y=mpg),color="red") +
  geom_smooth(method=lm,data = mpgdata[clu_idx1,c(1,6)],aes(x=acceleration,y=mpg),color="red") +
  geom_point(alpha=0.5,data = mpgdata[clu_idx2,c(1,6)],aes(x=acceleration,y=mpg),color="blue") +
  geom_smooth(method=lm,data = mpgdata[clu_idx2,c(1,6)],aes(x=acceleration,y=mpg),color="blue") +
  geom_point(alpha=0.5,data = mpgdata[clu_idx3,c(1,6)],aes(x=acceleration,y=mpg),color="green") +
  geom_smooth(method=lm,data = mpgdata[clu_idx3,c(1,6)],aes(x=acceleration,y=mpg),color="green") +
  geom_point(alpha=0.5,data = mpgdata[clu_idx4,c(1,6)],aes(x=acceleration,y=mpg),color="orange") +
  geom_smooth(method=lm,data = mpgdata[clu_idx4,c(1,6)],aes(x=acceleration,y=mpg),color="orange") 

weightdata <- mpgdata[,c(1,2,3,4)]
weightdata
wei_clu <- kmeans(weightdata[c(2,3,4)], centers = 3)
cluidx1 <- which(as.vector(wei_clu$cluster) == 1)
cluidx2 <- which(as.vector(wei_clu$cluster) == 2)
cluidx3 <- which(as.vector(wei_clu$cluster) == 3)
ggplot()+
  geom_point(alpha=0.5,data = mpgdata[cluidx1,c(1,5)],aes(x=weight,y=mpg),color="red") +
  geom_smooth(method=lm,data = mpgdata[cluidx1,c(1,5)],aes(x=weight,y=mpg),color="red") +
  geom_point(alpha=0.5,data = mpgdata[cluidx2,c(1,5)],aes(x=weight,y=mpg),color="blue") +
  geom_smooth(method=lm,data = mpgdata[cluidx2,c(1,5)],aes(x=weight,y=mpg),color="blue") +
  geom_point(alpha=0.5,data = mpgdata[cluidx3,c(1,5)],aes(x=weight,y=mpg),color="green") +
  geom_smooth(method=lm,data = mpgdata[cluidx3,c(1,5)],aes(x=weight,y=mpg),color="green")

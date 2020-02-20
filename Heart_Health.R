install.packages("GGally")
library(ggplot2)
library(caret)
library(GGally)
library(ggthemes)
library(broom)
library(dplyr)
library(bindrcpp)
library(caTools)
library(rattle)
library(RColorBrewer)
library(nnet)
library(rpart.plot)

heart <- read.csv(file.choose())

sum(is.na(heart))

heart$sex<-as.factor(heart$sex)
heart$cp<-as.factor(heart$cp)
heart$fbs<-as.factor(heart$fbs)
heart$exang<-as.factor(heart$exang)
heart$restecg<-as.factor(heart$restecg)
heart$slope<-as.factor(heart$slope)
heart$thal<-as.factor(heart$thal)
heart$target<-as.factor(heart$target)
str(heart)
levels(heart$sex)[levels(heart$sex)==0] <- "Female"
levels(heart$sex)[levels(heart$sex)==1] <- "Male"
levels(heart$fbs)[levels(heart$fbs)==0] <- "Fasting Blood Sugar <= 120"
levels(heart$fbs)[levels(heart$fbs)==1] <- "Fasting Blood Sugar > 120"
levels(heart$thal)[levels(heart$thal)==0] <- "No Thalassemia"
levels(heart$thal)[levels(heart$thal)==1] <- "Normal Thalassemia"
levels(heart$thal)[levels(heart$thal)==2] <- "Fixed Defect Thalassemia"
levels(heart$thal)[levels(heart$thal)==3] <- "Reversible Defect Thalassemia"
levels(heart$target)[levels(heart$target)==0] <- "Healthy"
levels(heart$target)[levels(heart$target)==1] <- "Heart Disease"
levels(heart$exang)[levels(heart$exang)==1] <- "Exercise Induced Angina"
levels(heart$exang)[levels(heart$exang)==0] <- "No Exercise Induced Angina"
levels(heart$cp)[levels(heart$cp)==0] <- "Chest Pain Type 0"
levels(heart$cp)[levels(heart$cp)==1] <- "Chest Pain Type 1"
levels(heart$cp)[levels(heart$cp)==2] <- "Chest Pain Type 2"
levels(heart$cp)[levels(heart$cp)==3] <- "Chest Pain Type 3"
levels(heart$restecg)[levels(heart$restecg)==0] <- "Rest ECG 0"
levels(heart$restecg)[levels(heart$restecg)==1] <- "Rest ECG 1"
levels(heart$restecg)[levels(heart$restecg)==2] <- "Rest ECG 2"
levels(heart$slope)[levels(heart$slope)==0] <- "Peak Excercise ST Slope 0"
levels(heart$slope)[levels(heart$slope)==1] <- "Peak Excercise ST Slope 1"
levels(heart$slope)[levels(heart$slope)==2] <- "Peak Excercise ST Slope 2"
sum(is.na(heart))
summary(heart)
age<-heart$..age


ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  theme_economist()  +
  scale_fill_manual(values=c("green","red")) 


ggplot(heart,aes(age, fill=target)) +
  geom_histogram(aes(y=..density..),breaks=seq(0, 80, by=1), color="grey17") +
  geom_density(alpha=.1, fill="black")+
  facet_wrap(~target, ncol=1,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("green","red"))+
  xlab("Age") +
  ylab("Density / Count") +
  ggtitle("Age Histogram")

ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~cp, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("green","red"))

ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~sex, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("green","red")) 

ggplot(heart,aes(target, fill=target)) +
  geom_bar(stat="count") +
  facet_wrap(~thal, ncol=2,scale="fixed") +
  theme_economist()  +
  scale_fill_manual(values=c("green","red"))


#############################  Logistic Model   ############################

log<-glm(target~., data=heart, family=binomial)
summary(log)

d<-heart[,c(2,3,9,10,12,14)]
summary(d)
log_d<-glm(target~., data=d, family=binomial)
summary(log_d)

log.df<-tidy(log_d)

########################## Random Forest #################################

data<-d
set.seed(1237)
train <- sample(nrow(data), .8*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]

gbm.ada.1 <- caret::train(target ~ ., 
                          data = TrainSet ,
                          method = "rf", 
                          trControl = fitControl,
                          metric="ROC")



## Tuning parameters ##
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)


TrainSet$target<-make.names(TrainSet$target)
set.seed(142)
TrainSet$target<-as.factor(TrainSet$target)


Random_forest <- caret::train(target ~ ., 
                          data = TrainSet ,
                          method = "rf", 
                          trControl = fitControl,
                          metric="ROC")

Random_forest


############################################################

varImp(gbm.ada.1)

pred <- predict(gbm.ada.1,ValidSet)
levels(pred)[2] <- "Heart Disease"
t<-table(ValidSet$target, pred)
t.df<-as.data.frame(t)
res<-caret::confusionMatrix(t, positive="Heart Disease")
res

ggplot(data = t.df, aes(x = Var1, y = pred, label=Freq)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low="firebrick2", high="springgreen2") +
  theme_economist() +
  xlab("Actual Heart Disease") +
  ylab("Predicted Heart Disease") +
  geom_text(size=8) +
  ggtitle("Random Forest")

gbmGrid <-  expand.grid(cp=c(0.01))
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
d$target<-make.names(d$target)
system.time(gbm.ada.1 <- caret::train(target ~ ., 
                                      data = d ,
                                      method = "rpart", 
                                      trControl = fitControl,
                                      metric="ROC",
                                      tuneGrid=gbmGrid))


gbm.ada.1

varImp(gbm.ada.1)


rpart.plot(gbm.ada.1$finalModel,   
           type=5,
           fallen.leaves = FALSE,
           box.palette = "GnRd",
           nn=TRUE)

##########################################################################################

log.df %>%
  mutate(term=reorder(term,estimate)) %>%
  ggplot( aes(term,estimate, fill=estimate)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low="firebrick2", high="springgreen2") +
  theme_economist() +
  geom_hline(yintercept=0) +
  coord_flip()

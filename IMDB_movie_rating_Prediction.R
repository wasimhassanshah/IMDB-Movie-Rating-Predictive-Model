rm(list=ls())

# 
# install.packages('dplyr')
# 
# install.packages('GGally')
# 
# install.packages('ggplot2')
# 
# install.packages('tree')
# install.packages('rpart')
# install.packages('rpart.plot')

install.packages('caret')
require(caret)
library(ggplot2)
library(GGally)
library(dplyr)

library(tree)
library(rpart)
library(rpart.plot)


#Our Dataset

movies <- read.csv(file="E:\\Kddm\\Final Project\\movie_metadata.csv",header = T,stringsAsFactors = F)

View(movies)

#We look for missing data, if there are, then in what proportion?
sum(is.na(movies))
colSums(is.na(movies))

mean(is.na(movies))

#not much, only 1%, we are safe to delete them.

#we don't need any of the character or factor variables for prediction, so we will discard them.
columns <- c()

for(i in 1:dim(movies)[2])
{
  if(is.numeric(movies[,i])|| is.integer(movies[,i]))
  {
    columns[i]=T
  }
  else
  {
    columns[i]=F
  }
}
movie_data <- na.omit(movies[,columns])

View(movie_data)


#How is the response variable distributed?

require(ggplot2)

ggplot(movie_data, aes(x=imdb_score)) + geom_histogram()


knn_movie_data <- movie_data


#knn_movie_data$imdb_score <-ifelse(knn_movie_data$imdb_score <=5,'Low',
#ifelse(knn_movie_data$imdb_score >5 & knn_movie_data$imdb_score <=7,'Medium','High'))



knn_movie_data$imdb_score <-ifelse(knn_movie_data$imdb_score <=7,'Low','High')




View(knn_movie_data)

summary(knn_movie_data)



#Using all the variables

#Normalizing the data
normalize <- function(x) {
  + return((x-min(x))/ (max(x) - min(x)))
  
}

data_knn_temp <- as.data.frame(lapply(knn_movie_data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16)], normalize))

summary(data_knn_temp)

View(data_knn_temp)

#binding the columns

data_knn_temp <- cbind(data_knn_temp,knn_movie_data$imdb_score)

colnames(data_knn_temp)[16] <- "imdb_score"

View(data_knn_temp)

#data_new_temp <- data_knn_temp

set.seed(9850)

g <- runif(nrow(data_knn_temp))

data_new_temp <- data_knn_temp[order(g),]

str(data_new_temp)

View(data_new_temp)




#Applying KNN using all the variables


#Actual Data

idx_test_1_temp <- data_new_temp[seq(1, nrow(data_new_temp), by = 5),]

idx_test_1_temp

nrow(idx_test_1_temp)



idx_train_1_temp <- data_new_temp[-seq(1, nrow(data_new_temp), by = 5),]

idx_train_1_temp

nrow(idx_train_1_temp)


test_1_temp <- idx_test_1_temp[,-16]
training_1_temp <- idx_train_1_temp[,-16]



#Target Data




training_target_temp <- idx_train_1_temp[,16]

test_target_temp <- idx_test_1_temp[,16]


library(class)
require(class)


#Use knn with k=15 and classify the test dataset

#Measuring the performance with k=5

predict_temp <- knn(training_1_temp, test_1_temp, training_target_temp, k=5)


tab_temp <- table(Actual=test_target_temp,Predict=predict_temp)


#Accuracy
mean(predict_temp == test_target_temp)




#Measuring the performance of knn with k=15

predict_temp <- knn(training_1_temp, test_1_temp, training_target_temp, k=15)


tab_temp <- table(Actual=test_target_temp,Predict=predict_temp)


#Accuracy
mean(predict_temp == test_target_temp)


accuracy <- rep(0, 10)
k <- 1:50
for(x in k){
  predict_temp <- knn(training_1_temp, test_1_temp, training_target_temp, k=x)
  accuracy[x] <- mean(predict_temp == test_target_temp)
}


plot(k, accuracy, type = 'b')

#Accuracy
postResample(test_target_temp, predict_temp)

#Missclassification rate

1-sum(diag(tab_temp))/sum(tab_temp)


#Mean Square error

predict_temp <- as.numeric(predict_temp)

test_target_temp <- as.numeric(test_target_temp)

mean(test_target_temp - predict_temp)^2



install.packages("corrplot")

require(corrplot)

# Correlation between numeric values
correlation_graph <- cor(movie_data[sapply(movie_data, is.numeric)])
corrplot(correlation_graph, method="shade")


#How are the variables correlated to the response variable?

correlation <- c()
for(i in 1:dim(movie_data)[2])
{
  correlation[i] <- cor(movie_data[,i],movie_data[,'imdb_score'])
}

correlation


#Scatter plot to Visualize correlation

ggplot(movie_data, aes(x=num_voted_users, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[7]))

ggplot(movie_data, aes(x=duration, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[2]))

ggplot(movie_data, aes(x=gross, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[6]))

ggplot(movie_data, aes(x=num_critic_for_reviews, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[1]))

ggplot(movie_data, aes(x=num_user_for_reviews, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[10]))

ggplot(movie_data, aes(x=movie_facebook_likes, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[16]))

ggplot(movie_data, aes(x=movie_facebook_likes, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[3]))

ggplot(movie_data, aes(x=movie_facebook_likes, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[8]))

ggplot(movie_data, aes(x=movie_facebook_likes, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[12]))


#As per the correlation , selecting the respective importance variables

summary(knn_movie_data)

data <- knn_movie_data[,c(1,2,3,6,7,8,10,12,16)]

View(data)

#Normalizing the data
normalize <- function(x) {
  + return((x-min(x))/ (max(x) - min(x)))
  
}

data <- as.data.frame(lapply(data[,c(1,2,3,4,5,6,7,8,9)], normalize))

summary(data)

View(data)

#binding the columns

data_1 <- cbind(data,knn_movie_data$imdb_score)

colnames(data_1)[10] <- "imdb_score"

View(data_1)

#data_new <- data_1

set.seed(9850)

g <- runif(nrow(data_1))

data_new <- data_1[order(g),]

str(data_new)

View(data_new)

#Actual Data

idx_test_1 <- data_new[seq(1, nrow(data_new), by = 5),]

idx_test_1

nrow(idx_test_1)



idx_train_1 <- data_new[-seq(1, nrow(data_new), by = 5),]

idx_train_1

nrow(idx_train_1)


test_1 <- idx_test_1[,-10]

training_1 <- idx_train_1[,-10]



#Target Data




training_target <- idx_train_1[,10]

test_target <- idx_test_1[,10]


library(class)
require(class)


#Use knn with k=15 and classify the test dataset


#Measuring the performance of knn with k=5

predict <- knn(training_1, test_1, training_target, k=5)


tab <- table(Actual=test_target,Predict=predict)


#Accuracy
mean(predict == test_target)

#Measuring the performance of knn with k=15

predict <- knn(training_1, test_1, training_target, k=15)


tab <- table(Actual=test_target,Predict=predict)


#Accuracy
mean(predict == test_target)


accuracy <- rep(0, 10)
k <- 1:50
for(x in k){
  predict <- knn(training_1, test_1, training_target, k=x)
  accuracy[x] <- mean(predict == test_target)
}


plot(k, accuracy, type = 'b')

#Accuracy
postResample(test_target, predict)

#Missclassification rate

1-sum(diag(tab))/sum(tab)


#Mean Square error

predict <- as.numeric(predict)

test_target <- as.numeric(test_target)

mean(test_target - predict)^2




#Regression tree


data_tree <- knn_movie_data[,c(1,2,3,6,7,8,10,12,16)]

data_1_tree <- cbind(data_tree,knn_movie_data$imdb_score)

colnames(data_1_tree)[10] <- "imdb_score"

View(data_1_tree)

#data_new <- data_1

set.seed(9850)

g <- runif(nrow(data_1_tree))

data_new_tree <- data_1_tree[order(g),]

str(data_new_tree)




View(data_new_tree)
head(data_new_tree)

nrow(data_new_tree)


# install.packages("rpart")
# 
# install.packages("rpart.plot")
# install.packages("rattle")
# install.packages("RColorBrewer")
require(rpart)

train_tree <- data_new_tree[seq(1,3000),] 

test_tree <- data_new_tree[seq(3001,3801),]

View(train_tree)

View(test_tree)


Regtree_1 <- rpart(imdb_score ~ ., data=train_tree, method="class")
Regtree_1

summary(Regtree_1)

plot(Regtree_1)
text(Regtree_1, pretty=0)

require(rpart.plot)
rpart.plot(Regtree_1)

rpart.plot(Regtree_1, type=3, extra=101, fallen.leaves=T)


prp(Regtree_1)


require(rattle)

#Plotting the Final tree using rpart package.

fancyRpartPlot(Regtree_1)


#test_reg <- subset(test, select=c("imdb_score"))

#View(test_reg)


test_reg_tree <- data_new_tree[seq(3001,3801),10]

#test_reg <- test[,10]

View(test_reg_tree)


#Testing the model

regpredict_1 <- predict(Regtree_1, test_tree, type="class")

table(test_reg_tree, predicted=regpredict_1)



#Accuracy
mean(test_reg_tree==regpredict_1)

sum(test_reg_tree==regpredict_1)/length(regpredict_1)


wrong_tree<-regpredict_1!=test_reg_tree

#Classification Error rate
rate_tree<-sum(wrong_tree)/length(wrong_tree)
rate_tree

mean(test_reg_tree!=regpredict_1)

regpredict_1<-as.numeric(regpredict_1)
test_reg_tree<-as.numeric(test_reg_tree)

#Mean Square error
mean(regpredict_1-test_reg_tree)^2

cor(regpredict_1,test_reg_tree)




#pruning the tree and optimizing the accuracy of the tree model

## cross validation  to check where to stop pruning

printcp(Regtree_1)

plotcp(Regtree_1)

set.seed(1000)
Regtree_temp_1 <- rpart(imdb_score ~ ., data=train_tree, method="class", cp=0.001)
Regtree_temp_1

fancyRpartPlot(Regtree_temp_1)
printcp(Regtree_temp_1)


plotcp(Regtree_temp_1)

prune_predict_1_tree <- predict(Regtree_temp_1, test_tree, type="class")

table(test_reg_tree,prune_predict_1_tree)

prune_predict_1_tree <- as.numeric(prune_predict_1_tree)

wrong_prune_tree<-prune_predict_1_tree!=test_reg_tree

#Classification Error rate
rate_prune_tree<-sum(wrong_prune_tree)/length(wrong_prune_tree)
rate_prune_tree

#Accuracy
mean(test_reg_tree==prune_predict_1_tree)



bestcp_tree <- Regtree_temp_1$cptable[which.min(Regtree_temp_1$cptable[,"xerror"]),"CP"]

set.seed(1000)
tree.pruned_tree <- prune(Regtree_temp_1, cp = bestcp_tree)

fancyRpartPlot(tree.pruned_tree)

printcp(tree.pruned_tree)

plotcp(tree.pruned_tree)

prune_predict_2_tree <- predict(tree.pruned_tree , test_tree, type="class")

table(test_reg_tree,predicted=prune_predict_2_tree)


prune_predict_2_tree <- as.numeric(prune_predict_2_tree)
#Accuracy
mean(test_reg_tree==prune_predict_2_tree)

wrong_prune_predict_tree<-prune_predict_2_tree!=test_reg_tree

#Classification Error rate
rate_prune_predict_tree<-sum(wrong_prune_predict_tree)/length(wrong_prune_predict_tree)
rate_prune_predict_tree





#Random Forest


set.seed(500)
# 
# install.packages("randomForest")


library(randomForest)

set.seed(500)
rf_first <- randomForest(imdb_score~.,data=train_tree)

plot(rf_first)

pred_rf_first <- predict(rf_first, test_tree)

table(test_reg_tree,predicted=pred_rf_first)



pred_rf_first<-as.numeric(pred_rf_first)


#Mean Square error
mean(pred_rf_first- test_reg_tree)^2

#Accuracy
mean(pred_rf_first==test_reg_tree)



#With parameter ntree=500
rf <- randomForest(imdb_score~.,data=train_tree ,ntree=500,mtry=floor(dim(data_new)[2]/3))

plot(rf)

pred_rf <- predict(rf, test_tree)

table(test_reg_tree,predicted=pred_rf)



pred_rf<-as.numeric(pred_rf)


#Mean Square error
mean(pred_rf- test_reg_tree)^2

#Accuracy
mean(pred_rf==test_reg_tree)

postResample(pred_rf, test_reg_tree)

wrong_rf_algo<-pred_rf!=test_reg_tree

#Classification Error rate
rate_rf_algo<-sum(wrong_rf_algo)/length(wrong_rf_algo)
rate_rf_algo


#Tuning the parameter 'ntree'
is.numeric(test_reg_tree)


array_ntree<- c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500)
mse <- c()

j<-1
for(i in array_ntree)
{ set.seed(500)
  rf <- randomForest(imdb_score~.,data=train_tree,ntree=i)
  pred_rf <- predict(rf,test_tree)
  pred_rf <- as.numeric(pred_rf)
  mse[j]<-mean((pred_rf-test_reg_tree)^2)
  j=j+1
  
}

# plot the MSE against the number of trees. We will use the value of ntree with the minimum MSE


data_mse <- data.frame(array_ntree,mse)
ggplot(data_mse, aes(x=(array_ntree), y=mse)) + geom_line() + geom_point()


#By changing the ntree parameter

set.seed(500)
rf_tune <- randomForest(imdb_score~.,data=train_tree ,ntree=600,mtry=floor(dim(data_new)[2]/3))

pred_rf_tune <- predict(rf_tune, test_tree)

table(test_reg_tree,predicted=pred_rf_tune)



pred_rf_tune<-as.numeric(pred_rf_tune)


#Mean Square error
mean(pred_rf_tune- test_reg_tree)^2

#Accuracy
mean(pred_rf_tune==test_reg_tree)

postResample(pred_rf_tune, test_reg_tree)

wrong_rf<-pred_rf_tune!=test_reg_tree

rate_rf<-sum(wrong_rf)/length(wrong_rf)
rate_rf
plot(rf_tune)

varImpPlot(rf)



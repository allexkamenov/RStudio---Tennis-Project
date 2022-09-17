#read the .csv files
tennis1 = read.csv("AusOpen-women-2013.csv")
tennis2 = read.csv("FrenchOpen-women-2013.csv")
tennis3 = read.csv("Wimbledon-women-2013.csv")

#check the working directory
getwd()

#getting sense of the data
drop(tennis1)
drop(tennis2)
drop(tennis3)
table(tennis1$Round)
table(tennis2$Round)
#dataset tennis3 is missing 5 round two matches in the raw data
table(tennis3$Round)

#checking the number of variables and observations in each data set 
dim(tennis1)
dim(tennis2)
dim(tennis3)

#summary statistics - most variables are numeric so this doesn't give us much information
str(tennis1)
#checking for NA values
summary(tennis1)
#varaible ST3.3 has the most missing values and this could be because some of the matches did not go into a 3rd set
str(tennis2)
summary(tennis2)
#TPW.1 and TPW.2 are missing in this data set, unlike the rest
str(tennis3)
summary(tennis3)
#looking at missing values for each dattaset again
which(is.na(tennis1$ST5.2)
      
#EDa
library(ggplot2)
library(hexbin)

ggplot(tennis1, aes(x = WNR.1, y = WNR.2)) + geom_hex(bins = 25)
cor(tennis1$WNR.1, tennis1$WNR.2)
#scatter ploth winners won from player 1 against winners won by player 2
ggplot(tennis1, aes(x = WNR.1, y = WNR.2)) + geom_point()
#pointing the variable Round as a colour in the scatter ploth
ggplot(tennis_merged, aes(x = WNR.1, y = WNR.2, colour = Round)) + geom_point()
#bigger size due to amount of matches that had similar results
ggplot(tennis1, aes(x = WNR.1, y = WNR.2)) + geom_count(alpha = 0.5)

#scatter ploth break points won by player 1 against break points won by player 2
ggplot(tennis2, aes(x = BPW.1, y = BPW.2)) + geom_point()
#pointing the variable Round as a colour in the scatter ploth
ggplot(tennis2, aes(x = BPW.1, y = BPW.2, colour = Round)) + geom_point()
#bigger size due to amount of matches that had similar results
ggplot(tennis2, aes(x = BPW.1, y = BPW.2)) + geom_count(alpha = 0.5)

#look at the outcome of the finals
tennis1[c(127), ]
tennis2[c(127), ]
tennis3[c(122), ]
ggplot(tennis1, aes(x = WNR.1, y = WNR.2, colour = Round)) + geom_point() + facet_wrap(~ Round, nrow = 3, ncol = 3)
ggplot(tennis2, aes(x = WNR.1, y = WNR.2, colour = Round)) + geom_point() + facet_wrap(~ Round, nrow = 3, ncol = 3)
#The winner in tennis 3, Player 2, has lew winners than Player 1 who lost
ggplot(tennis3, aes(x = WNR.1, y = WNR.2, colour = Round)) + geom_point() + facet_wrap(~ Round, nrow = 3, ncol = 3)

ggplot(tennis3, aes(x = WNR.1, y = WNR.2)) + geom_hex(bins = 25)
#3
ggplot(tennis1,aes(x = WNR.1,y = WNR.2)) +
  geom_point(colour = "blue") +
  geom_smooth(colour = "red",size = 1) +
  scale_y_continuous(limits = c(5,60), breaks = seq(5,60,5)) +
  ggtitle ("A Winner distribution by players in Australian Open") +
  xlab("Player 1 Winner") +  ylab ("Player 2")


#data preparation
names(tennis1)
head(tennis1)
str(tennis1)
names(tennis2)
head(tennis2)
str(tennis2)
names(tennis3)
head(tennis3)
str(tennis3)
#check for null values
sum(is.na(tennis1))
sum(is.na(tennis2))
sum(is.na(tennis3))
#number of complete observations
nrow(tennis1)
nrow(tennis2)
nrow(tennis3)
sum(complete.cases(tennis1))
nrow(tennis1)==sum(complete.cases(tennis1))
#there is no need to convert variables from factors to int and the opposite 
table(tennis1$Round)
(table(tennis1$Round, tennis1$ST1.1))
(table(tennis2$Round, tennis1$ST1.1))
(table(tennis3$Round, tennis1$ST1.1))
table(tennis1$Round)
table(tennis2$Round)
#the data in Wimbledon is missing 5 second rounds, meaning that we have no data of 5 Players 1 and 5 Players 2 playing in the second round.
table(tennis3$Round)
#adding additional variable Surface
tennis1$Surface<-rep('Hard', each=127)
tennis2$Surface<-rep('Clay', each=127)
tennis3$Surface<-rep('Grass', each=122)

#install.packages("tidyverse")
library(tidyverse)
#we drop ST4.1, ST5.2 and ST4.2, ST5.2 because Women play maximum of 3 sets, thus these variables are irrelevant
tennis1_sub = subset(tennis1, select = c(Result, Round, FSP.1, FSW.1, SSP.1, SSW.1, ACE.1, DBF.1, WNR.1, UFE.1, BPC.1, BPW.1, NPA.1, NPW.1, TPW.1, FSP.2, FSW.2, SSP.2, SSW.2, ACE.2, DBF.2, WNR.2, UFE.2, BPC.2, BPW.2, NPA.2, NPW.2, TPW.2, Surface))
tennis2_sub = subset(tennis2, select = c(Result, Round, FSP.1, FSW.1, SSP.1, SSW.1, ACE.1, DBF.1, WNR.1, UFE.1, BPC.1, BPW.1, NPA.1, NPW.1, TPW.1, FSP.2, FSW.2, SSP.2, SSW.2, ACE.2, DBF.2, WNR.2, UFE.2, BPC.2, BPW.2, NPA.2, NPW.2, TPW.2, Surface))
tennis3_sub = subset(tennis3, select = c(Result, Round, FSP.1, FSW.1, SSP.1, SSW.1, ACE.1, DBF.1, WNR.1, UFE.1, BPC.1, BPW.1, NPA.1, NPW.1, TPW.1, FSP.2, FSW.2, SSP.2, SSW.2, ACE.2, DBF.2, WNR.2, UFE.2, BPC.2, BPW.2, NPA.2, NPW.2, TPW.2, Surface))

tennis_merged <- rbind(tennis1_sub, tennis2_sub, tennis3_sub)
ggplot(tennis_merged, aes(x = WNR.1, y = WNR.2, colour = Round, shape = Surface)) + geom_point()  + facet_wrap(~ Round, nrow = 3, ncol = 3)

#dealing with missing variables
sum(is.na(tennis_merged))
library(mice)
md.pattern(tennis_merged)
set.seed(21)
t_imputed = complete(mice(tennis_merged))
tennis_merged=t_imputed

#Train and Test
library(caTools)
set.seed(21)
split = sample.split(tennis_merged$Result, SplitRatio=0.5)
tennis_test=subset(tennis_merged, split==FALSE)
tennis_train=subset(tennis_merged, split==TRUE)
nrow(tennis_test)
nrow(tennis_train)
#Our baseline model 
table(sign(tennis_train$Result))
94/188

#correlation
str(tennis_train)
cor(tennis_train[c("FSP.1", "FSW.1", "SSP.1", "SSW.1", "ACE.1", "DBF.1", "WNR.1", "UFE.1", "BPC.1", "BPW.1", "NPA.1", "NPW.1", "TPW.1", "FSP.2", "FSW.2", "SSP.2", "SSW.2", "ACE.2", "DBF.2", "WNR.2", "UFE.2", "BPC.2", "BPW.2", "NPA.2", "NPW.2", "TPW.2", "Result")])

#we build the logistic regression model by adding the most sigificant values found from above
Tennis_LRM = glm(Result ~ BPC.1 + BPC.2 + BPW.1 + BPW.2 + TPW.1 + TPW.2 + UFE.1 + UFE.2+FSW.1, data=tennis_train, family=binomial)
summary(Tennis_LRM)
#We start dropping the least signficant variables
Tennis_LRM = glm(Result ~ BPC.1 + BPC.2 + BPW.1 + BPW.2 + TPW.1 + TPW.2 + UFE.1 + UFE.2, data=tennis_train, family=binomial)
summary(Tennis_LRM)
Tennis_LRM = glm(Result ~ BPC.1 + BPC.2 + BPW.2 + TPW.1 + TPW.2 + UFE.1 + UFE.2, data=tennis_train, family=binomial)
summary(Tennis_LRM)
Tennis_LRM = glm(Result ~ BPC.2 + BPW.2 + TPW.1 + TPW.2 + UFE.1 + UFE.2, data=tennis_train, family=binomial)
summary(Tennis_LRM)
Tennis_LRM = glm(Result ~ BPC.1 + BPC.2 + BPW.2 + TPW.1 + TPW.2 + UFE.2, data=tennis_train, family=binomial)
summary(Tennis_LRM)
#The AIC increased so we would use the previous model which had a lower AIC 
Tennis_LRM = glm(Result ~ BPC.2 + BPW.2 + TPW.1 + TPW.2 + UFE.1 + UFE.2, data=tennis_train, family=binomial)
summary(Tennis_LRM)

#predictions that Player 1 would win on our training data
Prediction1=predict(Tennis_LRM, type="response")
table(actual=tennis_train$Result, prediction=Prediction1>=0.5)
#Our model on the training data predicts 0.95% correctly
(90+90)/188
#baseline on test data set prediction
table(actual=tennis_test$Result, predicted=sign(tennis_test$Result))
#prediction for our model on test data
Prediction_2=predict(Tennis_LRM, newdata=tennis_test, type='response')
table(actual=tennis_test$Result, prediction=Prediction_2>=0.5)
#we make 5 mistakes in the testing set where Player 1 actually lost
#most matches were played on grass, this could be generated from the filled nan values in TPW.2 and TPW.1
#as mentioned in the report, they were missing in the raw data
subset(tennis_test, Prediction_2>=0.5 & Result == 0)

#evaluation
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
Tennis_Tree = rpart(Result~., method = "class", data = tennis_train, control = rpart.control(cp=.01))
rpart.plot(Tennis_Tree)
TennisCART= predict(Tennis_Tree, newdata = tennis_train, type = "class")
ConfMatrix = as.matrix(table(Actual = tennis_test$Result, Predicted = TennisCART))
ConfMatrix
#compute accuracy
n = sum(ConfMatrix) # number of instances
Trues = diag(ConfMatrix) # number of correctly classified instances per class 
Accuracy = sum(Trues)/n 
Accuracy

#Pruning the tree
printcp(Tennis_Tree)
plotcp(Tennis_Tree)
optimalcp = Tennis_Tree$cptable[which.min(Tennis_Tree$cptable[,"xerror"]),"CP"]
optimalcp
#prune the tree according to the optimalcp value
PrunedTennis_Tree=prune(Tennis_Tree,cp=optimalcp)
prp(PrunedTennis_Tree)
fancyRpartPlot(PrunedTennis_Tree)
fancyRpartPlot(Tennis_Tree)
# predictions using pruned model against the test set and compute accuracy
CARTpruned_predict = predict(Tennis_Tree, newdata = tennis_test, type = "class")
cmPruned = as.matrix(table(Actual = tennis_test$Result, Predicted = CARTpruned_predict))
cmPruned
nPr = sum(cmPruned)
diagPr = diag(cmPruned) 
accuracyPruned = sum(diagPr) / n 
accuracyPruned

#build random forest model
tennis_train$Result = as.factor(tennis_train$Result)
tennis_test$Result = as.factor(tennis_train$Result)
TennisForest = randomForest(Result ~ BPC.2 + BPW.2 + TPW.1 + TPW.2 + UFE.1 + UFE.2, data = tennis_train, ntree=200, nodesize=25 )
varImpPlot(TennisForest)
# make predictions against test set, make confusion matrix and compute accuracy
PredictForest = predict(TennisForest, newdata=tennis_test)
cmForest = as.matrix(table(Actual = tennis_test$Result, Predicted = PredictForest))
cmForest
n = sum(cmForest) # number of instances
diagForest = diag(cmForest) # number of correctly classified instances per class 
accuracyForest = sum(diagForest) / n 
accuracyForest
print(TennisForest)
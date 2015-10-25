library(dplyr)
library(caret)
library(rattle)
library (e1071)
library(plyr)
library(kernlab)
library(rpart)

#get data
setwd("C:/r_work")
inFile <- "pml-trainingmodnames.csv"
data1<-read.csv(inFile, header=TRUE, sep=",")
head(data1)
summary(data1)
dim(data1)

#testfile
testfile<- "pml-testingnames2.csv"
ans1<-read.csv(testfile, header=TRUE, sep=",")
ans2<- ans1[c(2:54)]
#ans2<- subset(ans2a, select =  -c(roll_arm, pitch_arm, yaw_arm))

#subset for names - carlitos, pedro, jeremy, adelmo, eurico, charles

dataf1<- subset( data1,user_name== "carlitos")
dataf2<-  dataf1[c(2:54)]
#roll_arm, pitch_arm, yaw_arm - missing
#dataf2<- subset(dataf2a, select =  -c(roll_arm, pitch_arm, yaw_arm))
# issue with no variance

# adelmo 3 fiedsl with no variance
#roll_forearm, pitch_forearm, yaw_forearm
#split data test- train
#carlitos is ok, done


inTrain<- createDataPartition(y=dataf2$classe,p=0.75,list=FALSE)
training<- dataf2[inTrain,]
testing<- dataf2[-inTrain,]
dim (training)
dim (testing)


#pca

#preproc<- preProcess((training[,-53]+1),method="pca",pcaComp=5)
preproc<- preProcess((training[,-53]+1),method="pca",thresh=0.7)

dataPC<- predict(preproc, (training[,-53]+1))
qplot(dataPC[,1],dataPC[,2],colour=classe,data=training)

#models
trainPC<- predict (preproc,(training))
modFit2<- train(training$classe ~.,method="knn",data=trainPC)
testPC<- predict(preproc, (testing))
confusionMatrix( testing$classe, predict(modFit2,testPC))

#answers
ansPC<- predict(preproc ,(ans2))

confusionMatrix( ans2$classe, predict(modFit2,ansPC))
modFit4<-train(training$classe ~ . ,method="knn",preProcess="pca",data=training)                         
confusionMatrix(ans2$classe, predict(modFit4,ans2))   
predict(modFit4,ans2)                          
  

                          
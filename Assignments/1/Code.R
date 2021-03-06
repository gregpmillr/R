#PART 1

#Vector and Matrix
#1)
vec = c(sample(1:100,12,replace = FALSE))
print(vec)

#2)
tail(vec,5)

#3)
mat = matrix(vec,3,4)

#4)
mat[2,3]

#5)
mat[2:3,3:4]

#6)
#function iterating through matrix
iterate = function(){
  v <- vector()
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat))
    {
      if(mat[i,j] %% 2 != 0){
        v <- c(v,mat[i,j])
      }
    }
  }
  print(v)
}

iterate()


#Data Frame
#1)
library(gdata)
help(read.xls)
mydata = read.csv(paste(getwd(),"/Downloads/Credit.csv",sep=""))

#2)
nrows <- nrow(mydata)
ncols <- ncol(mydata)
nrows
ncols

#3)
mydata[mydata$Income>100 & mydata$Age<50,]

#4)
setwd("/home/greg/Desktop")
write.table(mydata, file="foo.table", sep=";")


#Create a function
#1)
#function returning all numbers between startNumber,endNumber which are 
#divisible by 7 but are not a multiple of 5
findDivisibleNotMultiple = function(startNumber, endNumber){
  v <- c()
  for(i in startNumber:endNumber){
    if(i %% 7 == 0 & i %% 5 != 0){
      v <- c(v,i)
    }
  }
  return(v)
}
#find all associated values from 100 to 200
v <- c(findDivisibleNotMultiple(100,200))
v

#2)
#function normalizing a given vector
normalizeVector = function(v){
  min <- min(v)
  max <- max(v)
  index <- 0;
  
  for(i in v){
    index = index + 1
    v[index] = (i - min)/(max-min)
  }
  print(v)
}

#normalize vector from first question
normalizeVector(vec)



#PART 2

#1)
set.seed(340)

#2)
nmean <- 500
nsd <- 100
x <- rnorm(1000, nmean, nsd)

#3)
hist(x,probability = TRUE)

#4)
curve(dnorm(x, mean=500,sd=100), col="blue", lwd=2, add=TRUE)

#5)
firstXVal <- quantile(x,probs=0.05)
secondXVal <- quantile(x,probs=0.95)
#plot lines for quantile values of x
abline(v=firstXVal)
abline(v=secondXVal)

#6)
#display x val beside line
text(firstXVal,0.003,paste("x=",firstXVal,sep=""),adj=1.1)
text(secondXVal,0.003,paste("x=",secondXVal,sep=""),adj=c(-.1,-.1))



#PART 3

#1)
library(ISLR)
require(ISLR)
print(ncol(Weekly)) ## num of cols in data set
print(nrow(Weekly)) ## num of rows in data set

#2)
head(Weekly)
print(colnames(Weekly))

#3)
rm(list=ls())
library(class)

library(ISLR)

#will need to normalize data for graph/plots
normalize <- function(x) {
  return( (x - min(x)) / (max(x) - min(x)) )
}

#apply normalization
Weekly_n <- as.data.frame(lapply(Weekly[,c(2,3)], normalize))

#apply the Years column to the new normalized data frame
Weekly_n[,"Year"] <- c(Weekly$Year)
Weekly_n[,"label"] <- rep(NA,1089)

#apply color for labels
Weekly_n[Weekly_n$Lag1>0.5 & Weekly_n$Lag2 >0.5,"label"] = "red"   # Quadrant 1
Weekly_n[Weekly_n$Lag1<0.5 & Weekly_n$Lag2 >0.5,"label"] = "green" # Quadrant 2
Weekly_n[Weekly_n$Lag1<0.5 & Weekly_n$Lag2 <0.5,"label"] = "orange"
Weekly_n[Weekly_n$Lag1>0.5 & Weekly_n$Lag2 <0.5,"label"] = "blue"

#the "get only these" criteria
train = (Weekly_n$Year >= 1990 & Weekly_n$Year <= 2008)
test = (Weekly_n$Year >= 2009 & Weekly_n$Year <= 2010)

#apply the train and test vector to this so we only get what we want
train.X <- cbind(Weekly_n$Lag1,Weekly_n$Lag2)[train,]
test.X = cbind(Weekly_n$Lag1,Weekly_n$Lag2)[test,]

#set the target (cl) to be Direction and test vector to this so we only get what we want
train.target <- Weekly$Direction[train]
test.target <- Weekly$Direction[test]

set.seed(1)
knn.pred = knn(train.X,test.X,train.target,k=5)
table(knn.pred,test.target)
mean(knn.pred == test.target) ##how accurate the prediction was

plot(Weekly_n[,1:2],col=Weekly_n$label,pch=5,asp=1,xlim=c(0,1),ylim=c(0,1))
abline(h=0.5,lty=3)
abline(v=0.5,lty=3)

#4)
## The random seed is set before applying knn() because
## if there are tied observations, we must randomly break the tie using R.
## The seed is set because we'd want to reproduce results!


#5 **AND** 6)
#draw plot 
plot(1, type="n", xlim=c(0, 30), ylim=c(0, 1), xlab="K", ylab="Accuracy Rate")

#plot points for various values of k
for(i in 1:20){
  if(i == 1){
    set.seed(1)
    knn.pred = knn(train.X,test.X,train.target,k=i);
    points(i, mean(knn.pred == test.target));
  }
  else if(i == 3){
    set.seed(1)
    knn.pred = knn(train.X,test.X,train.target,k=i);
    points(i, mean(knn.pred == test.target));
  }
  else if(i %% 5 == 0){
    set.seed(1)
    knn.pred = knn(train.X,test.X,train.target,k=i);
    points(i, mean(knn.pred == test.target));
  }
} ##end for

## I would choose k=15, as it has the highest rate of accuracy.


#7)
## Although I've already added normalization, if I didn't add it in
## then it would be the extra needed step because the values generally
## stay smaller and in a more precise decimal value compared to Lag1
## and Lag2 which has sporatic values. I did/would normalize this data
## using feature scaling.
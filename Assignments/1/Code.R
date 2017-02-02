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

set.seed(99)

## prepare data set
train = data.frame(x1 = runif(20,-1,1), x2 = runif(20,-1,1), label = rep(NA,20))

## assign labels based on x,y coordinates
train[train$x1>0 & train$x2 >0, "label"] = "red" #quadrant 1
train[train$x1<0 & train$x2 >0, "label"] = "green" #quadrant 2
train[train$x1<0 & train$x2 <0, "label"] = "orange" 
train[train$x1>0 & train$x2 <0, "label"] = "blue" #quadrant 1

## plot the points
plot(train[,1:2], col=train$label,pch=19,asp=1,xlim=c(-1,1),ylim=c(-1,1))
abline(h=0,lty=3)
abline(v=0,lty=3)

p = c(0.1,-0.5)  # test data
points(0.1,-0.5,pch=10)

library(class)
m1 <- knn(train=train[,1:2],test=p,cl=train[,3],k=5,prob=T)
m1
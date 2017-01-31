rm(list=ls())

#PLOTTING LINEAR REGRESSION

ads = read.csv("Advertising.csv")

y = ads$Sales
x = ads$TV

#slope
beta1 = sum((x - mean(x))*(y - mean(y)))/ sum((x-mean(x))^2)

#intercept
beta0 = mean(y) - beta1*mean(x)

#fitting linear model
fit = lm(y ~ x)

fit

summary(fit)

plot(x,y)

#add line to plot with abline
abline(fit,col="red")

set.seed(1)

x = round(runif(20,1,10),2)

#add a random error for each of y
y = 3*x + 2
y = y + 2 + rnorm(length(x),0,4)

fit = lm(y~x)
plot(x,y,xlim=c(0,15),ylim=c(0,50))
abline(2,3,col="red")
abline(fit,col="blue")

#generate different lines and plot them
par(mfrow = c(1,1))
for (i in 1:10){
  y = 3*x + 2 + rnorm(length(x),0,4)
  fit=lm(y~x)
  abline(fit,lty=3)
}

y = ads$Sales
x = ads$TV
fit = lm(y~x)
resid = residuals(fit) # residuals
RSS = sum(resid^2)
RSE = sqrt(RSS/(length(x) - 2))
SEbeta1 = sqrt(RSE^2 / sum((x-mean(x))^2))
SEbeta0 = sqrt(RSE^2*(1/length(x) + mean(x)^2/sum((x-mean(x))^2)))



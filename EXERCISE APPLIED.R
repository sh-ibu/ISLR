library(ISLR)
attach(Auto)
na.omit(Auto)
lm.fit1 = lm(mpg~horsepower , data=Auto)
summary(lm.fit1)
predict(lm.fit1,data.frame(horsepower=c(98)), interval = 'confidence')
predict(lm.fit1,data.frame(horsepower=c(98)),interval='prediction')
plot(horsepower,mpg)
abline(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit1)
plot(Auto)
cor(Auto[sapply(Auto,is.numeric)])
lm.fit2 = lm(mpg~.-name,data=Auto )
summary(lm.fit2)       
par(mfrow=c(2,2))
plot(lm.fit2)
plot(predict(lm.fit2),rstudent(lm.fit2))
lm.fit3 = lm(mpg~cylinders*displacement+displacement*weight,data=Auto)
summary(lm.fit3)
lm.fit4=lm(mpg~+displacement*weight,data=Auto)
summary(lm.fit4)
lm.fit5 = lm(mpg~(cylinders*weight)+((displacement))+weight,data=Auto)
summary(lm.fit5)
lm.fit6=lm(log(mpg)~.-name,data=Auto)
summary(lm.fit6)
#Model lm.fit6 is best until now


#QUESTION NO 10

attach(Carseats)
lm.fit1=lm(Sales~Price+Urban+US,data=Carseats)
summary(lm.fit1)
lm.fit2=lm(Sales~US+Price,data=Carseats)
plot(lm.fit2)

#QUESTION 11

set.seed(1)
x=rnorm(100)
y=2*x + rnorm(100)
lm.fit1 = lm(y~x+0)
summary(lm.fit1)
lm.fit2=lm(x~y+0)
summary(lm.fit2)


#QUESTION 12


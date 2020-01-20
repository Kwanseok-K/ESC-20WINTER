library(ISLR)
set.seed(1)
attach(Auto)

#LR fit with Validation Set
train = sample(392,196)
plot(mpg~horsepower, data = Auto, pch = '.', cex = 1.8)

#deg = 1
lm.fit = lm(mpg~horsepower, data= Auto, subset = train)

mean((mpg-predict(lm.fit, Auto))[-train]^2)

#line 그리기
newdt = data.frame(horsepower = seq(min(horsepower), max(horsepower), length.out = 500))
newdt$pred1 = predict(lm.fit, newdata = newdt)
with(newdt, lines(x = horsepower, y = pred1))

#deg = 2
lm.fit2 = lm(mpg~poly(horsepower,2),data=Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

newdt$pred2 = predict(lm.fit2, newdata = newdt)
with(newdt, lines(x = horsepower, y = pred2, col = 'red'))

#deg = 3
lm.fit3 = lm(mpg~poly(horsepower,3),data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


newdt$pred3 = predict(lm.fit3, newdata = newdt)
with(newdt, lines(x=horsepower, y=pred3, col='blue'))

set.seed(2)
train = sample(392, 196)
lm.fit = lm(mpg~horsepower, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2 = lm(mpg~poly(horsepower,2),data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

glm.fit = glm(mpg~horsepower, data=Auto)
coef(glm.fit)

lm.fit = lm(mpg~horsepower, data = Auto)
coef(lm.fit)

##LOOCV in glm##
#glm을 써야 boot의 CV를 계산할 수 있음
library(boot)
glm.fit = glm(mpg~horsepower, data = Auto)
cv.err = cv.glm(Auto,glm.fit)
cv.err$delta

#higher degrees
cv.error = rep(0,5)
for (i in 1:5) {
  glm.fit = glm(mpg~poly(horsepower,i), data = Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
plot(1:5, cv.error, type='b') #2개 이상은 별로 안다르단걸 알 수 있음


set.seed(17)
cv.error.10 = rep(0,10)
for(i in  1:10){
  glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit,K=10)$delta[1]
}

##k-fold Cross-Validation##
set.seed(17)
kcv.error = rep(0,10)
for(i in 1:10){
  glmfit_poly = glm(mpg~poly(horsepower, i), data = Auto)
  kcv.error[i] = cv.glm(Auto, glmfit_poly, K=10)$delta[1]
}
plot(1:10, kcv.error, type='b')

## The Bootstrap ##
head(Portfolio) ; dim(Portfolio)

alpha.fn = function(data,index){
  X = data$X[index]
  Y = data$Y[index]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace = T))
boot(Portfolio, alpha.fn, R=1000)

## Estimating the Accuracy of a Linear Regression Model##

#Linear fit : comparing two estimates for beta std#
boot.fn = function(data, index)
  return(coef(lm(mpg~horsepower, data = data, subset = index)))
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392,392, replace=T))
boot.fn(Auto,sample(392,392,replace=T))

boot(Auto, boot.fn, 1000) #compare this with
summary(lm(mpg~horsepower, data = Auto))$coef
##Bootstrap Betahat S.E와 OLS Betahat S.E 숫자가 다른 이유
# OLS는 MSE(t(X)*X)^-1
# Bootstrap -> 하나하나 뽑은 Betahat간의 S.E
#1. MSE 가 sigma^2로 가지 않을 때(model misspecification)
#2. OLS : 하나의 train set이 주어졌을 때 betahat의 분산
# bootstrap : 여러개의 train set이 주어졌을 때 그 각각의 betahat간의 분산
#Bootstrap estimate for standard error in linear regression 검색해보기!

boot.fn = function(data,index)
coefficients(lm(mpg~horsepower + I(horsepower^2), data=data, subset=index))  
set.seed(1)
boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower + I(horsepower^2), data = Auto))$coef

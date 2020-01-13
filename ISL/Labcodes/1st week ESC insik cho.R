#4.6 lab: Logistic Regression, LDA, QDA, and KNN
library(ISLR)
names(Smarket)
dim(Smarket
    )

summary(Smarket)
pairs(Smarket)
cor(Smarket) # gives an error since Direction variable i qualitative

cor(Smarket[,-9]) #can check that cor works without direction
attach(Smarket)
plot(Volume)


#4.6.2: Logistic Regression
glm.fit = glm(Direction~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family = binomial)

summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef

glm.probs=predict(glm.fit, type = "response")
glm.probs[1:10]
glm.pred = rep("Down", 1250)
glm.pred[glm.probs>0.5]  = "Up"
table(glm.pred , Direction)
mean(glm.pred == Direction)

train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

glm.fit = glm(Direction~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type= "response")

glm.pred = rep("Down", 252)
glm.pred[glm.probs>.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

glm.fit = glm(Direction~ Lag1 + Lag2,
             data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type= "response")

glm.pred = rep("Down", 252)
glm.pred[glm.probs>.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)


#4.6.3 Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posteriot[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)


#4.6.4 Quadratic Discriminant Analysis
qda.fit = qda(Direction ~ Lag1 + Lag2,data = Smarket, subset = train)
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class ==Direction.2005)

#4.6.5 KNN
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)

knn.pred = knn(train.X, test.X, train.Direction , k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)


#5.3.1 the Validation Set Approach
library(ISLR)
set.seed(1)
train = sample(392,196)
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

set.seed(2)
train = sample(392,196)
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)


#5.3.2 Leave-One-Out Cross-Validation
glm.fit = glm(mpg~horsepower, data = Auto)
coef(glm.fit)
lm.fit = lm(mpg~horsepower, data = Auto)
coef(lm.fit)


library(boot)
glm.fit = glm(mpg~horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)              
cv.err$delta

cv.error = rep(0,5)
for (i in 1:5){
glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
cv.error[i] = cv.glm(Auto, glm.fit)$delta[1] 
 }

#5.3.3 k-Fold Cross - Validation
set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10){
  glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10)$delta[1]
}
cv.error.10


#5.3.4 The bootstrap
alpha.fn = function(data,index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X,Y))/(var(X)+var(Y) -2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)
set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace = T))

boot(Portfolio, alpha.fn, R=1000)


#estimating the accuracy of a linear regression model
boot.fn = function(data,index)
return(coef(lm(mpg~horsepower, data = data, subset = index)))
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392,392,replace =T))

boot(Auto, boot.fn,1000)


summary(lm(mpg~horsepower, data = Auto))$coef

#ch04
install.packages('ISLR')
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

##logistic regression
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
glm.probs = predict(glm.fit, type = 'response')
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep('Down',1250)
glm.pred[glm.probs>.5]='Up'
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)
train=(Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
glm.fit=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Smarket, family= binomial, subset=train)
glm.probs=predict(glm.fit, Smarket.2005, type="response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)
glm.fit = glm(Direction ~ Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit ,Smarket.2005, type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred ,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

##LDA
install.packages('MASS')
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior [1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

##QDA
qda.fit=qda(Direction ~ Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit, Smarket.2005) $class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

##KNN
install.packages('class')
library(class)
train.X=cbind(Lag1,Lag2)[train ,]
test.X=cbind(Lag1,Lag2)[!train ,]
train.Direction =Direction[train]
set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
(83+43)/252
knn.pred=knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
standardized.X= scale(Caravan[,-86])
var(Caravan[ ,1])
var(Caravan[ ,2])
var(standardized.X[,1])
var(standardized.X[,2])
test = 1:1000
train.X = standardized.X[-test ,]
test.X = standardized.X[test ,]
train.Y = Purchase [-test]
test.Y = Purchase [test]
set.seed(1)
knn.pred = knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
9/(68+9)
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset = -test)
glm.probs=predict(glm.fit ,Caravan [test ,], type="response ")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs >.25]="Yes"
table(glm.pred,test.Y)


ch05
library(ISLR)
set.seed(1)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train ]^2)
lm.fit2=lm(mpg ~ poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[- train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[- train]^2)
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg ~ horsepower,subset=train)
mean((mpg-predict (lm.fit ,Auto))[-train ]^2)
lm.fit2=lm(mpg ~ poly(horsepower,2),data=Auto,subset=train)
mean((mpg -predict (lm.fit2,Auto))[- train]^2)
lm.fit3=lm(mpg ~ poly(horsepower,3),data=Auto , subset=train)
mean((mpg-predict(lm.fit3,Auto))[- train]^2)

##loocv
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)
install.packages('boot')
library(boot)
glm.fit=glm(mpg ~ horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
cv.error=rep(0,5)
for(i in 1:5){
glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
cv.error[i]=cv.glm(Auto,glm.fit)$delta [1]
}
cv.error

##k-fold cross-validation
set.seed(17)
cv.error.10=rep(0 ,10)
for(i in 1:10){
glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
cv.error.10[i]=cv.glm(Auto,glm.fit ,K=10)$delta[1]
}
cv.error.10

##bootstrap
alpha.fn=function(data ,index){
X=data$X[index]
Y=data$Y[index]
return ((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio ,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)
boot.fn=function (data ,index)
return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392, replace=T))
boot.fn(Auto,sample (392,392, replace=T))
boot(Auto,boot.fn ,1000)
summary (lm(mpg~horsepower ,data=Auto))$coef
boot.fn=function (data ,index)
coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary (lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

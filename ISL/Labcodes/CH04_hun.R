### CH04 Lab Work ###

library(ISLR)

# Data probe
head(Smarket); tail(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket)
# library(scatterplot3d)
# scatterplot3d(Lag1,Lag2, as.numeric(Direction)-1, main="3D Scatterplot")

# Correlations
# windows()
pairs(Smarket)
cor(Smarket[, -9])

attach(Smarket)
plot(Volume)

## Logistic Regression ##

# Logit Fitting
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data = Smarket, family= binomial)
summary(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,1, drop=F]
summary(glm.fit)$coef[,4, drop=F]

# predict(glm.fit, type="terms") # Estimated Discriminant
glm.probs = predict(glm.fit, type="response") # Estimated Bayes Classifier

glm.pred = rep("Down", 1250)
glm.pred[glm.probs>.5] = "Up"

table(glm.pred, Direction) # Confusion Matrix
mean(glm.pred != Direction) # Training ER
sum(Direction == "Up") / length(Direction) # Null Classifier

# Test MSE
train = (Year<2005) # Train-Test division
Smarket.test = Smarket[!train,]
Direction.test = Direction[!train]

glm.fit2 = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Smarket, family= binomial, subset=train)
glm.probs2 = predict(glm.fit2, Smarket.test, type="response")
glm.pred2 = rep("", 252)
glm.pred2[glm.probs2 > .5] = "Up"
glm.pred2[glm.probs2 <= .5] = "Down"
table(glm.pred2, Direction.test)
mean(glm.pred2!=Direction.test)

# Logit Fitting with less predictors
glm.fit3 = glm(Direction ~ Lag1+Lag2, data = Smarket, family= binomial, subset=train)
glm.probs3 = predict(glm.fit3, Smarket.test, type="response")
glm.pred3 = rep("", 252)
glm.pred3[glm.probs3 > .5] = "Up"
glm.pred3[glm.probs3 <= .5] = "Down"
table(glm.pred3, Direction.test)
mean(glm.pred3!=Direction.test)

predict(glm.fit3, 
        newdata = data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)),
        type = "response")


## Linear Discriminant Analysis ##
library(MASS)
lda.fit= lda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)
lda.fit 
plot(lda.fit) 

plot(Lag1[Direction =="Up"], Lag2[Direction =="Up"], col='red', pch="U", cex=0.5)
points(Lag1[Direction =="Down"], Lag2[Direction =="Down"], col='blue', pch='D', cex=0.5)
abline(0.032 / 0.044, -0.055 / 0.044) # Logit DB https://stats.stackexchange.com/questions/6206/how-to-plot-decision-boundary-in-r-for-logistic-regression-model
abline(0.34/ 0.513, -0.642 / 0.513, lty=2) # LDA DB https://stackoverflow.com/questions/40087417/lda-interpretation

lda.pred = predict(lda.fit, Smarket.test)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.test)
mean(lda.class == Direction.test)

sum(lda.pred$posterior[,1]>0.6)

## Quadratic Discriminant Analysis ##
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.class = predict(qda.fit, Smarket.test)$class
table(qda.class, Direction.test)
mean(qda.class == Direction.test)

# DBD
x =  seq(min(Smarket$Lag1), max(Smarket$Lag1), length.out=100)
y =  seq(min(Smarket$Lag2), max(Smarket$Lag2), length.out=100)
xygrid = expand.grid(x, y)
colnames(xygrid) = c('Lag1', 'Lag2')
xygrid.pred = predict(qda.fit, xygrid)$class
plot(Lag1[Direction =="Up"], Lag2[Direction =="Up"], col='red', pch="U", cex=0.5)
points(Lag1[Direction =="Down"], Lag2[Direction =="Down"], col='blue', pch='D', cex=0.5)
points(xygrid$Lag1[xygrid.pred =="Up"], xygrid$Lag2[xygrid.pred =="Up"], col='orange', pch="U", cex=0.5)
points(xygrid$Lag1[xygrid.pred =="Down"], xygrid$Lag2[xygrid.pred =="Down"], col='skyblue', pch='D', cex=0.5)
abline(0.032 / 0.044, -0.055 / 0.044) # Logit DB
abline(0.34/ 0.513, -0.642 / 0.513, lty=2) # LDA DB

## KNN ##
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
knn.pred= knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test)

knn.pred= knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test)

knn.pred= knn(train.X, test.X, train.Direction, k=5)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test)

## KNN: another application ##
dim(Caravan)
attach(Caravan)
summary(Purchase)

std.X = scale(Caravan[,-86])
test = 1:1000
train.X = std.X[-test,]
test.X = std.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]

knn.pred = knn(train.X, test.X, train.Y, k=1)
table(knn.pred, test.Y)
mean(test.Y!=knn.pred)

knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
mean(test.Y!=knn.pred)

knn.pred = knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)
mean(test.Y!=knn.pred)








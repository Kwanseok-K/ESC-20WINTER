install.packages("ISLR")
library(ISLR)
head(Smarket)
names(Smarket)
dim(Smarket)
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(Lag1, Lag2, as.numeric(Direction)-1)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

#logistic regression
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket, family=binomial) #familiy binomial을 넣어야 logistic regression 에서 binary case라는 것을 알려줌 
summary(glm.fit) #pvlaue가 높은 애들은 0이어도 크게 상관 없단 뜻 -> 빼자! 그래서 Lag1, Lag2만 보고 하자

coef(glm.fit)
summary(glm.fit)$coef[,4]

glm.probs=predict(glm.fit, type='response')
glm.probs[1:10]

contrasts(Direction)

glm.pred = rep("Down", 1250)
glm.pred[glm.probs>.5]="Up"

table(glm.pred, Direction)

(507+145)/1250
mean(glm.pred == Direction)

#Test MSE
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train) #subset으로 train만 
glm.probs = predict(glm.fit, Smarket.2005, type = 'response') #type = response로 해야 확률 그 자체를 알려줌

glm.pred = rep("", 252)
glm.pred[glm.probs>.5] = 'Up'
glm.pred[glm.probs <= .5] = 'Down'
table(glm.pred, Direction.2005) #glm.pred가 prediction, Direction.2005가 실제인 confusion matrix
mean(glm.pred == Direction.2005)
mean(glm.pred!=Direction.2005)#error rate

glm.fit = glm(Direction~Lag1+Lag2, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = 'response')
glm.pred = rep("Down",252)
glm.pred[glm.probs>.5] = 'Up'
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106/(106+76)

predict(glm.fit, newdata=data.frame(Lag1 = c(1.2,1.5), Lag2=c(1.1,-0.8)),type='response')

#LDA
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
lda.fit
#graph
plot(Lag1[Direction == "Up"], Lag2[Direction == "Up"], col = 'red', pch = "U", cex=0.5)
points(Lag1[Direction == "Down"], Lag2[Direction == "Down"], col = 'blue', pch = "D", cex=0.5)
#decision boundary 검색
#logit -> how to plot decision boundary in R for logistic regression model?
#LDA -> LDA interpretation

plot(lda.fit)


lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

#LDA confusion matrix
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class!=Direction.2005)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

sum(lda.pred$posterior[,1]>.9)

#QDA

qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
#QDA confusion matrix
table(qda.class, Direction.2005)
mean(qda.class != Direction.2005)
#decision matrix 구하기
x = seq(min(Smarket$Lag1), max(Smarket$Lag1), length.out = 100)
y = seq(min(Smarket$Lag2), max(Smarket$Lag2), length.out = 100)
xygrid = expand.grid(x,y)
colnames(xygrid) = c('Lag1', 'Lag2')
xygrid.pred = predict(qda.fit, xygrid)$class
plot(Lag1[Direction == "Up"], Lag2[Direction == "Up"], col = 'red', pch = "U", cex=0.5)
points(Lag1[Direction == "Down"], Lag2[Direction == "Down"], col = 'blue', pch = "D", cex=0.5)
points(xygrid$Lag1[xygrid.pred == 'Up'], xygrid$Lag2[xygrid.pred == 'Up'], col = 'orange', pch = "U", cex=0.5)
points(xygrid$Lag1[xygrid.pred == 'Down'], xygrid$Lag2[xygrid.pred == 'Down'], col = 'skyblue', pch = "D", cex =0.5)
?points

#KNN
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
(83+43)/252

knn.pred = knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred, test.Y)
9/(68+9)

knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
5/26
knn.pred = knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)
4/15

glm.fit = glm(Purchase~., data = Caravan, family = binomial, subset=-test)

glm.probs = predict(glm.fit, Caravan[test, ], type = 'response')
glm.pred = rep("No", 1000)
glm.pred[glm.probs>.5] = "Yes"
table(glm.pred, test.Y)

glm.pred = rep("No", 1000)
glm.pred[glm.probs>.25] = "Yes"
table(glm.pred, test.Y)
11/(22+11)

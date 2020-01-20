#Lab 1: subset selection methods
library(ISLR)
fix(Hitters
    )
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters = na.omit(Hitters)
dim(Hitters)

library(leaps)
regfit.full = regsubsets(Salary~., Hitters
                         )
summary(regfit.full)

regfit.full = regsubsets(Salary~., Hitters, nvmax = 19)
reg.summary =summary(regfit.full)

reg.summary$rsq

plot(reg.summary$rss , xlab = "Number of ariables", ylab = "RSS")
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = " Adjusted RSq")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col = "red" , cex = 2 , pch = 20)
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
coef(regfit.full,6)

#6.5.2 Forward and Backward Stepwise Selection
regfit.fwd = regsubsets(Salary~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary~., data = Hitters, nvmax= 19, method = "backward")

#6.5.3. Choosing Among Models using the calidation set approach and cross-validation
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test = (!train)

regfit.best = regsubsets(Salary~., data = Hitters[train,], nvmax = 19)
test.mat = model.matrix(Salary~., data = Hitters[test,])

val.errors = rep(NA,19)
for(i in 1:19){
  coefi = coef(regfit.best, id=1)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((Hitters$alary[test]-pred)^2)
}
val.errors

predict.regsubsets =function(object ,newdata ,id,...){
  form=as.formula (object$call [[2]]) 
  mat=model.matrix(form ,newdata ) 
  coefi=coef(object ,id=id) + xvars=names(coefi) 
  + mat[,xvars]%*%coefi 
  }

regfit.best = regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(regfit.best,10)

k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters),replace = TRUE)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit = regsubsets(Salary~., data = Hitters[folds!=j,],nvmax = 19)
  for(i in 1:19){
    pred = predict(best.fit, Hitters[folds==j,],id =1)
    cv.errors[j,i] = mean( (Hitters$Salary[folds ==j]-pred)^2)
  }
}  



#6.6lab2: Ridge Regression and the Lasso
library(glmnet)
x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary

grid = 10^seq(10,-2,length = 100)
ridge.mod = glmnet(x,y,alpha = 0, lambda = grid)
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]


predict(ridge.mod, s=50, type = "coefficients")[1:20,]

train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train,], y[train], alpha=0,lambda = grid, thresh = 1e-12)
ridge.pred =predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred -y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred = predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred = predict(ridge.mod, s=0, newx = x[test,])
mean((ridge.pred - y.test)^2)

cv.out = cv.glmnet(x[train,] , y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s=bestlam, newx = x[test,])
mean((ridge.pred -y.test)^2)

#6.6.2 The Lasso
lasso.mod =glmnet(x[train,],y[train],alpha = 1, lambda = grid)
plot(lasso.mod)

cv.out = cv.glmnet(x[train,] ,y[train],alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)

out = glmnet(x,y,alpha =1, lambda = grid)
lasso.coef = predict(out, type = "coefficients",s=bestlam)[1:20,]



#6.7 PCR and PLS regression
library(pls)
set.seed(2)
pcr.fit = pcr(Salary~. , data = Hitters, scale = TRUE,
              validation = "CV")

summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")

set.seed(1)
pcr.fit = pcr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred = predict(pcr.fit, x[test,],ncomp = 7)
mean((pcr.pred - y.test)^2)

pcr.fit = pcr(y~x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

#6.7.2 Partial Least Squares
set.seed(1)
pls.fit = plsr(Salary~., data=Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)

pls.pred = predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2)
pls.fit = plsr(Salary~. , data = Hitters , scale = TRUE, ncomp =2)
summary(pls.fit)



##7.8 Lab: Non-Linear Modeling

library(ISLR)
attach(Wage)

fit = lm(wage~poly(age,4), data = Wage) #poly() create orthogonal combination of polynomial
coef(summary(fit))
fit2 = lm(wage~poly(age,4,raw = T),data = Wage)
coef(summary(fit2))

agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgray")
title("Degree - 4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd =2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

preds2 = predict(fit2, newdata = list(age = age.grid), se= TRUE)
max(abs(preds$fit - preds2$fit))

fit.1 = lm(wage~age, data = Wage)
fit.2 = lm(wage~poly(age,2), data = Wage)
fit.3 = lm(wage~poly(age,3), data = Wage)
fit.4 = lm(wage~ poly(age,4), data = Wage)
fit.5 = lm(wage~poly(age,5), data = Wage)
anova(fit.1, fit.2 ,fit.3, fit.4, fit.5)

coef(summary(fit.5))
(-11.982)^2


fit = glm(I(wage>250) ~ poly(age,4), data = Wage, family = binomial)
preds = predict(fit, newdata = list(age = age.grid), se= T)

pfit = exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

preds = predict(fit, newdata = list(age = age.grid), type = "response", se = T)

plot(age, I(wage>250), xlim = agelims, type = "n", ylim = c(0,.2))
points(jitter(age), I((wage>250)/5),cex = .5, pch="|", col = "darkgray")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd= 1, col= "blue", lty = 3)


###step function
table(cut(age,4))
fit = lm(wage~cut(age,4),data = Wage)
coef(summary(fit))


#7.8.2 SPLINES
library(splines)
fit = lm(wage~bs(age, knots = c(25,40,60)),data = Wage)
pred = predict(fit, newdata = list(age = age.grid),se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2*pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2*pred$se, lty = "dashed")
dim(bs(age, knots = c(25,40,60)))
dim(bs(age,df = 6))
attr(bs(age,df = 6), "knots")

fit2 = lm(wage~ns(age,df = 4), data = Wage)
pred2 = predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

plot(age, wage, xlim = agelims,cex = .5, col = "darkgrey")
title("Smoothing Splines")
fit = smooth.spline(age,wage, df=  16)
fit2 = smooth.spline(age, wage , cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)


#7.8.3GAMS
gam1 = lm(wage~ns(year,4) + ns(age,5)+education, data = Wage)

library(gam)
gam.m3 = gam(wage~s(year,4) + s(age,5) + education, data = Wage)
par(mfrow = c(1,3))
plot(gam.m3, se = TRUE, col = "blue")

plot.Gam(gam1, se= TRUE, col = "red")

gam.m1 = gam(wage~s(age,5) + education, data = Wage)
gam.m2 = gam(wage~year + s(age,5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3 , test = "F")


preds = predict(gam.m2, newdata = Wage)
gam.lo = gam(wage~s(year, df=4) + lo(age, span= 0.7) + education, data = Wage)
plot.Gam(gam.lo, se=TRUE, col = "green")
gam.lo.i = gam(wage~lo(year, age, span = 0.5) + education, data = Wage)
              
library(akima)
plot(gam.lo.i)

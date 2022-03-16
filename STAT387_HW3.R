# Applied

####################################
## 2.13 (a, b, c, d, e, f, g, h, i)
####################################
### Problem (a)
library(corrplot)
library(ISLR2)
attach(Weekly)
summary(Weekly)
plot(Weekly)
corrplot(cor(Weekly[, -9]), method = "square")



### Problem (b)
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.fit)



### Problem (c)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep("Down", 1089)
glm.pred[glm.probs > .5] <- "Up"

# create a confusion matrix
table(glm.pred, Direction)



### Problem (d)
train = (Year < 2009)
test = Weekly[!train,]
glm.fit2 = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)

# compute the confusion matrix
glm.probs2 <- predict(glm.fit2, test, type = "response")
glm.pred2 <- rep("Down", 104) # 104 is the length of glm.probs2
glm.pred2[glm.probs2 > .5] <- "Up"
Direction.20092010 = Direction[!train]
table(glm.pred2, Direction.20092010)

mean(glm.pred2 == Direction.20092010)



### Problem (e)
library(MASS)
lda.fit <- lda(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
lda.pred <- predict(lda.fit, test)
print(length(lda.pred))
print(length(test))
table(lda.pred$class, Direction.20092010)

mean(lda.pred == Direction.20092010)



### Problem (f)
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.pred <- predict(qda.fit, test)$class
table(qda.pred, Direction.20092010)

mean(qda.pred == Direction.20092010)



### Problem (g)
library(class)
Weekly.train = as.matrix(Lag2[train])
Weekly.test = as.matrix(Lag2[!train])
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(Weekly.train, Weekly.test, train.Direction, k = 1)
table(knn.pred, Direction.20092010)

mean(knn.pred == Direction.20092010)



### Problem (h)
library(e1071)
nb.fit = naiveBayes(Direction ~ Lag2, data = Weekly, subset = train)
nb.class = predict(nb.fit, Direction.20092010)
table(nb.class, Direction.20092010)

mean(nb.class == Direction.20092010)









####################################
## 3.8 (a, b, c)
####################################
### Problem (a)
set.seed(1)
x <- rnorm(100) # vector
y <- x - 2 * x^2 + rnorm(100)



### Problem (b)
plot(x, y)



### Problem (c)
# i
library(boot) # need this library for LOOCV
Data = data.frame(x,y)
glm.fit1 = glm(y ~ x)
cv.err = cv.glm(Data, glm.fit1)
cv.err$delta

# ii
glm.fit2 = glm(y~poly(x,2))
cv.err = cv.glm(Data, glm.fit2)$delta[1]

# iii
glm.fit3 = glm(y~poly(x,3))
cv.err = cv.glm(Data, glm.fit3)$delta[1]

# iv
glm.fit4 = glm(y~poly(x,4))
cv.err = cv.glm(Data, glm.fit4)$delta[1]

cv.error <- rep(0,4)
for (i in 1:4) {
  glm.fit <- glm(y ~ poly(x, i), data = Data)
  cv.error[i] <- cv.glm(Data, glm.fit)$delta[1]
}
cv.error

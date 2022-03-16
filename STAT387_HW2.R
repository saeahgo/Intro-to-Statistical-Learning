# Applied

#################################
## 3.8 (a, b, c)
#################################
### Problem (a)
data(Auto) # load the data
fix(Auto) 
fit1 <- lm(mpg ~ horsepower, data = Auto)
summary(fit1)

# get the associated 95 % confidence interval
predict(fit1, data.frame(horsepower = 98), interval = "confidence")

# get the associated 95 % prediction interval
predict(fit1, data.frame(horsepower = 98), interval = "prediction")


### Problem (b)
plot(Auto$horsepower, Auto$mpg,
     main = "mpg vs. horsepower",
     xlab = "horsepower",
     ylab = "mpg")
abline(fit1,
       col = "red") # make the line color to red to easily see


### Problem (c)
par(mfrow = c(2, 2)) # two columns two rows
plot(fit1)






#################################
## 4. 10 (a, b, c, d, e, f)
#################################
### Problem (a)
data(Carseats)
fix(Carseats)

fit2 <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit2)


### Problem (e)
fit3 <- lm(Sales ~ Price + US, data = Carseats)
summary(fit3)






#################################
## 5. 13(a, b, c, d, e)
#################################
### Problem (a)
set.seed(1)
x = rnorm(100)


### Problem (b)
eps = rnorm(100, sd = sqrt(0.25)) # variance = sd^2 # eps is epsilon (error term)


### Problem (c)
y = -1 + 0.5 * X + eps
length(y)


### Problem (d)
plot(x, y)


### Problem (e)
fit4 <- lm(y ~ x)
summary(fit4)

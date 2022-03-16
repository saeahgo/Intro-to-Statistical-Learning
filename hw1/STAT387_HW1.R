# Applied
## 3. 9 (a, b, c, d, e, f)

#################################################
### Problem (a)
#################################################
data(Auto, package = "ISLR") # load the data
fix(Auto) # since I already have loaded the ISLR package with the "library" command, I don't need to use "read.table" command to load the "Auto" data. It is already loaded in R. I can view the file using the command "fix(Auto)".


#################################################
### Problem (b)
#################################################
range(Auto$mpg)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)


##################################################
### Problem (c)
##################################################
# Method 1 (simply use mean() and sd() functions)
mean(Auto$mpg)
mean(Auto$displacement)
mean(Auto$horsepower)
mean(Auto$weight)
mean(Auto$acceleration)

sd(Auto$mpg)
sd(Auto$displacement)
sd(Auto$horsepower)
sd(Auto$weight)
sd(Auto$acceleration)

# Method 2 (use colMeans() and colSds())
colMeans(Auto[,c(1, 3:6)])
colSds(as.matrix(Auto[,c(1, 3:6)])) # need to make the data to matrix form

###################################################
### Problem (d)
###################################################
# drop rows using slice() function in the dplyr package
new_Auto <- Auto %>% slice(-c(10:84)) # remove from 10th to 85th observations

# check the structure of the Auto dataset
str(Auto) # since name's structure is factor (not numeric values), I cannot get mean and standard deviation with the name column.

# find means and standard deviations of only numeric columns (except name)
colMeans(new_Auto[,c(1:8)])
colSds(as.matrix(new_Auto[,c(1:8)]))
sapply(new_Auto[, c(1:8)], range)


###################################################
### Problem (e)
###################################################
pairs(Auto) # method 1
pairs(~ mpg + cylinders + displacement + horsepower + weight + acceleration + year + origin + name, Auto) # method 2

plot(Auto$horsepower, Auto$mpg)
hist(Auto$acceleration)
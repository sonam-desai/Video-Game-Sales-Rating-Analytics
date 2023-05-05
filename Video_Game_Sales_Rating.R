rm(list=ls())

install.packages("leaps")
install.packages("glmnet")
install.packages("randomForest")
install.packages("tree")
library(tree)
library(randomForest)
library(ISLR2)
library(leaps)
library(glmnet)
library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
data = read_csv("C://Users//sonam//OneDrive - San Diego State University (SDSU.EDU)//Sem 2//Business Analytics//Project//Video_Games_Sales.csv")
#View(data)

data = na.omit(data)

data_subset <- data[, c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales","Critic_Score","Critic_Count","User_Score","User_Count")]
data_subset_all <- subset(data, select = -c(NA_Sales,EU_Sales,JP_Sales,Other_Sales ))

# Convert the x column to numeric
data_subset$NA_Sales <- as.numeric(data_subset$NA_Sales)
data_subset$EU_Sales <- as.numeric(data_subset$EU_Sales)
data_subset$JP_Sales <- as.numeric(data_subset$JP_Sales)
data_subset$Other_Sales <- as.numeric(data_subset$Other_Sales)
data_subset$Global_Sales <- as.numeric(data_subset$Global_Sales)
data_subset$Critic_Score <- as.numeric(data_subset$Critic_Score)
data_subset$Critic_Count <- as.numeric(data_subset$Critic_Count)
data_subset$User_Score <- as.numeric(data_subset$User_Score)
data_subset$User_Count <- as.numeric(data_subset$User_Count)

data_subset_all$Name <- as.factor(data_subset_all$Name)
data_subset_all$Platform <- as.factor(data_subset_all$Platform)
data_subset_all$Year_of_Release <- as.factor(data_subset_all$Year_of_Release)
data_subset_all$Genre <- as.factor(data_subset_all$Genre)
data_subset_all$Publisher <- as.factor(data_subset_all$Publisher)
data_subset_all$Developer <- as.factor(data_subset_all$Developer)
data_subset_all$Rating <- as.factor(data_subset_all$Rating)


data[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales", "Critic_Score", "Critic_Count", "User_Score", "User_Count")] <- sapply(data[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales", "Critic_Score", "Critic_Count", "User_Score", "User_Count")], as.numeric)
Video_Games_Sales <- data[complete.cases(data[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales", "Critic_Score", "Critic_Count", "User_Score", "User_Count")]), ]


# Create a histogram
log_NASales <- log(data_subset$NA_Sales)
hist(subset(log_NASales,log_NASales >= 0), breaks = 20, xlim = c(0, 5), col = 'green',xlab = 'NA_Sales (USD)', ylab = 'Video Games Frequency', main = 'NA Sales for Video Games')

log_EUSales <- log(data_subset$EU_Sales)
hist(subset(log_EUSales,log_EUSales >= 0), breaks = 20, xlim = c(0, 5), col = "yellow", xlab = 'EU_Sales (USD)', ylab = 'Video Games Frequency', main = 'EU Sales for Video Games')

log_JPSales <- log(data_subset$JP_Sales)
hist(subset(log_JPSales,log_JPSales >= 0), col = "blue", breaks = 20, xlim = c(0, 3), xlab = 'JP_Sales (USD)', ylab = 'Video Games Frequency', main = 'JP Sales for Video Games')

log_OtherSales <- log(data_subset$Other_Sales)
hist(subset(log_OtherSales,log_OtherSales >= 0), col = "orange", breaks = 10, xlim = c(0, 4), xlab = 'Other_Sales (USD)', ylab = 'Video Games Frequency', main = 'Other Sales for Video Games')

log_GlobalSales <- log(data_subset$Global_Sales)
hist(subset(log_GlobalSales,log_GlobalSales >= 0), breaks = 30, xlim = c(0, 5), col = 'red',xlab = 'Global_Sales (USD)', ylab = 'Video Games Frequency', main = 'Global Sales for Video Games')

hist(data_subset$Critic_Score, breaks = 20, xlim = c(0,100), col = 'Pink',xlab = 'Critic_Score ', ylab = 'Video Games Frequency', main = 'Critic Score for Video Games')
hist(data_subset$Critic_Count, breaks = 20, xlim = c(0, 120), col = 'Black',xlab = 'Critic_Count ', ylab = 'Video Games Frequency', main = 'Critic Count for Video Games')
hist(data_subset$User_Score, breaks = 20, xlim = c(0, 12), col = 'green',xlab = 'User_Score ', ylab = 'Video Games Frequency', main = 'User Score for Video Games')

log_UserCount <- log(data_subset$User_Count)
hist(subset(log_UserCount,log_UserCount >= 0), breaks = 20, xlim = c(0,10 ), col = 'green',xlab = 'User_Count', ylab = 'Video Games Frequency', main = 'User Count for Video Games')

log_User_Score <- log(data_subset$User_Score)
hist(subset(log_User_Score,log_User_Score >= 0), breaks = 20, xlim = c(0,3 ), col = 'orange',xlab = 'User_Score', ylab = 'Video Games Frequency', main = 'User Score for Video Games')

# creating pie charts for qualitative variable

# Create a data frame with the number of games for each platform
platform_count <- Video_Games_Sales %>%  count(Platform)
platform_percent <- round(platform_count$n / sum(platform_count$n) * 100, 2)
pie(platform_count$n, labels = paste0(platform_count$Platform, ": ", platform_percent, "%"), main = "Number of Games by Platform")

# Create a pie chart of the Year_of_Release
Year_of_Release_count <- Video_Games_Sales %>%  count(Year_of_Release)
Year_of_Release_percent <- round(Year_of_Release_count$n / sum(Year_of_Release_count$n) * 100, 2)
pie(Year_of_Release_count$n, labels = paste0(Year_of_Release_count$Year_of_Release, ": ", Year_of_Release_percent, "%"),main = "Years These Video games were released")

# Create a pie chart of the Genre
Genre_count <- Video_Games_Sales %>%  count(Genre)
Genre_percent <- round(Genre_count$n / sum(Genre_count$n) * 100, 2)
pie(Genre_count$n, labels = paste0(Genre_count$Genre, ": ", Genre_percent, "%"),main = "Video games Genres")

# Create a pie chart of the Publisher
#Publisher_count <- Video_Games_Sales %>%  count(Publisher)
#Publisher_percent <- round(Publisher_count$n / sum(Publisher_count$n) * 100, 2)
#pie(Publisher_count$n, labels = paste0(Publisher_count$Publisher, ": ", Publisher_percent, "%"),main = "Video games Publishers ")


# Create a pie chart of the Developer
#Developer_count <- Video_Games_Sales %>%  count(Developer)
#Developer_percent <- round(Developer_count$n / sum(Developer_count$n) * 100, 2)
#pie(Developer_count$n, labels = paste0(Developer_count$Developer, ": ", Developer_percent, "%"),main = "Video games Developers ")

# Create a pie chart of the Rating
Rating_count <- Video_Games_Sales %>%  count(Rating)
Rating_percent <- round(Rating_count$n / sum(Rating_count$n) * 100, 2)
pie(Rating_count$n, labels = paste0(Rating_count$Rating, ": ", Rating_percent, "%"),main = "Video games Ratings ")
#in case we need label or a list for pie charts.
#legend("right", legend = Rating_count$Rating, fill = rainbow(length(Rating_count$Rating)))

#Correlation Matrix - Quantitative Variables
cor_matrix <- cor(data_subset_all[,c("Critic_Score","Critic_Count", "User_Score", "User_Count","Global_Sales")])
corrplot(cor_matrix, method = "color")

#Scatter Plot
scatter_plot <- pairs(Video_Games_Sales[,c( "Critic_Score","Critic_Count", "User_Score", "User_Count","Global_Sales")])
scatter_plot

#Box Plot
# Create box plots for each level of a qualitative predictor

Video_Games_Sales$Name <- as.factor(Video_Games_Sales$Name)
Video_Games_Sales$Platform <- as.factor(Video_Games_Sales$Platform)
Video_Games_Sales$Year_of_Release <- as.factor(Video_Games_Sales$Year_of_Release)
Video_Games_Sales$Genre <- as.factor(Video_Games_Sales$Genre)
Video_Games_Sales$Publisher <- as.factor(Video_Games_Sales$Publisher)
Video_Games_Sales$Developer <- as.factor(Video_Games_Sales$Developer)
Video_Games_Sales$Rating <- as.factor(Video_Games_Sales$Rating)

boxplot(Global_Sales ~ Genre, data = Video_Games_Sales, main = "Box Plot for Genre")
boxplot(Global_Sales ~ Platform, data = Video_Games_Sales, main = "Box Plot for Platform")
boxplot(Global_Sales ~ Developer, data = Video_Games_Sales, main = "Box Plot for Developer")
boxplot(Global_Sales ~ Publisher, data = Video_Games_Sales, main = "Box Plot for Publisher")

#######Model Selection


n= dim(data_subset)[1]
set.seed(1210)
train.index = sample(n, 0.7*n)
train = data_subset[train.index, ]
test = data_subset[-train.index, ]

#1) Multivariate Regression Model
lm1 = lm(Global_Sales ~ Critic_Score + Critic_Count + User_Score + User_Count , data = train)
summary(lm1)
AIC(lm1)
BIC(lm1)

plot(train$Critic_Score, lm1$residuals) #residual plot
plot(train$Critic_Count, lm1$residuals) #residual plot
library(scatterplot3d)
scatterplot3d(train$Critic_Score, train$Critic_Score, lm1$residuals, highlight.3d = TRUE,
                col.axis = "blue", col.grid = "lightblue", main = "Helix", zlim=c(-4,4),
                pch = 20)


scatterplot3d(train$User_Score, train$User_Count, lm1$residuals, highlight.3d = TRUE,
              col.axis = "blue", col.grid = "lightblue", main = "Helix", zlim=c(-4,4),
              pch = 20)

lm1.pred1 = predict(lm1, test)
test.y = test$Global_Sales
mean((test.y - lm1.pred1)^2)

#Quadratic Polynomial
lm2 = lm(Global_Sales ~ poly(Critic_Score + Critic_Count + User_Score + User_Count, 2), data = train)
lm2.pred2 = predict(lm2, test)
mean((test.y - lm2.pred2)^2)

#Cubic Polynomial - Best
lm3 = lm(Global_Sales ~ poly(Critic_Score + Critic_Count + User_Score + User_Count, 3), data = train)
lm3.pred3 = predict(lm3, test)
mean((test.y - lm3.pred3)^2)

#### Stepwise Selection
### Best Subset Selection

data_subset1 <- subset(data_subset, select = -c(NA_Sales,EU_Sales,JP_Sales,Other_Sales ))

regfit.full = regsubsets(Global_Sales ~ ., data = data_subset1, nvmax = 12)  #nvmax is the maximum size of subsets to examine
reg.summary = summary(regfit.full)
reg.summary

names(reg.summary)
reg.summary$rsq  # r2 increases as number of variables increases.
reg.summary$adjr2   #adjusted R squared
reg.summary$bic   #BIC values

par(mfrow = c(1, 1))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
###
which.max(reg.summary$adjr2)
points(4, reg.summary$adjr2[4], col = "red", cex = 2, pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(3, reg.summary$bic[3], col = "red", cex = 2, pch = 20)


### Forward and Backward Stepwise Selection
###
regfit.fwd = regsubsets(Global_Sales ~ ., data = data_subset1, nvmax = 12, method = "forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Global_Sales ~ ., data = data_subset1, nvmax = 12, method = "backward")
summary(regfit.bwd)

## Ridge Regression and the Lasso

### split data into training data set and test data set
n=dim(data_subset1)[1]
set.seed(1)
train1.index=sample(n, round(0.7*n))
train1= data_subset1[train1.index,]
test1= data_subset1[-train1.index,]

### What happen if we fit a linear regression model？
lm4= lm(Global_Sales ~., data=train1)
summary(lm4)
lm4.pred= predict(lm4, newdata=test1)
mean(lm4$residuals^2)  #training error
y.test1= test1$Global_Sales
mean((lm4.pred - y.test1)^2)   #test/prediction error which is much larger than the training error. The data set maybe overfit.


### Ridge Regression
x = model.matrix(Global_Sales ~ ., data_subset1)[, -1]
y = data_subset$Global_Sales
x.train2= x[train1.index,]
x.test2= x[-train1.index,]
y.train2= y[train1.index]
y.test2 = y[-train1.index]


### Ridge Regression

###
set.seed(1) #select the optimal lambda
cv.out = cv.glmnet(x.train2, y.train2, alpha = 0) #cv.glmnet is used to select the best lambda.
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
#glmnent is used to fit a ridge regression.
ridge.mod = glmnet(x.train2, y.train2, alpha = 0, lambda = bestlam) 
ridge.pred = predict(ridge.mod, newx = x.test2)   #prediction
mean((ridge.pred - y.test2)^2)   #test error


#### Refit the model with all data, if ridge regression model is selected
out = glmnet(x, y, alpha = 0)   
predict(out, type = "coefficients", s = bestlam)[1:5, ]



### The Lasso
###
set.seed(1)
cv.out1 = cv.glmnet(x.train2, y.train2, alpha = 1)

plot(cv.out1)
bestlam1 = cv.out1$lambda.min
bestlam1
lasso.mod = glmnet(x.train2, y.train2, alpha = 1, lambda = bestlam1)
lasso.pred = predict(lasso.mod, s = bestlam1, newx = x.test2)
mean((lasso.pred - y.test2)^2)  # test error
lasso.coef = predict(lasso.mod, type = "coefficients")
lasso.coef


### Refit the model with all data, if lasso is selected.
lasso.mod = glmnet(x, y, alpha = 1, lambda = bestlam1)
lasso.coef = predict(lasso.mod, type="coefficients")
lasso.coef

###### Classification Tree

High = factor(ifelse(data_subset1$Global_Sales<=10.00, "No", "Yes"))
y.test3 = High[-train1.index]

train1 <- na.omit(train1)
out.tree1= tree(High~., data = train1)
#pdf("tree10.pdf", width=15, height=7)
plot(out.tree1)
text(out.tree1, pretty = 0)
#dev.off()
summary(out.tree1)

pred = predict(out.tree, newdata = test, type ='class')
pred
confusionMatrix(data = pred, reference = y.test)


##########Random Forest

set.seed(151) 
index4 = sample(n, round(0.5*n))
tree1 = tree(Global_Sales~., data = data_subset1, subset = index4)
summary(tree1)
plot(tree1)
text(tree1, pretty = 0)

set.seed(10121)  #10121
index5 = sample(n, round(0.7*n))
tree2 = tree(Global_Sales~., data = data_subset1, subset = index5)
summary(tree2)
plot(tree2)
text(tree2, pretty = 0)


lm5= lm(Global_Sales~., data = data_subset1, subset = index4)
lm6= lm(Global_Sales~., data = data_subset1, subset = index5)
summary(lm5)
summary(lm6)

pred.tree3 = predict(tree1, data_subset1)
pred.tree4 = predict(tree2, data_subset1)
pred.lm5 = predict(lm5, data_subset1)
pred.lm6 = predict(lm6, data_subset1)
mean((data_subset1$Global_Sales - pred.tree3)^2)
mean((data_subset1$Global_Sales - pred.tree4)^2)
mean((data_subset1$Global_Sales - pred.lm5)^2)
mean((data_subset1$Global_Sales - pred.lm6)^2)


par(mfrow=c(1,2))
plot(pred.tree3, pred.tree4)
abline(a = 0, b = 1)
plot(pred.lm5, pred.lm6)
abline(a = 0, b = 1)


####################################
### bagging, bagging is a special case of a random forest with m = p
set.seed(1)
train.index = sample(1:n, n/2)
train3 = data_subset1[train.index, ]
test3 = data_subset1[-train.index, ]
y.test3 = test3$Global_Sales
set.seed(1)
bag = randomForest(Global_Sales~., data = train3, mtry = 4, importance = TRUE)
bag

yhat.bag = predict(bag, newdata = test3)
#pdf("pred3.pdf", width=4, height=4)
plot(y.test3, yhat.bag)
abline(0, 1)
#dev.off()

mean((y.test3 - yhat.bag)^2)


#tree= tree(Global_Sales~., data = train3)
#cv = cv.tree(tree)
#prune = prune.tree(tree, best = cv$size[which.min(cv$dev)])

#pdf("prune_boston.pdf", width = 6, height = 4)
#plot(prune)
#text(prune, pretty=0)
#dev.off()

#yhat.tree= predict(prune, newdata = test3)
#mean((y.test3- yhat.tree)^2)


#pdf("pred4.pdf", width=4, height=4)
#plot(y.test3, yhat.tree)
#abline(0, 1)
#dev.off()




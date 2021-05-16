# Probability and Statistics Final Presentation
# MSIT
# Christen Cacanog, Elmo Ranolo, David Ybanez

# import data and omit
setwd("C:/Users/lenovo/Documents/Masteral/MSIT 1- 2/MIT 61301/File")
data <- read.csv("bigmartsalestrainset.csv")
bm_data <- na.omit(data[-1]) # remove rows with blank cells
head(bm_data)
str(bm_data)

# load libraries
library(ISLR)
library(MASS)
library(olsrr)
library(crayon)
library(cli)
library(ggplot2)
library(corrplot)
library(car) #package to calculate Variance Inflation Factor
library(leaps) #best subsets regression
library(glmnet) #allows ridge regression, LASSO and elastic net
library(caret) #parameter tuning
library(bestglm)


# create dummies, convert categorical values to columns with 1s or 0s
dummies=dummyVars(~.,data=bm_data,fullRank=TRUE)
dummies

# create data frame based from dummies
bm_df = as.data.frame(predict(dummies,newdata=bm_data))
head(bm_df)
plot(bm_df[1:11])

#Multiple graphs of bivariate
#I. Testing for Overall Significance
full=lm(bm_data$Item_MRP~., data = bm_df)
anova(full)
summary(full)
coefficients(full)

# Variable Selection
# Automatic Procedures
#Forward
forward<-ols_step_forward_p(full,data=bm_df,details=TRUE)
plot(forward)

#Backward
backward<-ols_step_backward_p(full,details=TRUE)
plot(backward)

#Stepwise 
stepwise<-ols_step_both_p(full,details=TRUE,pent=0.05,prem=0.05)
plot(stepwise)

#All possible
#full=lm(bm_data$Item_MRP~., data = PhD1)
#full
#k<-ols_step_all_possible(full)
#k
#View(k)
#plot(k)


#Best Subset (variables)
#k<-ols_step_best_subset(full)
#k
#View(k)
#plot(k)

#Test the new model
head(bm_df)
full1=lm(Item_MRP~Item_Outlet_Sales+Item_Weight+Item_Visibility, data = bm_df)
summary(full1)

#Normally Test
qqnorm (residuals (full1), ylab="Residuals")
qqline (residuals (full1))

#Shapiro-Wilk Normality Test
shapiro.test(residuals(full1))

#Kolmogorov-Smirnov Normality Test
ks.test(residuals(full1),rnorm(mean=0,sd=1,20),alternative="two.sided") 


#Homoscedasticity
#A. Graphical Analysis of Residuals
plot(fitted(full1),residuals(full1))

#B. Bartlett's Test Example
data2=c(fitted(full1),residuals(full1))
data2
rating=c(rep("predicted",5000),rep("residuals",5000))
data_2=data.frame(data2,rating)
data_2
bartlett.test(data2~rating, data=data_2)

#Multicollinearity
bm_data=read.csv("BigMart.csv")
bm_data=data.frame(bm_data)
cor=cor(PhD1)
corrplot(cor,method="pie") #Choices:"circle", "square", "ellipse", "number", "shade", "color", "pie". 
corrplot(cor,method="number")

#Formal Diagnostics Sample-if needs to dependent variables
library(car)
vif(lm(Item_MRP~Item_Outlet_Sales+Item_Weight+Item_Visibility, data = bm_df))

#IV. Independence
#A. Graphical Analysis of Residuals
#B. Durbin-Watson Test
library(lmtest)
dwtest(lm(Item_MRP~Item_Outlet_Sales+Item_Weight+Item_Visibility, data = bm_df))

#Leverage - Cooks Distance
full=lm(Item_MRP~Item_Outlet_Sales+Item_Weight+Item_Visibility, data = bm_df)
summary(full)
full
hatvalues(full)
plot(hatvalues(full),type="h") #Index 6                

plot(residuals(full),hatvalues(full))           
text(residuals(full),hatvalues(full),labels=rownames(bm_data),col="red")

cooks.distance(full)
plot(cooks.distance(full),hatvalues(full))
text(cooks.distance(full),hatvalues(full),labels=rownames(bm_data),col="red")
#same observation with the hatvalues
#affects the observation-An observation with Cook's Distance greater than 1 is worthy of inspection. 

#Predict and Confidence Interval
full=lm(Item_MRP~Item_Outlet_Sales+Item_Weight, data = bm_data)
summary(full)
new.current = data.frame(Item_MRP=1, Item_Outlet_Sales = 0.25, Item_Weight = 100)
predict (full, new.current, interval="prediction", level = 0.95 )

# Add regression plane
my.lm <- lm(df1$Item_MRP ~ df1$Item_Outlet_Sales + df1$Item_Weight, )
my.lm
s3d$plane3d(my.lm)
# Add supplementary points
s3d$points3d(0.25, 100, 52.36, type= "h", col="red", pch=10)

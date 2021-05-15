# Probability and Statistics Final Presentation
# MSIT
# Christen Cacanog, Elmo Ranolo, David Ybanez

# import data and omit
setwd("C:/devtools/data/")
data <- read.csv("bigmartsalestrainset.csv")
bm_data <- na.omit(data[-1]) # remove rows with blank cells
head(bm_data)

# load libraries
# Before loading the libraries, make sure to install them
# using install.packages("<package_name>")
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
full = lm(bm_df$Item_Outlet_Sales~., data = bm_df)
summary(full)

#Best Subset (variables)
k<-ols_step_best_subset(full)
k
View(k)
plot(k)

# export data frame to CSV
# write.csv(bm_df, "bm_dummies.csv")

# create data frame based from selected variables
bm_final = data.frame(bm_df$Item_Outlet_Sales,
                      bm_df$Item_Fat_ContentRegular,
                      bm_df$"Item_Fat_ContentLow Fat",
                      bm_df$Item_Weight,
                      bm_df$Item_Visibility,
                      bm_df$Item_MRP)
head(bm_final)

full = lm(
  bm_final$bm_df.Item_Outlet_Sales~
    bm_final$bm_df.Item_MRP+
    bm_final$bm_df.Item_Visibility+
    bm_final$bm_df.Item_Weight+
    bm_final$bm_df..Item_Fat_ContentLow.Fat.+
    bm_final$bm_df.Item_Fat_ContentRegular)
summary(full)

coeff = coefficients(full)
coeff

#plot
eq = paste("y = ", round(coeff[2], 4), "*x", "+", round(coeff[1], 4)) #round to 4 digits
ggplot(bm_final,
       aes(x = bm_df.Item_MRP,
           y = bm_df.Item_Outlet_Sales)) +
  geom_point(alpha = .1, size = 1) +
  geom_smooth(se = FALSE,
              method = "lm",
              formula = y~poly(x, 1),
              size = 1.5) +
  labs(x = "Maximum Retail Price (list price) of the product",
       title = "Simple Linear Regression",
       subtitle = eq, 
       y = "Sales of the product in a particular store/outlet") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# Probability and Statistics Final Project
# MSIT
# Christen Cacanog, Elmo Ranolo, David Ybanez

# import data
setwd("C:/devtools/data/")
data <- read.csv("bigmartsalestrainset.csv")
head(data) # preview the data

# load libraries
# Before loading the libraries, make sure to install them
# using install.packages("<package_name>")
library(gridExtra) # display plots in grid format
library(dplyr) # data manipulation (group_by)
library(ggplot2)
library(cowplot) # for plot_grid
library(stringr) # for str_replace
library(caret) # parameter tuning. for dummyVars
library(dummies) # for dummies
library(olsrr) # for OLS regression models, for ols_* functions

# 0. Data Cleaning

# 0.1. Check for blanks in the dataset
summary(data) # Item_Weight has 1463 NA's and should be removed

# 0.2. Omitting rows with empty cells
nrow(data) # total rows before omitting
bm_data <- na.omit(data) # use bm_data moving forward
nrow(bm_data) # total rows after omitting

# 1. Exploratory data analysis
str(bm_data) # show structure of data

# 1.1. Univariate Analysis

# 1.1.1. Numerical Variables
bm_data_numeric = dplyr::select_if(bm_data, is.numeric)
names(bm_data_numeric)

# We only have three numeric predictor columns because technically
# Outlet_Establishment_Year is a categorical variable.

# Plot for Item_Weight
plot_weight <- ggplot(bm_data) +
  geom_histogram(
    aes(Item_Weight),
    color = 'black',
    fill = '#E45B5B')

# Plot for Item_Visibility
plot_visibility <- ggplot(bm_data) +
  geom_histogram(
    aes(Item_Visibility),
    color = 'black',
    fill = '#5FE45B')

# Plot for Item_MRP]
plot_mrp <- ggplot(bm_data) +
  geom_histogram(
    aes(Item_MRP),
    color = 'black',
    fill = '#3D9DDC'
    #binwidth = 0.75
  )

# Combine all plots into one grid
grid.arrange(plot_weight, plot_visibility, plot_mrp, ncol = 3, nrow = 1)

# 1.1.2. Categorical Variables
# https://stackoverflow.com/questions/27125672/what-does-function-mean-in-r

# Item_Type
ggplot(bm_data %>% group_by(Item_Type) %>% summarise(Count = n())) + # %>% is a pipe function
  geom_bar(
    aes(
      Item_Type, 
      Count, 
      fill = interaction(Item_Type, Count, sep = ": ")), 
    stat = "identity") +
  xlab("") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8)) +
  ggtitle("Item_Type")

# Outlet_Size
plot_outlet_size <- ggplot(bm_data %>% group_by(Outlet_Size) %>% summarise(Count = n())) +
  geom_bar(
    aes(Outlet_Size, Count),
    stat = "identity",
    fill = "#6C3DDC") +
  geom_label(
    aes(Outlet_Size, Count, label = Count),
    vjust = 0.5, size =2.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Item_Fat_Content
plot_item_fat_content <- ggplot(bm_data %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +
  geom_bar(
    aes(Item_Fat_Content, Count),
    stat = "identity",
    fill = "#DC3DC7") +
  geom_label(
    aes(Item_Fat_Content, Count, label = Count),
    vjust = 0.5, size = 2.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(plot_outlet_size, plot_item_fat_content, ncol = 2)

# 1.2. Multivariate Analysis
# Check for relationships

# Item_Weight and Item_Outlet_Sales
plot_weight_sales <- ggplot(bm_data) +
  geom_point(
    aes(Item_Weight, Item_Outlet_Sales),
    colour = "#FB5555",
    alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Item_Visibility and Item_Outlet_Sales
plot_visibility_sales <- ggplot(bm_data) +
  geom_point(
    aes(Item_Visibility, Item_Outlet_Sales),
    colour = "#55A3FB",
    alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Item_MRP and Item_Outlet_Sales
plot_mrp_sales <- ggplot(bm_data) +
  geom_point(
    aes(Item_MRP, Item_Outlet_Sales),
    colour = "#45B74C", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

sec_row = plot_grid(plot_visibility_sales, plot_mrp_sales, ncol = 2)
plot_grid(sec_row, plot_weight_sales, nrow = 2)

# Outlet_Identifier, Outlet_Type, and Item_Outlet_Sales
plot_identifier_type_sales <- ggplot(bm_data) +
  geom_boxplot(
    aes(Outlet_Identifier, sqrt(Item_Outlet_Sales), fill = Outlet_Type)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
plot_identifier_type_sales


# 2. Data Pre-processing

# 2.1. Combine Item_Fat_Content categories
bm_data$Item_Fat_Content <-str_replace(
  str_replace(str_replace(bm_data$Item_Fat_Content,"LF","Low Fat"),"reg","Regular"),"low fat","Low Fat")
table(bm_data$Item_Fat_Content)

# 2.2. Create Outlet_Age and Item_Category columns
bm_data <- bm_data %>% 
  mutate(Item_Category = substr(Item_Identifier, 1, 2),
         Outlet_Age = 2013 - Outlet_Establishment_Year)
table(bm_data$Item_Category)

# 2.3. Convert non-food items to Non-Edible category in Item_Category
bm_data$Item_Fat_Content[bm_data$Item_Category == "NC"] = "Non-Edible" 
table(bm_data$Item_Fat_Content)

# 2.4. Convert categorical data to numerical. One Hot Encoding (dummies/dummyVars)
# dummies = dummyVars(~.,data=bm_data,fullRank=TRUE)
# dummies
bm_data <- dummy.data.frame(
  bm_data,
  names = c(
    'Item_Fat_Content',
    'Outlet_Size',
    'Outlet_Location_Type',
    'Outlet_Type',
    'Item_Category',
    'Outlet_Identifier'),
  sep ='_')

summary(bm_data)
# 2.5. Create data frame from dummies
# bm_df = as.data.frame(predict(dummies,newdata=bm_data))

bm_df <- subset(
  bm_data,
  select = -c(Item_Identifier, Item_Type, Outlet_Establishment_Year))
str(bm_df)
summary(bm_df)

# 3. Exploratory data analysis
View(bm_df)
plot(bm_df[1:11]) # 29 is too many. need to wait a couple minutes to finish the plot

bm_lm = lm(bm_df$Item_Outlet_Sales~., data = bm_df)
anova(bm_lm)
summary(bm_lm)
coefficients(bm_lm)

# Variable selection
# Automatic Procedures

# Forward
olssforwardp_bm_lm <- ols_step_forward_p(bm_lm, data = bm_df, details = TRUE)
plot(olssforwardp_bm_lm)

# Backward
olssbackp_bm_lm <- ols_step_backward_p(bm_lm,details=TRUE)
plot(olssbackp_bm_lm)

# Stepwise
#olssbothp_bm_lm <- ols_step_both_p(bm_lm, details=TRUE, pent=0.05, prem=0.05) # error
#plot(olssbothp_bm_lm)

# All possible
#if(.Platform$OS.type == "windows") withAutoprint({
#  memory.size()
#  memory.size(TRUE)
#  memory.limit()
#})
#memory.limit(size=56000)
#olssap_bm_lm <- ols_step_all_possible(bm_lm)

#Best Subset (variables)
olssbs_bm_lm <- ols_step_best_subset(bm_lm)
View(olssbs_bm_lm)
plot(olssbs_bm_lm)

# 4. Test the new model














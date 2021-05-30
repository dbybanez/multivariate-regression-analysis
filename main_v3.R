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
library(sqldf) # SQL queries

# 1. Data Inspection

# Check the summary of data
summary(data)

# Check which columns are categorical and numerical
data_numeric = dplyr::select_if(data, is.numeric) # numerical
names(data_numeric) 

data_categorical = dplyr::select_if(data, is.character) # categorical
names(data_categorical) 

# Check for blanks: numerical variables
item_weight_blanks     = sqldf("SELECT COUNT(*) AS Item_Weight_Blanks FROM data WHERE Item_Weight IS NULL OR Item_Weight = ''") 
item_weight_blanks     # 1463 blanks for Item_Weight
item_visibility_blanks = sqldf("SELECT COUNT(*) AS Item_Visiblity_Blanks FROM data WHERE Item_Visibility IS NULL OR Item_Visibility = ''") 
item_visibility_blanks # no blanks for Item_Visibility
item_MRP_blanks        = sqldf("SELECT COUNT(*) AS Item_MRP FROM data WHERE Item_MRP IS NULL OR Item_MRP = ''") 
item_MRP_blanks        # no blanks for Item_MRP
outlet_year_blanks     = sqldf("SELECT COUNT(*) AS Outlet_Establishment_Year_Blanks FROM data WHERE Outlet_Establishment_Year IS NULL OR Outlet_Establishment_Year = ''") 
outlet_year_blanks     # no blanks for Outlet_Establishment_Year
outlet_sales_blanks    = sqldf("SELECT COUNT(*) AS Item_Outlet_Sales_Blanks FROM data WHERE Item_Outlet_Sales IS NULL OR Item_Outlet_Sales = ''")
outlet_sales_blanks    # no blanks for Item_Outlet_Sales

# Check for blanks: categorical variables
item_identifier_blanks   = sqldf("SELECT COUNT(*) AS Item_Identifier_Blanks FROM data WHERE Item_Identifier IS NULL OR Item_Identifier = ''") 
item_identifier_blanks   # no blanks for Item_Identifier
item_fat_content_blanks  = sqldf("SELECT COUNT(*) AS Item_Fat_Content_Blanks FROM data WHERE Item_Fat_Content IS NULL OR Item_Fat_Content = ''") 
item_fat_content_blanks  # no blanks for Item_Fat_Content
item_type_blanks         = sqldf("SELECT COUNT(*) AS Item_Type_Blanks FROM data WHERE Item_Type IS NULL OR Item_Type = ''") 
item_type_blanks         # no blanks for Item_Type
outlet_identifier_blanks = sqldf("SELECT COUNT(*) AS Outlet_Identifier_Blanks FROM data WHERE Outlet_Identifier IS NULL OR Outlet_Identifier = ''") 
outlet_identifier_blanks # no blanks for Outlet_Identifier
outlet_size_blanks       = sqldf("SELECT COUNT(*) AS Outlet_Size_Blanks FROM data WHERE Outlet_Size IS NULL OR Outlet_Size = ''") 
outlet_size_blanks       # 2410 blanks for Outlet_Size
outlet_location_blanks   = sqldf("SELECT COUNT(*) AS Outlet_Location_Type_Blanks FROM data WHERE Outlet_Location_Type IS NULL OR Outlet_Location_Type = ''") 
outlet_location_blanks   # no blanks for Outlet_Location_Type
outlet_type              = sqldf("SELECT COUNT(*) AS Outlet_Type_Blanks FROM data WHERE Outlet_Type IS NULL OR Outlet_Type = ''")
outlet_type              # no blanks for Outlet_Type

# Check for inconsistencies in the data: numerical variables
head(table(data$Item_Weight))         # Item_Weight
head(table(data$Item_Visibility))     # Item_Visibility
head(table(data$Item_MRP))            # Item_MRP
table(data$Outlet_Establishment_Year) # Outlet_Establishment_Year
head(table(data$Item_Outlet_Sales))   # Item_Outlet_Sales

# Check for inconsistencies in the data: categorical variables
head(table(data$Item_Identifier)) # Item_Identifier
table(data$Item_Fat_Content)      # Item_Fat_Content
head(table(data$Item_Type))       # Item_Type
table(data$Outlet_Identifier)     # Outlet_Identifier
table(data$Outlet_Size)           # Outlet_Size
table(data$Outlet_Location_Type)  # Outlet_Location_Type
table(data$Outlet_Type)           # Outlet_Type

# 2. Exploratory data analysis

# 2.1 Univariate Analysis 

# 2.1.1 Numerical Variables
# We only have three numerical variables since technically Outlet_Establishment_Year is a categorical variable.
# And we won't be using Item_Outlet_Sales because it's the Target Variable or dependent variable we want to predict. 

# Plot for Item_Weight
plot_weight <- ggplot(data) +
  geom_histogram(
    aes(Item_Weight),
    color = 'black',
    fill = '#E45B5B')

# Plot for Item_Visibility
plot_visibility <- ggplot(data) +
  geom_histogram(
    aes(Item_Visibility),
    color = 'black',
    fill = '#5FE45B')

# Plot for Item_MRP]
plot_mrp <- ggplot(data) +
  geom_histogram(
    aes(Item_MRP),
    color = 'black',
    fill = '#3D9DDC'
    #binwidth = 0.75
  )

# Combine all plots into one grid
grid.arrange(plot_weight, plot_visibility, plot_mrp, ncol = 3, nrow = 1)

# 2.1.2 Categorical Variables
# We won't be performing the analysis for Item_Identifier because it has a lot of unique values.
# R reference: https://stackoverflow.com/questions/27125672/what-does-function-mean-in-r

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

grid.arrange(plot_item_fat_content, plot_outlet_size, ncol = 2)

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


# 2. Data Cleaning

# Omitting blanks
# Although in data science, as much as 
nrow(data) # total rows before omitting
bm_data <- na.omit(data) # use bm_data moving forward
nrow(bm_data) # total rows after omitting


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














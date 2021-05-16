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
library(gridExtra)

# 0.0. Data Cleaning

# 0.1. Omitting rows with empty cells
nrow(data) # total rows before omitting
bm_data <- na.omit(data) # use bm_data moving forward
nrow(bm_data)

# 1.0. Exploratory data analysis
str(data) # show structure of data

# 1.1. Univariate Analysis
# Lists all tables with numeric data types 
data_numeric = dplyr::select_if(data, is.numeric)
names(data_numeric)

# We only have three numeric predictor columns because technically
# Outlet_Establishment_Year is a categorical variable.

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

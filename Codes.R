# ----- Load the required packages
# install.packages("tidyverse")
# install.packages("gridExtra")
# install.packages("hexbin")
library(tidyverse)
library(gridExtra)
library(hexbin)

# ------ Data Import and Preparation
# -!!- setwd("D:/Jackie/Documents/Data Science Projects/20180124 BigMartSales/2. Prepared Data")
# Please set your own working directory
# Dataset is from this link: https://datahack.analyticsvidhya.com/contest/practice-problem-big-mart-sales-iii/ #
martdataset_train <- read.csv("Train_UWu5bXk.csv")
martdataset_test <- read.csv("Test_u94Q5KV.csv")

dim(martdataset_train) #8523 observations #12 variables (60%)
dim(martdataset_test) #5681 observations #11 variables (40%)

#>>> Combine Training set and test set
martdataset_test$Item_Outlet_Sales <- rep(NA, nrow(martdataset_test))
martdataset <- rbind(martdataset_train, martdataset_test)
dim(martdataset) #14204 observations #12 variables

#>>> Dealing with missing values and combine coherent values
#> change "LF" and "low fat" to "Low Fat"
#> change "reg" to "Regular"
for (i in 1:length(martdataset$Item_Fat_Content)){
        if (martdataset$Item_Fat_Content[i] %in% c("LF", "low fat")){
                martdataset$Item_Fat_Content[i] <- "Low Fat"    
        } else if (martdataset$Item_Fat_Content[i] %in% c("reg")){
                martdataset$Item_Fat_Content[i] <- "Regular"
        }
}
martdataset$Item_Fat_Content <- factor(as.character(martdataset$Item_Fat_Content))
                                       
#> Change the "" in the outlet_size to NA
martdataset$Outlet_Size <- as.character(martdataset$Outlet_Size)
martdataset$Outlet_Size <- ifelse(martdataset$Outlet_Size %in% c(""), 
                                  NA, 
                                  martdataset$Outlet_Size)

#> Compute NA in item_weight by its mean value
martdataset$Item_Weight <- ifelse(is.na(martdataset$Item_Weight), 
                                  mean(martdataset$Item_Weight, na.rm = T), 
                                  martdataset$Item_Weight)

#>>> Create new variable, Log2_MRP, for easy comparison of retail price
martdataset <- martdataset %>%
        mutate(Log2_MRP = log2(Item_MRP))

#>>> Re-arrange the columns
sale <- martdataset %>% 
        select(Item_Outlet_Sales)
no_sale <- martdataset %>% 
        select(everything(), -Item_Outlet_Sales)
martdataset <- cbind(no_sale, sale)

#>>> Separate into training set and test set
martdataset_train <- martdataset[1:8523, ]
martdataset_test <- martdataset[8524:14204, -13]
rm(martdataset, sale, no_sale)
martdataset_train <- as_tibble(martdataset_train)
martdataset_test <- as_tibble(martdataset_test)

# ---------- Exploratory Data Analysis

# ----- Item Sales
figure1a <- martdataset_train %>% 
        ggplot(aes(x = Item_Outlet_Sales)) +
        geom_histogram(binwidth = 500, color = "white") +
        xlab("Item Sales") +
        ggtitle("The Distribution of Item Sales")
figure1b <- martdataset_train %>% 
        ggplot(aes(x = Item_Outlet_Sales)) +
        geom_histogram(binwidth = 500, color = "white") +
        coord_cartesian(ylim = c(0, 25)) + 
        xlab("Item Sales") +
        ggtitle("Zoom in the y-axis of the Distribution of Item Sales")
grid.arrange(figure1a, figure1b)


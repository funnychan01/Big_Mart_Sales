# ---------- Load the required packages
library(tidyverse)
library(gridExtra)
library(hexbin)


# ---------- Data Import and Preparation
# -!!- setwd("D:/Jackie/Documents/Data Science Projects/20180124 BigMartSales/2. Prepared Data")
# Please set your own working directory
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

#> Change the "" in the outlet_size to NA
martdataset$Outlet_Size <- as.character(martdataset$Outlet_Size)
martdataset$Outlet_Size <- ifelse(martdataset$Outlet_Size %in% c(""), 
                                  "Unknown", 
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



# ---------- Encoding for Modeling
#>>> Combine dataset
martdataset_test$Item_Outlet_Sales <- rep(NA, nrow(martdataset_test))
martdataset <- rbind(martdataset_train, martdataset_test)

martdataset$Item_Fat_Content <- factor(as.character(martdataset$Item_Fat_Content),
                                       levels = c("Low Fat", "Regular"),
                                       labels = c(0, 1))

#>>> Encoding categorical variables
martdataset$Outlet_Size <- factor(martdataset$Outlet_Size, 
                                  levels = c("Small", "Medium", "High", "Unknown"),
                                  labels = c(0, 1, 2, 3))

martdataset$Outlet_Location_Type <- factor(martdataset$Outlet_Location_Type,
                                           levels = c("Tier 1", "Tier 2", "Tier 3"),
                                           labels = c(1, 2, 3))

martdataset$Outlet_Type <- factor(martdataset$Outlet_Type,
                                  levels = c("Grocery Store", "Supermarket Type1", "Supermarket Type2", "Supermarket Type3"),
                                  labels = c(0, 1, 2, 3))

#>>> Separate into training set and test set
martdataset_train <- martdataset[1:8523, ]
martdataset_test <- martdataset[8524:14204, -13]
rm(martdataset, sale, no_sale)
martdataset_train <- as_tibble(martdataset_train)
martdataset_test <- as_tibble(martdataset_test)
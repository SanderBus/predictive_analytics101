#path<-"C:/Users/sander/Desktop/BI/analyticsvidhya
#           /practice_problem-big_mart_sales_III"
#setwd(path)

train<-read.csv("Train_UWu5bXk.csv")
#test<-read.csv("Test_u94Q5KV.csv")

#First step in data science is always inspecting your data
#Second step find and "repair" faulty or missing data
#Third step is to turn categorized information (e.g. strings) into binary format
#  e.g. hair_color column has possibilities per individual of brown, blond, grey.
#       so one needs to turn this one column into three new ones consisting named
#       brown, blond and grey. The values these columns can have 
#       are 0 and 1 (i.e. yes or no)

#Inspection step:
# Excel

#structure of the table
#print(str(train))

#statistics per column of the table
print(summary(train))

#plotting to find possible correlations
#library(ggplot2)
#ggplot(train,aes(x=Outlet_Establishment_Year,y=Item_Outlet_Sales)) 
#       + geom_point(size=0.5,color='navy')

#Find NA and NaNs
#print(is.na(train))     #Find NA's in training table
print(colSums(is.na(train)))   #Find number of NA's per column

#Problems in current table:
#  1. Item_Fat_content contains multiple names for the same thing, 
#     e.g. Low Fat and LF
#  2. Non realistic values: Minimum value of item_visibility is zero. 
#     We will treat 0s as missing values.
#  3. Missing values: item_weight misses values
#  4. Missing factor levels: Outlet_size has small, medium, high and ...

#We need to fix these before we can go into analyzing the data

# 1. multiple names for the same state
library(plyr)    #its function revalue is a rename
train$Item_Fat_Content <- revalue(train$Item_Fat_Content, 
                                  c("LF" = "Low Fat", "reg" = "Regular"))
train$Item_Fat_Content <- revalue(train$Item_Fat_Content, 
                                  c("low fat" = "Low Fat"))

# 2. Non realistic values
foo = train$Item_Visibility
train$Item_Visibility[foo==0] <- median(foo[foo>0])
rm(foo)

# 3. missing values (give median (stable to outliers) as value 
#    for missing weights)
train$Item_Weight[is.na(train$Item_Weight)] <- median(train$Item_Weight, 
                                                      na.rm = TRUE)

# 4. missing name for levels
levels(train$Outlet_Size)[1] <- "Other"

#Now we have a well behaved table
print(summary(train))

# An intermediate step can be to create new categories from the data already
# available, for example split all products in the categories food, drinks and
# non-consumable (information is already there in the Item_identifier.)
q <- substr(train$Item_Identifier,1,2)  #take a substring (first two elements)
q <- gsub("FD","Food",q)
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)

print(table(q))

train$Item_Type_New <- q    # Create a new variable in the train object and fill
                            # it with q
rm(q)

print(summary(train))
options(max.print=500)
print(train)

# Next step is to turn human language strings into computer values
# Label encoding if there are just two options:
# i.e. replace terms by encoding, e.g. regular <- 1 and Low Fat <- 0
train$Item_Fat_Content <- ifelse(train$Item_Fat_Content == "Regular",1,0)

# Label encoding if there are more than two options requires creation of extra
# columns. i.e. each possibility in a column gets a new column.

library(dummies)
train <- dummy.data.frame(train, 
                          names = c('Outlet_Size','Item_Type',
                                    'Outlet_Location_Type','Outlet_Type',
                                    'Item_Type_New'), sep='_')

print(summary(train))
print(str(train))

#Drop the Item and Outlet Identifiers and the data preparation stage is ready
#Data inspection and thinking about the data should still be repeated
#Maybe including some extra columns to our table

#
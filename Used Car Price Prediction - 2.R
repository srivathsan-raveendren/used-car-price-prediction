---
Title: "Used Car Price Prediction"
author: "Srivathsan Raveendren & Rahul Jain"
Date: "8 December 2024"
---
Libraries
```{r}
install.packages("dplR")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("stringr")
library(stringr)
install.packages("ipred")
library(ipred)
install.packages("caret")
library(caret)
install.packages("ggplot2")
library(ggplot2)
install.packages("forcats")
library(forcats)
install.packages("scales")
library(scales)
install.packages("reshape2")
library(reshape2)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("NeuralNetTools")
library(NeuralNetTools)
install.packages("glmnet")
library(glmnet)
```



setwd("C:/Users/HP/OneDrive/Desktop/C/Big Data")
usedcars_df = read.csv("Used car sale.csv")
```{r}

head(usedcars_df)
summary(usedcars_df$selling_price)
usedcars <- usedcars_df %>% rename(Car_Make=name,
                                   Year=year,
                                   Price=selling_price,
                                   Mileage=km_driven,
                                   Fuel_Type=fuel,
                                   Seller_Type=seller_type,
                                   Transmission=transmission,
                                   Num_Owners=owner)
head(usedcars,5)

```
Mutating the name column into car make and car model 
```{r}
usedcars <-extract(usedcars,Car_Make,c("Car_Make","Car_Model"), "([^ ]+) (.*)")
head(usedcars,5)
```
Checking for missing values
```{r}
colSums(is.na(usedcars))
```


Feature Engineering

```{r}
# Current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Feature Engineering
usedcars <- usedcars %>%
  # Create Age of the car feature
  mutate(Car_Age = current_year - Year,
         # Create Price per Kilometer driven feature
         Price_per_KM = ifelse(Mileage > 0, ceiling((Price / Mileage) * 100) / 100))
summary(usedcars)

```

#******
#EDA
#******


```{r}
1


# Histogram for Selling Price

plot1 <- ggplot(usedcars, aes(x = Price)) +
  geom_histogram(bins = 20, fill = "darkorange", color = "black", alpha = 0.6) +
  geom_density(aes(y = ..count..), color = "darkred", size = 1) +
  scale_x_continuous(breaks = seq(0, 6000000, by = 500000), limits = c(0, 6500000)) +
  labs(title = "Histogram of Selling Price", x = "Selling Price", y = "Frequency") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # Rotate labels vertically
    panel.grid = element_blank() # Remove gridlines
  )

hist_data <- ggplot_build(plot1)$data[[1]]



# Histogram for Selling Price
plot2 <- ggplot(usedcars, aes(x = Price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Selling Price", x = "Selling Price", y = "Frequency") +
theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # Rotate labels vertically
    panel.grid = element_blank() # Remove gridlines
  )



# Histogram for Car count by Year
plot3 <- ggplot(usedcars, aes(x = Year)) +
  geom_histogram(bins = 30, fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "", x = "Year", y = "Count") +
  scale_y_continuous(breaks = seq(0, max(table(usedcars$Year)), by = 50)) + # Set y-axis intervals
  scale_x_continuous(breaks = seq(min(usedcars$Year), max(usedcars$Year), by = 1)) +
  theme_minimal()+
 theme(
   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
panel.grid = element_blank()
)

# Histogram for Car count by Car Make
plot4 <- ggplot(usedcars, aes(x = fct_reorder(Car_Make, -table(Car_Make)[Car_Make]))) +
  geom_bar(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Number of Cars by Make", x = "Car Make", y = "Count") +
  scale_y_continuous(breaks = seq(0, max(table(usedcars$Car_Make)), by = 100)) + # Set y-axis intervals
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # Rotate labels vertically
    panel.grid = element_blank() # Remove gridlines
  )

usedcars$Mileage_Binned <- cut(
  usedcars$Mileage,
  breaks = seq(min(usedcars$Mileage), max(usedcars$Mileage), by = 10000),
  include.lowest = TRUE
)

# Density Plot: Mileage vs. Price
plot5 <- ggplot(usedcars, aes(x = Mileage, y = Price)) +
  geom_hex(bins = 30) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Density Plot: Mileage vs. Price",
    x = "Mileage (Kilometers Driven)",
    y = "Price",
    fill = "Count"
  ) +
  scale_x_continuous(labels = label_comma()) +  # Format x-axis without scientific notation
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Bar Plot for Fuel Type
plot6 <- ggplot(usedcars, aes(x = Fuel_Type)) +
  geom_bar(fill = "darkblue", color = "black", alpha = 0.7) +
  labs(title = "", x = "Fuel Type", y = "Count") +
  theme(
      panel.grid = element_blank() # Remove gridlines
  )

# Bar plot for Seller Type Frequency
plot7 <- ggplot(usedcars, aes(x = Seller_Type)) +
  geom_bar(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "", x = "Seller Type", y = "Count") +
  theme(
    panel.grid = element_blank() # Remove gridlines
  )

# Bar plot for Transmission Type Frequency
plot8 <- ggplot(usedcars, aes(x = Transmission)) +
  geom_bar(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "", x = "Transmission Type", y = "Count") +
  theme(
    panel.grid = element_blank() # Remove gridlines
  )

# Scatter Plot for Selling Price vs Kilometers Driven
plot9 <- ggplot(usedcars, aes(x = Mileage, y = Price)) +
  geom_point(color = "red", alpha = 0.6) +
  labs(title = "Scatter Plot: Selling Price vs Kilometers Driven", 
       x = "Kilometers Driven", y = "Selling Price") +
  scale_x_continuous(labels = comma) + 
  theme(
    panel.grid = element_blank()
  )

# Boxplot for Selling Price by Seller Type
plot10 <- ggplot(usedcars, aes(x = Seller_Type, y = Price)) +
  geom_boxplot(fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Selling Price by Seller Type", x = "Seller Type", y = "Selling Price") +
  theme(
    panel.grid = element_blank()
  )





plot1
plot2
plot3
plot4
plot5
plot6
plot7
plot8
plot9
```

```{r}

# Scatter plot: Selling Price vs. Car Age
plot11 <- ggplot(usedcars, aes(x = Car_Age, y = Price)) +
  geom_point(color = "orange", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Selling Price vs. Car Age", x = "Car Age (Years)", y = "Selling Price") +
  scale_x_continuous(breaks = seq(0, max(table(usedcars$Car_Age)), by = 3)) + # Set y-axis intervals
  theme(
    panel.grid = element_blank()
  )

# Boxplot: Selling Price by Number of Owners
plot12 <- ggplot(usedcars, aes(x = Num_Owners, y = Price)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.color = "red") +
  labs(title = "Selling Price by Number of Owners", x = "Number of Owners", y = "Selling Price") +
  theme(
    panel.grid = element_blank()
  )

# Boxplot: Selling Price by Transmission Type
plot13 <- ggplot(usedcars, aes(x = Transmission, y = Price)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red") +
  labs(title = "Selling Price by Transmission Type", x = "Transmission Type", y = "Selling Price") +
  theme(
    panel.grid = element_blank()
  )

plot10
plot11
plot12
plot13

```


Checking for multi-collinearity 

```{r}

# Calculate correlation matrix for numerical features
numerical_features <- usedcars[, c("Price", "Car_Age", "Mileage","Year", "Price_per_KM")]
correlation_matrix <- cor(numerical_features, use = "complete.obs")

# Create a heatmap using ggcorrplot
ggcorrplot(correlation_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 4, 
           title = "Correlation Heatmap for Used Cars Dataset", 
           colors = c("red", "white", "blue"), 
           outline.color = "black")
```

```{r}

# Select numerical columns
numeric_cols <- usedcars[, sapply(usedcars, is.numeric)]
cor_matrix <- cor(numeric_cols)

# Melt the correlation matrix for ggplot
melted_cor <- melt(cor_matrix)

# Plot Heatmap
plot10 <- ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab") +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
print(plot10)

```



Dealing with the Outliers 


```{r}
# Count the number of cars sold in each year
cars_sold_per_year <- usedcars %>%
  group_by(Year) %>%
  summarise(Count = n()) %>%
  arrange(Year)

# Display the table
print(cars_sold_per_year)

str(usedcars)
```
  


Years from 1992 to 2003 have very few cars in the dataset, making it difficult for the model to learn trends or patterns specific to these years.We are deleting this values. 
```{r}
usedcars$Year <- as.numeric(usedcars$Year)

# Remove outliers based on the specified year range
usedcars <- usedcars %>%
  filter(!(Year >= 1992 & Year <= 1997))

cars_sold_per_year <- usedcars %>%
  group_by(Year) %>%
  summarise(Count = n()) %>%
  arrange(Year)

# Display the table
print(cars_sold_per_year)

```


Removing the  rows where fuel is 'Electric' because, we have single value present in the data set


```{r}
cars_sold_per_Fuel_Type <- usedcars %>%
  group_by(Fuel_Type) %>%
  summarise(Count = n()) %>%
  arrange(Fuel_Type)

# Display the table
print(cars_sold_per_Fuel_Type)
```



```{r}
data_cleaned <- usedcars[usedcars$Fuel_Type!= "Electric", ]
usedcars = data_cleaned
cars_sold_per_Fuel_Type <- usedcars %>%
  group_by(Fuel_Type) %>%
  summarise(Count = n()) %>%
  arrange(Fuel_Type)

# Display the table
print(cars_sold_per_Fuel_Type)


```
Removing outliers 
```{r}
install.packages("kableExtra")
library(kableExtra)

# Count the number of cars for each make
car_make_counts <- usedcars %>%
  group_by(Car_Make) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Add a column to highlight car makes with count < 10
car_make_counts <- car_make_counts %>%
  mutate(Highlight = ifelse(Count < 10, "Yes", "No"))

# Display the table with highlighting for counts < 10
car_make_counts %>%
  kbl() %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(which(car_make_counts$Highlight == "Yes"), bold = TRUE, background = "#FFDDC1")
```


Car makes with very low counts (less than 10) provide insufficient data for the model to learn meaningful patterns, Removing the low frequency car from the dataset

```{r}
# Identify car makes with fewer than 10 occurrences
low_count_makes <- car_make_counts %>%
  filter(Count < 10) %>%
  pull(Car_Make)

# Remove low-frequency car makes from the dataset
usedcars <- usedcars %>%
  filter(!Car_Make %in% low_count_makes)
# Verify the updated counts
updated_counts <- usedcars %>%
  group_by(Car_Make) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(updated_counts)
```


```{r}
# Count the number of cars for each owner category
owner_counts <- usedcars %>%
  group_by(Num_Owners) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Display the table
print(owner_counts)
```

```{r}
usedcars <- usedcars %>%
  filter(Num_Owners != "Test Drive Car")

# Verify the updated dataset
owner_counts <- usedcars %>%
  group_by(Num_Owners) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Display the updated table
print(owner_counts)
```




Normalizing the Price and Mileage column and creating a new data frame normalized_data
```{r}
columns_to_normalize <- c(3,4, 5,10,11)  # Replace with your desired column indices
data_to_normalize <- usedcars[, columns_to_normalize, drop = FALSE]
preProcValues <- preProcess(data_to_normalize, method = "range")
normalized_data <- predict(preProcValues, data_to_normalize)
usedcars_normalized <- usedcars
for (i in seq_along(columns_to_normalize)) {
  col_name <- names(usedcars)[columns_to_normalize[i]]
  new_col_name <- paste0(col_name, "_normalized")
  usedcars_normalized[[new_col_name]] <- normalized_data[[i]]
}
usedcars_normalized
view(usedcars_normalized)
```

Making a new data set by removing the car model, Price, and Mileage column from the data-set  and accuracy
```{r}
Model_ds <- usedcars_normalized %>%
  select(-2, -3,-4, -5,-10,-11)

```


One hot Encoding for all the character class 
```{r}

library(caret)
transform <- Model_ds
summary(transform)
dmy <- dummyVars(" ~.", data = transform, fullRank= T )
trsf <- data.frame(predict(dmy, newdata = transform))
summary(trsf)
Model_ds = trsf 
view(Model_ds)
```




Data Partitioning - Splitting the data set into test and training 


```{r}
library(caret)
# Identify one-hot encoded features (assuming they are all binary)
one_hot_features <- names(Model_ds)[sapply(Model_ds, function(x) all(x %in% c(0, 1)))]

# Create a composite stratification variable
Model_ds$strat_var <- apply(Model_ds[, one_hot_features], 1, function(x) paste(x, collapse = ""))
set.seed(123)

index <- createDataPartition(Model_ds$Price_normalized, p=0.7, list=FALSE)

training <- Model_ds[index,]
test <- Model_ds[-index,]

# Verify the dimensions of the splits
cat("Training set dimensions:", dim(training), "\n")
cat("Testing set dimensions:", dim(test), "\n")
dim(training)
summary(training)
summary(test)
```

```{r}
# Remove the stratification variable
training$strat_var <- NULL
test$strat_var <- NULL
```
Checking for distribution for car makes, to check whether the split of training and test are distributed proeprly

```{r}
for (feature in one_hot_features) {
  cat("\nDistribution of", feature, ":\n")
  cat("Training:", round(mean(training[[feature]]), 4), "\n")
  cat("Test:    ", round(mean(test[[feature]]), 4), "\n")
}
```

Error sum of squares of the default model on the training and test set

```{r}
default.pred <- mean(training$Price_normalized)
default.pred
```

```{r}
default.train.rss <- sum((training$Price_normalized-default.pred)^2)
default.train.rss
default.test.rss <- sum((test$Price_normalized-default.pred)^2)
default.test.rss
```



Cross- Validation using  5 folds
```{r}
report <- data.frame(Model=character(), R2.Train=numeric(), R2.Test=numeric())
```

```{r}
TControl <- trainControl(method="repeatedcv", number=10,repeats = 2)
```

#*****************
#Linear Regression
#*****************

```{r}
set.seed(123)
ols <- train(Price_normalized ~ ., data=training, method="lm", trControl=TControl, metric="Rsquared")
ols
summary(ols)

```


Finding the accuracy of Training and Test 

```{r}
prediction.train <- predict(ols, newdata = training)
train.rss <- sum((training$Price_normalized-prediction.train)^2)
ols.r2 <- 1.0-train.rss/default.train.rss
ols.r2
prediction.test <- predict(ols, newdata = test)
test.rss <- sum((test$Price_normalized-prediction.test)^2)
ols.pseudoR2 <- 1.0-test.rss/default.test.rss
ols.pseudoR2

```

Final report of test and training accuracy of ols
```{r}
report <- rbind(report, data.frame(Model="Linear Regression", Training=ols.r2, Test=ols.pseudoR2))
report
```

#*****************
#ridge regression
#*****************

```{r}
ridgeGrid <- expand.grid(
  alpha = 0, 
  lambda = seq(from = 0, to = 0.01, by = 0.001) # Lambda values from 0.01 to 0.2
)

# Train the Ridge Regression model using caret
set.seed(123)
ridge_model <- train(
  Price_normalized ~ ., 
  data = training, 
  method = "glmnet", 
  tuneGrid = ridgeGrid, 
  trControl = TControl,
  metric = "Rsquared"
)
ridge_model
summary(ridge_model)
plot(ridge_model,xvar="lambda")
```

```{r}
results = ridge_model$results
write.csv(results, "ridge_regression_results.csv", row.names = FALSE)
```

```{r}
prediction.train <- predict(ridge_model, newdata = training)
train.rss <- sum((training$Price_normalized-prediction.train)^2)
ridge.r2 <- 1.0-train.rss/default.train.rss
ridge.r2
prediction.test <- predict(ridge_model, newdata = test)
test.rss <- sum((test$Price_normalized-prediction.test)^2)
ridge.pseudoR2 <- 1.0-test.rss/default.test.rss
ridge.pseudoR2
```



```{r}
ridge_results <- ridge_model$results

# Visualize R-squared across lambda values
ggplot(ridge_results, aes(x = lambda, y = Rsquared)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Model Performance Across Lambda",
       x = "Lambda",
       y = "R-squared") +
  theme_minimal()
```


```{r}
# Extract coefficients at different lambda values
coef_matrix <- as.matrix(coef(ridge_model$finalModel, s = ridge_model$bestTune$lambda))

# Convert to a data frame for visualization
coef_df <- data.frame(
  Feature = rownames(coef_matrix),
  Coefficient = coef_matrix[,1]
)

# Plot coefficient shrinkage
ggplot(coef_df, aes(x = Feature, y = Coefficient)) +
  geom_bar(stat="identity", fill="skyblue") +
  coord_flip() +
  labs(title="Coefficient Shrinkage in Ridge Regression",
       x="Features",
       y="Coefficient Value") +
  theme_minimal()
```
```{r}
print(ridge_model$resample)
```



```{r}
report <- rbind(report, data.frame(Model="Ridge Regression", Training=ridge.r2, Test=ridge.pseudoR2))
report
```


#**********************
# k-Nearest Neighbors
#**********************
Running model using k-Nearest Neighbors
```{r}
knnGrid <- expand.grid(k=seq(from=1, to=15, by=1))
set.seed(123)
knnmodel <- train(Price_normalized ~ ., data=training, method="knn", tuneGrid=knnGrid, trControl=TControl, metric="Rsquared")
knnmodel
summary(knnmodel)
```

```{r}
knnmodel_results <- knnmodel$results

# Visualize R-squared across lambda values
ggplot(knnmodel_results, aes(x = k , y = Rsquared)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Model Performance Across k",
       x = "k-neighbors",
       y = "R-squared") +
  theme_minimal()
```




Finding the accuracy of Training and Test 
```{r}
prediction.train <- predict(knnmodel, newdata = training)
train.rss <- sum((training$Price_normalized-prediction.train)^2)
knn.r2 <- 1.0-train.rss/default.train.rss
knn.r2
prediction.test <- predict(knnmodel, newdata = test)
test.rss <- sum((test$Price_normalized-prediction.test)^2)
knn.pseudoR2 <- 1.0-test.rss/default.test.rss
knn.pseudoR2
```

Final report of test and training accuracy of ols
```{r}
report <- rbind(report, data.frame(Model="k-NN", Training =knn.r2, Test=knn.pseudoR2))
report
```



#***************************
# Support Vector Regression 
#***************************

Linear
```{r}
svlGrid <- expand.grid(C=seq(from=0.01, to=1, by=0.1))
eps <- 0.1
set.seed(123)
svr.linear <- train(Price_normalized ~ ., data=training, method="svmLinear", tuneGrid=svlGrid, trControl=TControl, metric="Rsquared", epsilon = eps)
svr.linear
```

```{r}

# Visualize R-squared across lambda values
svr.linear_results  = svr.linear$results ##
ggplot(svr.linear_results, aes(x = C , y = Rsquared)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Model Performance Across C",
       x = "C",
       y = "R-squared") +
  theme_minimal()
```


```{r}
# Extract results
results <- svr.linear$results
write.csv(results, "svr.linear.csv", row.names = FALSE)
```

Finding the accuracy of Training and Test 
```{r}
prediction.train <- predict(svr.linear, newdata = training)
train.rss <- sum((training$Price_normalized-prediction.train)^2)
svrlin.r2 <- 1.0-train.rss/default.train.rss
svrlin.r2
prediction.test <- predict(svr.linear, newdata = test)
test.rss <- sum((test$Price_normalized -prediction.test)^2)
svrlin.pseudoR2 <- 1.0-test.rss/default.test.rss
svrlin.pseudoR2
```


```{r}
report <- rbind(report, data.frame(Model="Linear SVR", Training =svrlin.r2, Test=svrlin.pseudoR2))
report
```


Radial
```{r}
eps <- 0.1      
svrGrid <- expand.grid(sigma = c(.016, .015, 0.17), C = c(20,30,40,50))
set.seed(123)
svr.rbf <- train(Price_normalized ~ ., data=training, method="svmRadial", tuneGrid=svrGrid, trControl=TControl, metric="Rsquared", epsilon = eps)
svr.rbf
```

```{r}
# Extract results
results <- svr.rbf$results
write.csv(results, "svr.radial.csv", row.names = FALSE)
```



```{r}
prediction.train <- predict(svr.rbf, newdata = training)
train.rss <- sum((training$Price_normalized-prediction.train)^2)
svrrbf.r2 <- 1.0-train.rss/default.train.rss
svrrbf.r2
prediction.test <- predict(svr.rbf, newdata = test)
test.rss <- sum((test$Price_normalized-prediction.test)^2)
svrrbf.pseudoR2 <- 1.0-test.rss/default.test.rss
svrrbf.pseudoR2

```

```{r}
report <- rbind(report, data.frame(Model="Radial SVR",Training =svrrbf.r2, Test=svrrbf.pseudoR2))
report
```



#**********************
# Random Forest Model
#**********************

```{r}

# Initialize a data frame to store results
results <- data.frame(maxnodes = integer(), mtry = integer(), Rsquared = numeric())

besta <- 0
bestn <- 0
bestm <- 0
for (maxnodes in c(90,100)) 
  set.seed(123)

  rfGrid <- expand.grid(mtry = seq(22,30, by = 2))###
  rfmodel <- train(
    Price_normalized ~ ., 
    data = training, 
    method = "rf", 
    tuneGrid = rfGrid,
    trControl = TControl ,
    metric = "Rsquared"
    
  )
    cat("\n\nmaxnodes =", maxnodes, 
      " Rsquared =", max(rfmodel$results$Rsquared), 
      " mtry =", rfmodel$bestTune$mtry)
rfmodel

```



```{r}
# Extract results from rfmodel
rf_results <- rfmodel$results

# Plot R-squared across mtry values for each maxnodes
ggplot(rf_results, aes(x = mtry, y = Rsquared)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Model Performance Across Hyperparameters",
       x = "mtry (Number of Features)",
       y = "R-squared") +
  theme_minimal()
```



Finding the accuracy of Training and Test 

```{r}
prediction.train <- predict(rfmodel, newdata = training)
train.rss <- sum((training$Price_normalized-prediction.train)^2)
rf.r2 <- 1.0-train.rss/default.train.rss
rf.r2
prediction.test <- predict(rfmodel, newdata = test)
test.rss <- sum((test$Price_normalized-prediction.test)^2)
rf.pseudoR2 <- 1.0-test.rss/default.test.rss
rf.pseudoR2

```

Final report of test and training accuracy of ols
```{r}
report <- rbind(report, data.frame(Model="Random Forest", Training =rf.r2, Test=rf.pseudoR2))
report
```


#**********************************
#          Neural Network
#**********************************
```{r}
nnGrid <- expand.grid(
  size = seq(1, 16, by = 3),       
  decay = seq(0.01, 0.1, by = 0.01) 
)

set.seed(123)
nnmodel <- train(Price_normalized ~ ., data = training, method = "nnet", tuneGrid = nnGrid, trControl = TControl, trace = FALSE,metric = "Rsquared",  MaxNWts = 2000)
nnmodel

```


```{r}

plotnet(nnmodel)
nnmodel$results
plot(nnmodel)

```



```{r}

prediction.train <- predict(nnmodel, newdata = training)
train.rss <- sum((training$Price_normalized-prediction.train)^2)
nn.r2 <- 1.0-train.rss/default.train.rss
nn.r2
prediction.test <- predict(nnmodel, newdata = test)
test.rss <- sum((test$Price_normalized-prediction.test)^2)
nn.pseudoR2 <- 1.0-test.rss/default.test.rss
nn.pseudoR2
```

```{r}
report <- rbind(report, data.frame(Model="Neural Network 1", Training=nn.r2, Test=nn.pseudoR2))
report
```



#************
#XG Boost
#************

```{r}
xgbGrid <- expand.grid(

  nrounds = seq(500,700, by = 100),
  max_depth = c(1,2,3),
  eta = c(0.09, 0.05),
  gamma = c(0),
  colsample_bytree = c(0.6, 0.8),
  min_child_weight = c(1, 3),
  subsample = c(0.8)

)

set.seed(123)
xgb_model <- train(
  Price_normalized ~ ., 
  data = training,
  method = "xgbTree",
  tuneGrid = xgbGrid,
  trControl = TControl,
  metric = "Rsquared",
  verbosity = 0
)
xgb_model
```

```{r}

prediction.train <- predict(xgb_model, newdata = training)
train.rss <- sum((training$Price_normalized-prediction.train)^2)
xgb.r2 <- 1.0-train.rss/default.train.rss
xgb.r2
prediction.test <- predict(xgb_model, newdata = test)
test.rss <- sum((test$Price_normalized-prediction.test)^2)
xgb.pseudoR2 <- 1.0-test.rss/default.test.rss
xgb.pseudoR2
```


```{r}
report <- rbind(report, data.frame(Model="XGB", Training=xgb.r2, Test=xgb.pseudoR2))
report
```
#************
#Final Report
#************


```{r}
# show training results
results <- resamples(list(lm =ols,ridge = ridge_model,KNN=knnmodel,       svr_linear=svr.linear,svr_radial = svr.rbf,
                          RFor=rfmodel, NeuNet=nnmodel,  
                          XGB = xgb_model))
summary(results)
dotplot(results)

# print report
report

```




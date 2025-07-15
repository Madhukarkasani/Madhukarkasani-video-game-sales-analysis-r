---
output:
  pdf_document: default
  html_document: default
---
# Madhukar Goud Kasani
# Student ID :4237234
# CSDA 5110 03 Analytic programming with R
# Professor Todd Dill
# Final Project


In this analysis, we dive into the world of video game sales to know underlying trends, patterns of  the gaming industry.The primary objective of this analysis is to gain a comprehensive understanding of the video game sales across different platforms, genres, and regions. By using statistical methodologies and data visualization techniques like ggplot in R, 

```{r}
# Load the libraries
library(tidyverse)    
library(data.table)   
library(dplyr)
library(plotly)      
library(corrplot)   
library(psych)        
library(Hmisc)  
library(ggplot2)
```

# Loading vgsales dataset
```{r}
vgsales <- read.csv("vgsale.csv")
head(vgsales)
```
# 1. Introduction

 a) The domain of the data is video game Industry, it focus on video game sales data , it encompassing  various attributes such as  Ranking of the game , platforms,release years,genres it shows category of the video game like  (Action,Racing , Fighting, shooter, sports),publishers, and sales figures across different regions(Northamerica ,Europe , Japan,others, and global sales). exploring trends over different years,comparing sales performance with different genres, identifying best selling games or top performing platforms in different regions. analyzing the market share of various publishers.



b) 
The video game sales dataset has 11 variables which are Rank, Name, Platform, Year, Genre, Publisher, NA_sales, EU_sales, Jp_Sales, Other sales and Global Sales. the dataset has both categorical and numerical columns.where Name, platform,genre, publisher are character variable. Rank and year have integer type . and NA_sales, EU_Sales , JP_Sales,Other sales and Global Sales are numeric variables.

```{r}
str(vgsales)
```
```{r}
numcol_vgsale <- vgsales[,c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales")]
```

```{r}
# summary statistics for numeric columns
summary(numcol_vgsale)
```
 

# c 
The outcome variable is Global Sales  also know as dependent variable where it summarizes the overall performance of a video game it represent the total global sales of each video game across all regions.

# D
 we know predictor variable is also know as independent variable are used to predict.where in case of  video game sales dataset Year,platform , genre and publisher are considered as predictor variable and fixed factors. where platform represent different gaming platforms like Playstation and more .Year represent the year of game release although it represent a numerical values but treated as fixed factor .Genre represent different gaming genres like sports, role-playing, shooting, racing, action,sports, shooter and more . publisher it identifying the publisher of the game . these all are predefined fixed categories .it cannot be change. 
Covarities variables are NA_Sales,EU_Sales ,JP_Sales ,Other_Sales and these represent sales figures in specific region. Global_Sales , these variable serve as covarities . it explore how salesin individual regions relate to global sales.
```{r}
shooter_sales <- vgsale$Global_Sales[vgsale$Genre == "Shooter"]
misc_sales <- vgsale$Global_Sales[vgsale$Genre == "Misc"]

# Perform two-sample t-test assuming equal variances
t_test_result <- t.test(shooter_sales, misc_sales)

print(t_test_result)

```


# 2. Exploration of the Data 

# Distribution of Year

```{r}
ggplot(vgsales, aes(x = Year)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Release Years", x = "Year", y = "Frequency")

```

# Distribution of Global Sales

```{r}
ggplot(vgsales, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Global Sales", x = "Global Sales", y = "Frequency")

```

# Distribution of Platform

```{r}
ggplot(vgsales, aes(x = Platform)) +
  geom_bar(fill = "coral") +
  labs(title = "Distribution of Gaming Platforms", x = "Platform", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

```

# Distribution of Genre

```{r}
ggplot(vgsales, aes(x = Genre)) +
  geom_bar(fill = "lightpink") +
  labs(title = "Distribution of Game Genres", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

```

# Distribution of Publisher

```{r}
top_publishers <- vgsales %>%
  count(Publisher) %>%
  top_n(20)

ggplot(top_publishers, aes(x = reorder(Publisher, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Top 20 Publishers by Game Count", x = "Publisher", y = "Game Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```


# creating a boxlot for sales by region

```{r}
sales_region <- numcol_vgsale%>%
  gather(key = "Region", value = "Sales") %>%
  ggplot(aes(x = Region, y = Sales)) +
  geom_boxplot() +
  labs(title = "Sales Distribution by Region", y = "Sales", x = "Region")

sales_region
```

# The number of games released as per the category

```{r}
games_released =with(vgsales, table(Genre))
plot(games_released, main = "Genre based distribution", xlab = "Genre", ylab = "No. of games",col = "blue")
```

# lets identify in which year the sales were the maximum for all the three regions

```{r}
sales_data <- vgsales %>%
  select(Year, NA_Sales, EU_Sales, JP_Sales)

sales_data_long <- sales_data %>%
  pivot_longer(cols = c(NA_Sales, EU_Sales, JP_Sales), names_to = "Region", values_to = "Sales")

# Find the year with maximum sales for each region
max_sales_year <- sales_data_long %>%
  group_by(Region) %>%
  summarise(Max_Sales = max(Sales),
            Year = Year[which.max(Sales)])

# Create a bar plot to visualize the year with maximum sales for each region
ggplot(max_sales_year, aes(x = Region, y = Max_Sales, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Year with Maximum Sales for Each Region",
       x = "Region", y = "Maximum Sales") +
  scale_fill_discrete(name = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom")

```
# Grouping by Year and summing sales for each region

```{r}
sales_by_region <- vgsales%>%
  group_by(Year) %>%
  summarise(
    Total_NA_Sales = sum(NA_Sales),
    Total_EU_Sales = sum(EU_Sales),
    Total_JP_Sales = sum(JP_Sales),
    Total_Other_Sales = sum(Other_Sales)
  )

# Reshaping the data into long format for plotting
sales_by_region_long <- sales_by_region %>%
  pivot_longer(cols = starts_with("Total"), names_to = "Region", values_to = "Total_Sales")

# Plotting line graphs for each region's sales by Year
ggplot(sales_by_region_long, aes(x = Year, y = Total_Sales, color = Region)) +
  geom_line() +
  labs(title = "Total Sales by Region over Years", x = "Year", y = "Total Sales") +
  theme_minimal() +
  scale_color_discrete(name = "Region") 
```

b)

```{r}
predictor_variables <- vgsales[, c("Rank", "Year", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")]

# Calculating  correlations between each predictor variable and outcome variable
correlations_with_outcomes <- cor(predictor_variables)

print("Correlations between each predictor variable and outcome variable" )
print(correlations_with_outcomes)


```

```{r}
# Calculate correlations between each pair of predictor variables
correlations_between_predictors <- cor(predictor_variables[, -ncol(predictor_variables)])

# Display correlations between predictor variables
print("Correlations between each pair of predictor variables:")
print(correlations_between_predictors)
```

# finding is there any correlation between numeric variables of the sales

```{r}
pairs(formula = ~ NA_Sales + EU_Sales + JP_Sales+Other_Sales+Global_Sales,cex=1, data= vgsales )
```

```{r}
num_Sales= vgsales[,c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales")]
cor(num_Sales)
```

# Analysis
3.
In these analysis Im trying to predict or explain the global sales based on other variables. The aim is to build a predictive model and explore relationships to explain variation in global sales by using variables like 'NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales', 'Year', 'Genre', 'Platform', and 'Publisher'.

The methods that im using regression techniques linear regression, multiple regression to predict 'Global_Sales' using one or multiple independent variables.it helps in understanding how changes in predictor variables influence the outcome.and the other method is ANOVA  it useful for determining if categorical predictors 'Genre', 'Platform', 'Publisher' significantly affect 'Global_Sales'it Helps identify if different levels of categorical variables have distinct impacts on sales.

The first Model i created using Simple linear Regression using linear model function lm(), where global sales is the dependent variable and NA_Sales is independent variable(predictor)

```{r}
# simple linear regression
slmodel <- lm(Global_Sales ~ NA_Sales, data = vgsales)
summary(slmodel)
```
Based on the above summary of model , it appears to be a strong and positive linear relationship between NA_sales anf Global_sales . the model explains 89.35% of variability in Global sales using NA_Sales.where p value is < 2.2e-16 which is low value it suggest that the  relationship between 'NA_Sales' and 'Global_Sales' is statistically significant.Overall, the low p-values, high R-squared, and significance of the coefficients suggest that 'NA_Sales' is a highly significant predictor for 'Global_Sales' in linear regression model.

```{r}
par(mfrow = c(2,2)) 
plot(slmodel)
```


# Multiple Linear Regression.

Multiple linear regression model to assess the relationship between Global_Sales (outcome) and predictor variables like regional sales figures, year of release, and categorical variables and it Explore the strength and significance of each predictor's impact on Global_Sales.


```{r}
library(caret)
set.seed(123)

# Splitting the data into 70% train and 30% test
train <- createDataPartition(vgsales$Global_Sales, p = 0.7, list = FALSE)
training_data <- vgsales[train, ]
testing_data <- vgsales[-train, ]

# multiple linear regression model
mlr_model <- lm(Global_Sales ~ NA_Sales + EU_Sales + JP_Sales + Other_Sales , data = vgsales)
summary(mlr_model)

```
All predictors (NA_Sales, EU_Sales, JP_Sales, Other_Sales) have coefficients close to 1, indicating a strong positive linear relationship with the Global_Sales. This suggests that for every one-unit increase in each predictor, the Global_Sales tend to increase by approximately 1 unit.The p-values (< 2.2e-16) indicate that all predictors are highly statistically significant in predicting Global_Sales.The R-squared of 1 suggests that the model explains all the variance in the Global_Sales, but it might also indicate overfitting or perfect multicollinearity issues.Overall, the model seems to fit the data  well, 


```{r}
# Plotting NA_Sales vs Global_Sales
plot(training_data$NA_Sales, training_data$Global_Sales,
     xlab = "NA_Sales", ylab = "Global_Sales",
     main = "Scatterplot of NA_Sales vs Global_Sales")
# Adding regression line to the plot
abline(lm(Global_Sales ~ NA_Sales, data = training_data), col = "red")

plot(training_data$EU_Sales, training_data$Global_Sales,
     xlab = "EU_Sales", ylab = "Global_Sales",
     main = "Scatterplot of EU_Sales vs Global_Sales")
abline(lm(Global_Sales ~ EU_Sales, data = training_data), col = "blue")

```

# Performing ANOVA for the multiple linear regression model

```{r}
anova_result <- anova(mlr_model)
# Displaying the ANOVA table
print(anova_result)
```



```{r}
par(mfrow = c(2, 2))
plot(mlr_model )  
```







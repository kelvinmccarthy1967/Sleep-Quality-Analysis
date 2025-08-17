# Load necessary libraries
library(ggplot2)
library(dplyr)
library(randomForest)
library(caret)
library(gridExtra)
library(DALEX)
library(pROC)
library(ROCR)
library(shiny)
library(knitr)

## Sleep Health and Lifestyle Dataset
##1.) Loading the dataset into R.
library(readr)
Sleep_health_and_lifestyle_dataset <- read_csv("Predictive Modelling/Assighmnets/Final Project/data/Sleep_health_and_lifestyle_dataset.csv")
View(Sleep_health_and_lifestyle_dataset)

# Check for missing values in the entire dataset
total_missing <- sum(is.na(Sleep_health_and_lifestyle_dataset))
print(paste("Total missing values:", total_missing))


##2.) Using the str() function to display the structure of the dataset.
str(Sleep_health_and_lifestyle_dataset)
##3)Renaming columns for easier referencing
colnames(Sleep_health_and_lifestyle_dataset) <- c("Person_ID", "Gender", "Age", "Occupation", 
                                                  "Sleep_Duration", "Quality_of_Sleep", 
                                                  "Physical_Activity_Level", "Stress_Level", 
                                                  "BMI_Category", "Blood_Pressure", "Heart_Rate", 
                                                  "Daily_Steps", "Sleep_Disorder")

##3.) Checking to determine the types of variables in different columns of the dataset.
is.numeric(Sleep_health_and_lifestyle_dataset$Person_ID)
is.numeric(Sleep_health_and_lifestyle_dataset$Age)
is.factor(Sleep_health_and_lifestyle_dataset$Gender)
is.factor(Sleep_health_and_lifestyle_dataset$Occupation)
is.numeric(Sleep_health_and_lifestyle_dataset$Sleep_Duration)
is.numeric(Sleep_health_and_lifestyle_dataset$Quality_of_Sleep)
is.factor(Sleep_health_and_lifestyle_dataset$BMI_Category)
is.factor(Sleep_health_and_lifestyle_dataset$Blood_Pressure)
is.numeric(Sleep_health_and_lifestyle_dataset$Heart_Rate)
is.factor(Sleep_health_and_lifestyle_dataset$Blood_Pressure)
is.factor(Sleep_health_and_lifestyle_dataset$Sleep_Disorder)
is.numeric(Sleep_health_and_lifestyle_dataset$Daily_Steps)
is.factor(Sleep_health_and_lifestyle_dataset$Person_ID)
is.numeric(Sleep_health_and_lifestyle_dataset$Physical_Activity_Level)
is.numeric(Sleep_health_and_lifestyle_dataset$Stress_Level)

#4.) Converting certain columns in the dataset into the factor data type.
Sleep_health_and_lifestyle_dataset$Gender<- as.factor(Sleep_health_and_lifestyle_dataset$Gender)
Sleep_health_and_lifestyle_dataset$Occupation<- as.factor(Sleep_health_and_lifestyle_dataset$Occupation)
Sleep_health_and_lifestyle_dataset$BMI_Category<- as.factor(Sleep_health_and_lifestyle_dataset$BMI_Category)
Sleep_health_and_lifestyle_dataset$Sleep_Disorder<- as.factor(Sleep_health_and_lifestyle_dataset$Sleep_Disorder)
Sleep_health_and_lifestyle_dataset$Blood_Pressure<- as.factor(Sleep_health_and_lifestyle_dataset$Blood_Pressure)
Sleep_health_and_lifestyle_dataset$Person_ID<- as.factor(Sleep_health_and_lifestyle_dataset$Person_ID)

##5.)# Summary statistics
summary(Sleep_health_and_lifestyle_dataset)

##6.) Calculating the minimum, maximum, mean, median, standard deviation and quantile values of the Age column
is.numeric(Sleep_health_and_lifestyle_dataset$Age)
min_amt<- min(Sleep_health_and_lifestyle_dataset$Age)
min_amt

max_amt<- max(Sleep_health_and_lifestyle_dataset$Age)
max_amt

meanORaverage<- mean(Sleep_health_and_lifestyle_dataset$Age)
meanORaverage

medianage<- median(Sleep_health_and_lifestyle_dataset$Age)
medianage

sd_age<- sd(Sleep_health_and_lifestyle_dataset$Age)
sd_age

quantile(Sleep_health_and_lifestyle_dataset$Age, probs= c(0.25,0.5,0.75))

##6.) Calculating the minimum, maximum, mean, median, standard deviation and quantile values of the Sleep.Duration column
is.numeric(Sleep_health_and_lifestyle_dataset$Sleep_Duration)

min_Sleep_D<- min(Sleep_health_and_lifestyle_dataset$Sleep_Duration)
min_Sleep_D

max_Sleep_D<- max(Sleep_health_and_lifestyle_dataset$Sleep_Duration)
max_Sleep_D

mean_Sleep_D<- mean(Sleep_health_and_lifestyle_dataset$Sleep_Duration)
mean_Sleep_D

medianSleep.D<- median(Sleep_health_and_lifestyle_dataset$Sleep_Duration)
medianSleep.D

sd_Sleep.D<- sd(Sleep_health_and_lifestyle_dataset$Sleep_Duration)
sd_Sleep.D

quantile(Sleep_health_and_lifestyle_dataset$Sleep_Duration, probs = c(0.25,0.5,0.75))

##7.) Calculating the correlation coefficient between two numeric vectors: Sleep.Duration and Quality.of.Sleep
cor(Sleep_health_and_lifestyle_dataset$Sleep_Duration,Sleep_health_and_lifestyle_dataset$Quality_of_Sleep) 

##8.) Finding the distribution and characteristics of the Gender variable in the dataset.
class(Sleep_health_and_lifestyle_dataset$Gender)
Sleep_health_and_lifestyle_dataset$Gender<- as.factor(Sleep_health_and_lifestyle_dataset$Gender)
class(Sleep_health_and_lifestyle_dataset$Gender)

levels(Sleep_health_and_lifestyle_dataset$Gender)
table(Sleep_health_and_lifestyle_dataset$Gender)

names(sort(-table(Sleep_health_and_lifestyle_dataset$Gender)))[1]

##9.) Finding the distribution and characteristics of BMI.Category in the dataset.
class(Sleep_health_and_lifestyle_dataset$BMI_Category)
Sleep_health_and_lifestyle_dataset$BMI_Category <- as.factor(Sleep_health_and_lifestyle_dataset$BMI_Category)
class(Sleep_health_and_lifestyle_dataset$BMI_Category)

prop.table(table(Sleep_health_and_lifestyle_dataset$BMI_Category))

names(sort(-prop.table(table(Sleep_health_and_lifestyle_dataset$BMI_Category))))[1]

##10.) Using the contingency table to the counts of observations for each combination of levels in the Gender and BMI.Category variables.

xtab.Gender.BMI.Category <- xtabs(~Gender+BMI_Category, data = Sleep_health_and_lifestyle_dataset)
xtab.Gender.BMI.Category

##11.) Computing the relative frequencies of each column in the contingency table
prop.table(xtab.Gender.BMI.Category,margin=1)
prop.table(xtab.Gender.BMI.Category,margin=2)

##12.) Using the summary function to generate an overview of the dataset's structure and characteristics.

Sleep_health_and_lifestyle_dataset$Person_ID<- as.factor(Sleep_health_and_lifestyle_dataset$Person_ID)
summary(Sleep_health_and_lifestyle_dataset)


##13.) Computing a histogram for the Age.
hist(Sleep_health_and_lifestyle_dataset$Age, main = "Histogram of Age",xlab = "Age",ylab = "Frequency",
     xlim = c(0,100),ylim = c(0,120), col = "red",border = F)

##14.) Computing a density plot for Age.

dpAge<-density(Sleep_health_and_lifestyle_dataset$Age)
plot(dpAge,main = "Density Plot of Age",xlab ="Age",
     xlim = c(0,100),col = "red")

##15.) Computing a combined histogram with a kernel density plot for Age

hist(Sleep_health_and_lifestyle_dataset$Age, main = "Histogram and Kernel Density Plot of Age",
     probability = "True", xlab = "Age",
     ylab = "Probability Density",col = "red", border = F,xlim=c(0,100))
lines(density(Sleep_health_and_lifestyle_dataset$Age), lwd=3, col="blue")

##16.) Generating a scatter plot and a regression line to explore the relationship between the Physical.Activity.Level and Age variables in the dataset

plot(Sleep_health_and_lifestyle_dataset$Physical_Activity_Level, Sleep_health_and_lifestyle_dataset$Age,
     main = "Scatter Plot of Age against Physical Activity Level", xlab = "Physical Activity Level",
     ylab = "Age", pch=19, cex=0.5)
abline(lm(Age ~ Physical_Activity_Level, data = Sleep_health_and_lifestyle_dataset),
       lwd=2, col="red")


##17.) Checking and ensuring that Gender and Sleep.Disorder are treated as factor variables, displaying the unique levels for each, and creating a contingency table to examine the relationship between Gender and Sleep.Disorder in the dataset.

is.factor(Sleep_health_and_lifestyle_dataset$Gender)
is.factor(Sleep_health_and_lifestyle_dataset$Sleep_Disorder)
Sleep_health_and_lifestyle_dataset$Gender <- as.factor(Sleep_health_and_lifestyle_dataset$Gender)
Sleep_health_and_lifestyle_dataset$Sleep_Disorder<- as.factor(Sleep_health_and_lifestyle_dataset$Sleep_Disorder)
levels(Sleep_health_and_lifestyle_dataset$Gender)
levels(Sleep_health_and_lifestyle_dataset$Sleep_Disorder)
Sleep_Disorder<- xtabs(~Gender+Sleep_Disorder, data = Sleep_health_and_lifestyle_dataset)
Sleep_Disorder

##18.) Generating a Bar Plot for Gender and Sleep Disorder
barplot(Sleep_Disorder, main = "Bar Plot of Gender and Sleep Disorder",
        col = c("red", "blue"), xlab = "Sleep Disorder", ylab = "Frequency",
        legend=rownames(Sleep.Disorder))

##19.) Generating a Dotchart for Age
dotchart(Sleep_health_and_lifestyle_dataset$Age, main = "Dotchart of Age", col = "blue",
         pt.cex = 0.2, pch = 19, xlab = "Age")

##20.) Generating a Boxplot to visualize the distribution of ages for each gender in the dataset and saving the result plot as a JPEG file
boxplot(Age ~ Gender, data = Sleep_health_and_lifestyle_dataset,
        col=c("red","blue"), xlab = "GENDER", ylab = "AGE",
        main="Boxplot of Age against Gender",
        notch = TRUE)
dev.copy(jpeg,"boxplot_of_Age.jpg")
dev.off()

##21.) Using the summary function to generate an overview of the dataset's structure and characteristics.
summary(Sleep_health_and_lifestyle_dataset)

##22.) Visualize distributions
ggplot(Sleep_health_and_lifestyle_dataset, aes(x = Sleep_Disorder, y = Quality_of_Sleep, fill = Sleep_Disorder)) +
  geom_boxplot() +
  labs(title = "Quality of Sleep by Sleep Disorder", x = "Sleep Disorder", y = "Quality of Sleep") +
  theme_minimal()

##23.) Correlation heatmap
cor_data <- Sleep_health_and_lifestyle_dataset %>%
  select(Age, Sleep_Duration, Physical_Activity_Level, Stress_Level, Quality_of_Sleep) %>%
  cor()
heatmap(cor_data, main = "Correlation Heatmap", col = heat.colors(256))

### 24.) Feature Engineering
# Binning Age
Sleep_health_and_lifestyle_dataset$Age_Group <- cut(Sleep_health_and_lifestyle_dataset$Age, 
                                                    breaks = c(0, 30, 50, 100), 
                                                    labels = c("Young Adult", "Middle Aged", "Senior"))
##25.) Normalize/Standardize predictors
Sleep_health_and_lifestyle_dataset$Standardized_Sleep_Duration <- scale(Sleep_health_and_lifestyle_dataset$Sleep_Duration)
Sleep_health_and_lifestyle_dataset$Standardized_Physical_Activity_Level <- scale(Sleep_health_and_lifestyle_dataset$Physical_Activity_Level)
Sleep_health_and_lifestyle_dataset$Standardized_Stress_Level <- scale(Sleep_health_and_lifestyle_dataset$Stress_Level)

################################################

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)

# Split the dataset into Training (70%) and Testing (30%) sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(Sleep_health_and_lifestyle_dataset$Quality_of_Sleep, p = 0.7, list = FALSE, times = 1)
train_data <- Sleep_health_and_lifestyle_dataset[trainIndex, ]
test_data <- Sleep_health_and_lifestyle_dataset[-trainIndex, ]

# Cross-validation setup
control <- trainControl(method = "cv", number = 10)

### Multiple Linear Regression Model (MLR)
# Train Multiple Linear Regression model
mlr_model <- train(Quality_of_Sleep ~ Age + Sleep_Duration + Physical_Activity_Level + Stress_Level, 
                   data = train_data, 
                   method = "lm", 
                   trControl = control)

# Model evaluation metrics on the test data
mlr_predictions <- predict(mlr_model, test_data)
mlr_results <- postResample(mlr_predictions, test_data$Quality_of_Sleep)

# Print evaluation metrics for MLR
print("MLR Results:")
print(mlr_results)

# Scatter plot of actual vs predicted values for the entire dataset
ggplot(data.frame(Actual = test_data$Quality_of_Sleep, Predicted = mlr_predictions), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) +  # Reference line for ideal prediction
  geom_smooth(method = "lm", col = "green") +  # Regression line for fitted values
  labs(title = "MLR: Actual vs Predicted Quality of Sleep", x = "Actual Quality of Sleep", y = "Predicted Quality of Sleep")

# Residual plot
ggplot(data.frame(Residuals = test_data$Quality_of_Sleep - mlr_predictions, Predicted = mlr_predictions), aes(x = Predicted, y = Residuals)) +
  geom_point(color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted", x = "Predicted Quality of Sleep", y = "Residuals")

### Presenting Results for Each Group (None, Insomnia, Apnea)
# Split data by Sleep Disorder
data_none <- subset(Sleep_health_and_lifestyle_dataset, Sleep_Disorder == "None")
data_insomnia <- subset(Sleep_health_and_lifestyle_dataset, Sleep_Disorder == "Insomnia")
data_apnea <- subset(Sleep_health_and_lifestyle_dataset, Sleep_Disorder == "Sleep Apnea")

# Train and evaluate Multiple Linear Regression model for each sleep disorder group
mlr_model_none <- train(Quality_of_Sleep ~ Age + Sleep_Duration + Physical_Activity_Level + Stress_Level, 
                        data = data_none, 
                        method = "lm", 
                        trControl = control)

mlr_model_insomnia <- train(Quality_of_Sleep ~ Age + Sleep_Duration + Physical_Activity_Level + Stress_Level, 
                            data = data_insomnia, 
                            method = "lm", 
                            trControl = control)

mlr_model_apnea <- train(Quality_of_Sleep ~ Age + Sleep_Duration + Physical_Activity_Level + Stress_Level, 
                         data = data_apnea, 
                         method = "lm", 
                         trControl = control)

# Predict and evaluate models for each group
pred_none <- predict(mlr_model_none, data_none)
results_none <- postResample(pred_none, data_none$Quality_of_Sleep)

pred_insomnia <- predict(mlr_model_insomnia, data_insomnia)
results_insomnia <- postResample(pred_insomnia, data_insomnia$Quality_of_Sleep)

pred_apnea <- predict(mlr_model_apnea, data_apnea)
results_apnea <- postResample(pred_apnea, data_apnea$Quality_of_Sleep)

# Print evaluation metrics for each group
print("MLR Results for None Group:")
print(results_none)

print("MLR Results for Insomnia Group:")
print(results_insomnia)

print("MLR Results for Apnea Group:")
print(results_apnea)

# Scatter plots for each group (None, Insomnia, Apnea)
# None Group
ggplot(data.frame(Actual = data_none$Quality_of_Sleep, Predicted = pred_none), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) +  # Reference line for ideal prediction
  geom_smooth(method = "lm", col = "green") +  # Regression line for fitted values
  labs(title = "MLR: Actual vs Predicted Quality of Sleep (None Group)", x = "Actual Quality of Sleep", y = "Predicted Quality of Sleep")

# Insomnia Group
ggplot(data.frame(Actual = data_insomnia$Quality_of_Sleep, Predicted = pred_insomnia), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) +  # Reference line for ideal prediction
  geom_smooth(method = "lm", col = "green") +  # Regression line for fitted values
  labs(title = "MLR: Actual vs Predicted Quality of Sleep (Insomnia Group)", x = "Actual Quality of Sleep", y = "Predicted Quality of Sleep")

# Apnea Group
ggplot(data.frame(Actual = data_apnea$Quality_of_Sleep, Predicted = pred_apnea), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) +  # Reference line for ideal prediction
  geom_smooth(method = "lm", col = "green") +  # Regression line for fitted values
  labs(title = "MLR: Actual vs Predicted Quality of Sleep (Apnea Group)", x = "Actual Quality of Sleep", y = "Predicted Quality of Sleep")

### Variable Importance Based on Regression Coefficients
# Extract coefficients from the linear model for visualization
coefficients <- coef(mlr_model$finalModel)
coef_df <- data.frame(Variable = names(coefficients), Coefficient = coefficients)

# Bar plot of coefficients to understand variable importance
ggplot(coef_df, aes(x = reorder(Variable, -abs(Coefficient)), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Variable Importance Based on Regression Coefficients", x = "Variable", y = "Coefficient")
###################################


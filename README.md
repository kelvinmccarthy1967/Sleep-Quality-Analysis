# 😴 Sleep Quality Analysis Based on Lifestyle and Health Factors  

This project investigates how **lifestyle, health, and sleep disorders** impact sleep quality.  
Using **Random Forests** and **Multiple Linear Regression**, we identify the most important predictors of sleep quality and evaluate their predictive power.  

---

## 🎯 Objectives  

- Explore the impact of **sleep duration, stress, activity, and disorders** (Insomnia, Sleep Apnea) on sleep quality.  
- Build predictive models (MLR & Random Forest) to assess sleep quality.  
- Determine the **most important predictors** for different disorder groups.  

---

## 📊 Data & Variables  

| Variable                  | Type       | Transformation          | Description |
|----------------------------|-----------|--------------------------|-------------|
| Quality_of_Sleep          | Response  | Factor transformation   | Sleep quality (ordinal 4–9) |
| Age                       | Predictor | Standardized            | Individual’s age (years) |
| Sleep_Duration            | Predictor | Standardized            | Total hours slept |
| Physical_Activity_Level   | Predictor | Standardized            | Minutes of daily activity |
| Stress_Level              | Predictor | Standardized            | Stress (scale 1–10) |
| Sleep_Disorder            | Predictor | Categorical (None, Insomnia, Sleep Apnea) | Type of disorder |

---

## ⚙️ Methods  

- **Preprocessing**: Standardization, factor transformations, and categorical encoding  
- **Models**:
  - Random Forest (ntree = 500)  
  - Multiple Linear Regression (MLR)  
- **Validation**: Train-test split + 10-fold cross-validation  

---

## 📈 Results  

- **Random Forest** performed best:  
  - RMSE = 0.285  
  - R² = 0.942  
  - MAE = 0.102  
  - AUC (ROC Curve) = 0.658  
- **Most important predictors**:  
  - **Insomnia:** Stress Level, Sleep Duration  
  - **Sleep Apnea:** Sleep Duration  
  - **No Disorder:** Sleep Duration, Stress Level  

---

## 📉 Visualizations  

- Boxplots of sleep quality across disorders  
- Variable importance plots per disorder group  
- Correlation heatmap of lifestyle & health variables  
- Regression plots (actual vs predicted)  
- Confusion matrix of classification results  

---

## 🚀 Usage  

Clone the repo and open the R project:  

```r
# Install dependencies
install.packages(c("tidyverse", "caret", "randomForest", "ggplot2", "corrplot"))

# Run analysis
rmarkdown::render("Sleep_Quality_Analysis.Rmd")


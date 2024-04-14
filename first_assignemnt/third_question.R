##################### PART 3

file_path <- "./data/laptop_data_cleaned.csv"
col_classes <- c("factor",    # Company
                 "factor",    # TypeName
                 "factor",   # Ram
                 "numeric",   # Weight
                 "numeric",   # Price
                 "factor",   # TouchScreen
                 "factor",   # Ips
                 "numeric",   # Ppi
                 "factor",    # Cpu_brand
                 "factor",   # HDD
                 "factor",   # SSD
                 "factor",    # Gpu_brand
                 "factor")    # Os

#data <- read.csv(file_path, colClasses = col_classes)
data <- read.csv("Downloads/laptop_data_cleaned.csv", colClasses = col_classes)


# a) maybe the best model its the logarithm of all the numerical variables
# b) partial F-test for making the decision for sure

# a) Consider the numerical variables in the data set and find the best simple linear regression
# model to predict the prices (Test the assumptions and use transformations if it is required.)
# Explain why the model you find is the best simple linear regression model and interpret the
# coefficients of the model (25p)

# The assumptions we need to take into account for a simple linear regression model are:
# Linearity: The relationship between the independent variable(s) and the dependent variable should be linear.
# Homoscedasticity: The variance of the residuals should be constant across all levels of the independent variable(s).
# Normality of Residuals: The residuals (the differences between observed and predicted values) should be normally distributed.

# In order to check which model is better we can check the Adjusted R-squared using summary(). The R-squared value is the amount 
# of variance explained by your model. It is a measure of how well your model fits your data. As a matter of fact, the higher it is, the better is your model.
# Moreover adjusted R-squared will decrease if additional variables do not contribute to the model’s explanatory power.
# We have three numerical variables (Price, Weight and Ppi) so there are few models we can test: 

# Only Weight
ow_model <- lm(Price ~ Weight, data = data)
plot(ow_model)
summary(ow_model)
# Adjusted R-squared: 0.02215

# Assumptions check 

# Linearity: as p-value of weight in the Linear Model is significant (***), we can state that there is a linear correlation between Price and Weight.
# Homoscedasticity:
bptest(ow_model)
# As the p-val<0.05 we have enough evidence for rejecting the null hypothesis, so we can state that the Homoscedasticity assumption is not fulfilled.

plot(ow_model, 2)
shapiro.test(ow_model$residuals)


# Only Ppi
op_model <- lm(Price ~ Ppi, data = data)
summary(op_model)
# Adjusted R-squared: 0.2305

# Assumptions check 

# Linearity: as p-value of weight in the Linear Model is significant (***), we can state that there is a linear correlation between Price and Ppi.
# Homoscedasticity:
bptest(op_model)
# As the p-val>0.05 we have enough evidence for accepting the null hypothesis, so we can state that the Homoscedasticity assumption is fulfilled.

# In this case we can see that Ppi is a better predictor than Weight for Price.


# b) Fit a multivariate linear regression model with two (numerical) independent variables. Choose
# the most significant regression model with two predictors. (Transform the variables if it is
# needed and test all the assumptions.) Then compare this model to the simple linear regression
# model that you fit in (a). Which one is a better model? Why? (25p)

# Weight + Ppi
lm_model <- lm(Price ~ Weight + Ppi, data = data)
summary(lm_model)
# Adjusted R-squared: 0.3336

# Assumptions check 

# Linearity: as p-value of Weight, Ppi and the interaction in the Linear Model is significant (***), we can state that there is a linear correlation between Price and Weight.
# Homoscedasticity:
bptest(lm_model)
# As the p-val<0.05 we have enough evidence for rejecting the null hypothesis, so we can state that the Homoscedasticity assumption is not fulfilled.

# Log(weight + Ppi)
log_var <- log(data$Weight, data$Ppi)
log_model <- lm(Price ~ log_var, data = data)
summary(log_model)
# Adjusted R-squared: -0.0007012

# Assumptions check 

# Linearity: as p-value of the logged variables in the Linear Model is not significant, we can state that there is not a linear correlation between Price and logged variables.


# With this values we could state that the best model for predicting Price is Weight+Ppi, but we can do a further checking with a partial F-test.



# c) Now add a factor to the regression model you have chosen in section (b). (You can write a loop
# to add factors one by one to the previous model and decide based on the results.) Interpret
# the coefficients and overall summary of the model. Test the model in section (b) with the
# model that has an additional factor. Which one would you choose? Why? (35p)
# d) Test the validity of the final model. (15p)


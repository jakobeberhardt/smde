##################### PART 3

library("lmtest")
library("car")

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
                 "numeric",   # SSD
                 "factor",    # Gpu_brand
                 "factor")    # Os

#data <- read.csv(file_path, colClasses = col_classes)
data <- read.csv(file_path, colClasses = col_classes)

# a) Consider the numerical variables in the data set and find the best simple linear regression
# model to predict the prices (Test the assumptions and use transformations if it is required.)
# Explain why the model you find is the best simple linear regression model and interpret the
# coefficients of the model (25p)

# The assumptions we need to take into account for a simple linear regression
# model are:
# No multicolinearity: Not correlation between independent variables.
# Homoscedasticity: The variance of the residuals should be constant across all
# levels of the independent variable(s). Normality of Residuals: The residuals
# (the differences between observed and predicted values) should be normally
# distributed.
# Normality of residuals: The residual (the difference between observed and
# predicted values) should be normally distributed.
# Independence of erros: The residual (the difference between observed and
# predicted values) should be independent.

# In order to check which model is better we can check the Adjusted R-squared
# using summary(). The R-squared value is the amount of variance explained by
# your model. It is a measure of how well your model fits your data. As a matter
# of fact, the higher it is, the better is your model.
# Moreover adjusted R-squared will decrease if additional variables do not
# contribute to the model’s explanatory power.
# We have four numerical variables (Price, Weight, SSD and Ppi) so there are
# few models we can test:

### Only Weight
ow_model <- lm(Price ~ Weight, data = data)
plot(ow_model)
summary(ow_model)
# Adjusted R-squared: 0.02215

# Assumptions check (we do not have to check multicolinearity as we are only
# using one independent variable)

## Homoscedasticity:
bptest(ow_model)
# As the p-val<0.05 we have enough evidence for rejecting the null hypothesis,
# so we can state that the Homoscedasticity assumption is not fulfilled.

## Normality of the residuals
plot(ow_model, 2)
shapiro.test(ow_model$residuals)
# As the pval(3.73e-09)<0.05 we have enough evidence to reject the null
# hypothesis, the assumption of normality of the residuals is not fulfilled.

### Only Ppi
op_model <- lm(Price ~ Ppi, data = data)
summary(op_model)
# Adjusted R-squared: 0.2305

## Assumptions check

## Homoscedasticity:
bptest(op_model)
# As the p-val>0.05 we have enough evidence for accepting the null hypothesis,
# so we can state that the Homoscedasticity assumption is fulfilled.

## Normality of the residuals
plot(op_model, 2)
shapiro.test(op_model$residuals)
# In this case the p-val(0.223)>0.05 so we have enough evidence for accepting
# the null hypothesis, the assumption of normality in the residuals is
# fulfilled.

### Only SSD
os_model <- lm(Price ~ SSD, data = data)
summary(os_model)
# Adjusted R-squared: 0.4336

## Homoscedasticity:
bptest(os_model)
# As the p-val>0.05 we have enough evidence for accepting the null hypothesis,
# so we can state that the Homoscedasticity assumption is fulfilled.

## Normality of residuals
plot(os_model, 2)
shapiro.test(os_model$residuals)
# With these tests we can be sure that residuals follow a normal distribution
# (p-val>0.05).

# In this case we can see that SSD is a better predictor than Ppi for Price. We
# do not take weight into account as it does not fulfill the assumptions and
# the results are not reliable.

# b) Fit a multivariate linear regression model with two (numerical) independent variables. Choose
# the most significant regression model with two predictors. (Transform the variables if it is
# needed and test all the assumptions.) Then compare this model to the simple linear regression
# model that you fit in (a). Which one is a better model? Why? (25p)

### Weight + Ppi
wp_model <- lm(Price ~ Weight + Ppi, data = data)
summary(wp_model)
# Adjusted R-squared: 0.3336

## Assumptions check

## Homoscedasticity:
bptest(wp_model)
# As the p-val<0.05 we have enough evidence for rejecting the null hypothesis,
# so we can state that the Homoscedasticity assumption is not fulfilled.

## Normality of the residuals
plot(wp_model, 2)
shapiro.test(wp_model$residuals)
# The assumption of normality of the residuals is not fulfilled as the
# p-val(0.0014)<0.05, the results of this model are not reliable.


### Weight + SSD
ws_model <- lm(Price ~ Weight + SSD, data = data)
summary(ws_model)
# Adjusted R-squared: 0.409

## Assumptions check

## Homoscedasticity:
bptest(ws_model)
# As the p-val(0.0324)<0.05 we have enough evidence for rejecting the null
# hypothesis, so we can state that the Homoscedasticity assumption is not
# fulfilled.

### Ppi + SSD
ps_model <- lm(Price ~ Ppi + SSD, data = data)
summary(ps_model)
# Adjusted R-squared: 0.4637

## Assumptions check

## Homoscedasticity:
bptest(ps_model)
# As the p-val(0.2116)>0.05 we have enough evidence for accepting the null
# hypothesis, so we can state that the Homoscedasticity assumption is fulfilled.

## Normality of the residuals
plot(ps_model, 2)
shapiro.test(ps_model$residuals)
# The assumption of normality of the residuals is fulfilled as the
# p-val(0.1543)>0.05.

## No Multicolinearity
# In order to test correlation we can check the variance inflation factor
vif(ps_model)
# The correlation between Ppi and SSD is 1.33, we can state that the assumption
# of not multicolinearity between the independent variables (Ppi and SSD) is fulfilled

## Independence of erros
# We can use DurbinWatson test to verify the independence of errors
dwtest(ps_model)
# The test yields a DW value equal to 2.05 means which confirms the independence
# of errors

# With this results we could state that the best model for predicting Price is
# Ppi+SSD, as it has the higher Adjusted R-squared. We can do a further checking
# with a partial F-test.
# For the partial F-tests we will use ANOVA:

anova(ps_model, os_model)

# The ANOVA test suggests that Model 1 provides a significantly better fit to
# the data compared to Model 2.
# This conclusion is based on the significantly lower residual sum of squares
# (RSS) and the associated F-statistic with
# a very low p-value (***), indicating strong evidence against the null
# hypothesis.

# c) Now add a factor to the regression model you have chosen in section (b). (You can write a loop
# to add factors one by one to the previous model and decide based on the results.) Interpret
# the coefficients and overall summary of the model. Test the model in section (b) with the
# model that has an additional factor. Which one would you choose? Why? (35p)

for (fact in c("Company", "TypeName", "Ram", "TouchScreen", "Ips", "Cpu_brand",
               "HDD", "Gpu_brand", "Os")) {
  # Create the formula as a string
  formula_str <- paste("Price ~ Ppi + SSD +", fact)
  print(paste("Price ~ Ppi + SSD +", fact))
  # Convert the string to a formula
  psf_model <- lm(as.formula(formula_str), data = data)
  print(summary(psf_model))
}

## Adjusted R-squared per factor
# Company: 0.5471
# TypeName: 0.632
# Ram: 0.6842
# TouchScreen: 0.4651
# Ips: 0.4745
# Cpu_brand: 0.6946
# HDD: 0.5146
# Gpu_brand: 0.5494
# OS: 0.5056

# With these results we could state that Cpu_brand is the factor that helps more
# to explain the Price

psc_model <- lm(Price ~ Ppi + SSD + Cpu_brand, data = data)

## Assumptions check

## Homoscedasticity:
bptest(psc_model)
# As the p-val(6.755e-14)<0.05 we have enough evidence for rejecting the null
# hypothesis, so we can state that the Homoscedasticity assumption is not
# fulfilled.

# As this model (Cpu_brand) fails the assumption of homoscedasticity we will
# continue the analysis with the second best explanatory factor: Ram

psr_model <- lm(Price ~ Ppi + SSD + Ram, data = data)

## Assumptions check

## Homoscedasticity:
bptest(psr_model)
# As the p-val(2.659e-05)<0.05 we have enough evidence for rejecting the null
# hypothesis, so we can state that the Homoscedasticity assumption is not
# fulfilled.

# As this model (RAM) fails the assumption of homoscedasticity we will continue
# the analysis with the second best explanatory factor: TypeName

pst_model <- lm(Price ~ Ppi + SSD + TypeName, data = data)

# As this model (TypeName) fails the assumption of homoscedasticity we will
# continue the analysis with the second best explanatory factor: Gpu_brand

psg_model <- lm(Price ~ Ppi + SSD + Gpu_brand, data = data)

## Assumptions check

## Homoscedasticity:
bptest(psg_model)
# As the p-val(0.6878)>0.05 we have enough evidence for accepting the null
# hypothesis, so we can state that the Homoscedasticity assumption is fulfilled.

## Normality of the residuals
plot(psg_model, 2)
shapiro.test(psg_model$residuals)
# The assumption of normality of the residuals is fulfilled as the
# p-val(0.07783)>0.05.

## No Multicolinearity
# In order to test correlation we can check the variance inflation factor
vif(psg_model)
# The correlation between the three independent variables is less than 1.2, we
# can state that the assumption of not multicolinearity between the independent
# variables is fulfilled.

## Independence of erros
# We can use DurbinWatson test to verify the independence of errors
dwtest(psg_model)
# The test yields a DW value equal to 1.99 means which confirms the independence
# of errors

# Now that we have chosen the most suitable factor to include in the model, we
# can compare the without-factor and with-factor models with ANOVA.

anova(ps_model, psg_model)

# The ANOVA test suggests that Model 2 provides a significantly better fit to
# the data compared to Model 1,  what means that adding the factor Gpu_brand is
# better for explaining the variation in the response variable 'Price'.
# This conclusion is based on the significantly lower residual sum of squares
# (RSS) and the associated F-statistic with a very low p-value (***),
# indicating strong evidence against the null hypothesis.

# d) Test the validity of the final model. (15p)

# Let's verify graphically how good the model is by using a visual analysis
predictions <- predict(psg_model, data)

# Create a plot
plot(data$Price,
     predictions,
     xlab = "Actual Prices",
     ylab = "Predicted Prices",
     main = "Actual vs Predicted Prices")
abline(0, 1)


# Create a residuals vs fitted values plot
plot(fitted(psg_model),
     residuals(psg_model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, lty = 2)

# To verify how accurate the model is, we take a subset of 10 laptops in our
# dataset and verify how accurate the model is.

set.seed(123)
random_rows <- sample(nrow(data), 10)
subset_data <- data[random_rows, ]

predicted_prices <- predict(psg_model, newdata = subset_data)
differences <- predicted_prices - subset_data$Price

results <- data.frame(
  Predicted = predicted_prices,
  Actual = subset_data$Price,
  Difference = differences,
  Percentage = (differences / subset_data$Price) * 100
)

print(results)


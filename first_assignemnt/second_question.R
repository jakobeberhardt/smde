################################
### SMDE Assignment 1 Part 2 ###
################################

### a) Read the laptop price data set and create a sub dataset including only laptop brands “Dell”,
### “Acer” and “Hp”.  ###

filtered_data <- data[data$Company %in% c("Dell", "Acer", "HP"), ]
head(filtered_data)

### Summarize the variable “company” and check the overall distribution of
### Price and Weight for this subset. (10p) ###

summary(filtered_data$Company)

# In order to visually check the distributions we can plot histograms of the data:

hist(filtered_data$Price)
hist(filtered_data$Weight)
# Visually it seems that Price follows a normal distribution but Weight does not, we can check it with a Shapiro Wilk test.

shapiro.test(filtered_data$Price)
shapiro.test(filtered_data$Weight)
# We can now be sure that Price follows a normal distribution (p-val = 0.28) and Weight does not (p-val = 2.2e-16).

### b) The objective is to analyze the relationship between the brand of the computer and its price
### and its weight. First test the assumptions of the statistical method by using corresponding test
### and plots. Write your conclusions about the assumptions. If there is any violated assumption,
### interpret what should be done? Are both variables suitable for the corresponding analysis? If
### not, explain why? (30p) ###

# In order to analyze the relationship between the brand of the computer and its price and its weight we can use a linear regresion model,
# the assumptions for this model are:
# Linearity: The relationship between the independent variable(s) and the dependent variable should be linear.
# Independence: Observations should be independent of each other.
# Homoscedasticity: The variance of the residuals should be constant across all levels of the independent variable(s).
# Normality of Residuals: The residuals (the differences between observed and predicted values) should be normally distributed.

# Assumptions check:

# Linearity
plot(filtered_data$Company, filtered_data$Price)

# Independence (assumed)

# Homoscedasticity

# Calculate the residuals
residuals <- residuals(model)
# Perform ANOVA using the residuals
anova_result <- anova(lm(residuals ~ 1, data = data))  # Compare the residuals to a model with only an intercept term
# Print ANOVA table
print(anova_result)# Plot residuals against fitted values

plot(model$fitted.values, abs(resid(model)), ylab = "Absolute Residuals", xlab = "Fitted Values", main = "Scale-Location Plot")


# Normality of Residuals
shapiro.test(model)

# Linear model
model <- lm(filtered_data$Price ~ filtered_data$Company)

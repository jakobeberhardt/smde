################################
### SMDE Assignment 1 Part 2 ###
################################
library("lmtest")

### a) Read the laptop price data set and create a sub dataset including only
###    laptop brands “Dell”, “Acer” and “Hp”. Summarize the variable “company”
###    and check the overall distribution of Price and Weight for this subset.

file_path <- "./data/laptop_data_cleaned.csv"
col_classes <- c("factor",    # Company
                 "factor",    # TypeName
                 "factor",    # Ram
                 "numeric",   # Weight
                 "numeric",   # Price
                 "factor",    # TouchScreen
                 "factor",    # Ips
                 "numeric",   # Ppi
                 "factor",    # Cpu_brand
                 "factor",    # HDD
                 "factor",    # SSD
                 "factor",    # Gpu_brand
                 "factor")    # Os

data <- read.csv(file_path, colClasses = col_classes)
filtered_data <- droplevels(data[data$Company %in% c("Dell", "Acer", "HP"), ])
head(filtered_data)

summary(filtered_data$Company)

# To verify if Price and Weight follow a normal distribution we use for both
# of them both a visual (histogram) and a statistical (shapiro) test.

hist(filtered_data$Price,
     breaks = seq(min(filtered_data$Price),
                  max(filtered_data$Price),
                  length.out = 100),
     xlab = "Price",
     main = "Distribution of Prices")
shapiro.test(filtered_data$Price)

hist(filtered_data$Weight,
     breaks = seq(min(filtered_data$Weight),
                  max(filtered_data$Weight),
                  length.out = 100),
     xlab = "Weight",
     main = "Distribution of Weights")
shapiro.test(filtered_data$Weight)

# From the tests we have executed we can state that the Price follows a normal
# distribution (p-val = 0.28) and Weight does not (p-val = 2.2e-16).

### b) The objective is to analyze the relationship between the brand of the
###    computer and its price and its weight. First test the assumptions of the
###    statistical method by using corresponding test and plots. Write your
###    conclusions about the assumptions. If there is any violated assumption,
###    interpret what should be done? Are both variables suitable for the
###    corresponding analysis? If not, explain why?

# In order to analyze the relationship between the brand of the computer and
# its price and its weight we can use a linear regresion model, the assumptions
# for this model are:
# 1 - Linearity
#     The relationship between the independent variable(s) and the dependent
#     variable should be linear.
# 2 - Homoscedasticity
#     The variance of the residuals should be constant across all levels of the
#     independent variable(s).
# 3 - Independence of the residuals.
# 4 - Normality of Residuals
#     The residuals (the differences between observed and predicted values) should
#     be normally distributed.

# Linear model
model <- lm(filtered_data$Price ~ filtered_data$Company)

# Assumptions check:

# Linearity
plot(filtered_data$Company, filtered_data$Price)

# Homoscedasticity
bptest(model)

# We fail to reject the null as the p-val > 0.05, thus we can state that the variance of the error term is constant.

# Calculate the residuals
residuals <- residuals(model)
# Perform ANOVA using the residuals
anova_result <- anova(lm(residuals ~ 1, data = data))  # Compare the residuals to a model with only an intercept term
# Print ANOVA table
print(anova_result)# Plot residuals against fitted values

plot(model$fitted.values, abs(resid(model)), ylab = "Absolute Residuals", xlab = "Fitted Values", main = "Scale-Location Plot")


# Normality of Residuals
# Visually we can check it with a QQ-plot, if the residuals follow the diagonal the distribution is normal
plot(model, which = 2)
# We can also test it with a shapiro test as before
shapiro.test(model)

# Both tests conclude that the residuals follow a normal distribution

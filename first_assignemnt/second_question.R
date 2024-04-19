################################
### SMDE Assignment 1 Part 2 ###
################################
library("lmtest")
library("car")

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
                 "numeric",   # SSD
                 "factor",    # Gpu_brand
                 "factor")    # Os

data <- read.csv(file_path, colClasses = col_classes)
filtered_data <- droplevels(data[data$Company %in% c("Dell", "Acer", "HP"), ])
head(filtered_data)
summary(filtered_data$Company)

# To analyze the distributions of the weights and prices in this subset, we
# visualize the data using a histogram. In a second step, we apply a Shapiro
# test to validate that the distribution of both variables is in fact normal.
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

# From the executed tests we can state that the prices follow a normal
# distribution (p-val = 0.28), yet, the weights are right-skewed and therefore fail
# the Shapiro test (p-val = 2.2e-16).

### b) The objective is to analyze the relationship between the brand of the computer and its price
### and its weight. First test the assumptions of the statistical method by using corresponding test
### and plots. Write your conclusions about the assumptions. If there is any violated assumption,
### interpret what should be done? Are both variables suitable for the corresponding analysis? If
### not, explain why? (30p) ###

# We use an ANOVA test in order to analyze the relationships between brand and
# its price and weight.
# To this end, we have to check the respective assumptions of ANOVA:
# 1 - The distribution of the population must be normal.
# 2 - Homoscedasticity: The variance of the residuals should be constant across
#     all levels of the independent variables.
# 3 - Observations are independent.
#     As stated in the disclaimer, in the general case it is not possible to
#     verify this, but, as this is a regression analysis, the Durbin-Watson test
#     can be used on the residuals to detect the presence of autocorrelation.

# Normality has to be verified on the input data while assumption
# homosedasticity
# and independence of observations have to be verified on the residuals (see c).

# We have seen that weight variable is not normally distributed.
# To be able to apply ANOVA, we will try to remove outliers and do a
# log-transformation.
# We remove possible by using the inter-quartile range (IQR) method along with
# the 1.5 IQR rule:
# Calculate first and third quartiles
Q1 <- quantile(filtered_data$Weight, 0.25)
Q3 <- quantile(filtered_data$Weight, 0.75)

# Calculate inter-quartile range (IQR)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- which(filtered_data$Weight < lower_bound | filtered_data$Weight > upper_bound)

# Remove outliers
cleaned_data <- filtered_data[-outliers,]

# Normality test for the clean data
shapiro.test(cleaned_data$Weight)

# We can see that removing the outliers is not sufficient to obtain a normal
# distribution.
# Therefore, we continue by applying a logarithmic transformation:
log_clean_data <- log10(cleaned_data$Weight)

# Normality test for the clean and log-transformed data
shapiro.test(log_clean_data)

# As the distribution of weight is still not normal, we cannot proceed with the
# analysis in the ANOVA test.

### c) After fulfilling the assumptions, apply the related statistical method and interpret your
### findings. Does the brand of the computer have significant effect on its price and its weight?
### (just consider the variable(s) that fulfill the assumptions tested in section (b)) (35p)

# Now we will proceed with performing the ANOVA test.
model <- aov(filtered_data$Price ~ filtered_data$Company)
summary(model)

# We can apply a TukeyHSD post-hoc test to check which and how groups differ
# from each other
TukeyHSD(model)

# Interpretation of the Tukey test:
# Dell has a significantly higher price compared to Acer,
# with a mean difference of approximately 0.623 (95% CI: [0.476, 0.769]) and an
# adjusted p-value of 0.000.
# HP also has a significantly higher price compared to Acer, with a mean
# difference of approximately 0.503 (95% CI: [0.356, 0.651]) and an adjusted
# p-value of 0.000.
# However, there is no significant difference in price between HP and Dell,
# as the mean difference is approximately -0.119 (95% CI: [-0.227, -0.012])
# with an adjusted p-value of 0.025.

# We can also do post-hoc tests that adjust the p-value in order to reduce type
# I errors, Bonferroni is more conservative than fdr.
pairwise.t.test(filtered_data$Price,
                filtered_data$Company,
                p.adjust.method = "bonferroni")
pairwise.t.test(filtered_data$Price,
                filtered_data$Company,
                p.adjust.method = "fdr")

# We can interpret the results of the tests as following:

# Acer vs. Dell: The p-value for comparing prices between Acer and Dell is
# extremely small (< 2e-16), which indicates a highly significant difference
# in prices between these two companies.

# Acer vs. HP: The p-value for comparing prices between Acer and HP is also
# very small, indicating a highly significant difference in prices between
# these two companies.

# Dell vs. HP: The p-value for comparing prices between Dell and HP is 0.027,
# which is smaller than alpha (0.05), indicating a statistically significant
# difference in prices between these two companies, but not as extreme as the
# differences observed between Acer and each of Dell and HP.

# Lastly, we will verify the remaining assumptions which are homoscedasticity
# and independence of observation.

# In order to check homoscedasticity we can use a qqplot:
plot(model, 2)
# According to this plot we could state that the residuals distribution is
# normal, however we can further check this with a Levene's test.

leveneTest(model)
# The p-value obtained is 0.067, as it is greater than alpha we fail to reject
# the null hypothesis of homoscedasticity, so the assumption is fulfilled.

# The independence of observations is tested using the Durbin-Watson test.
dwtest(model)

# The test yields a p-value of 0.1393, hence, we reject the null hypothesis
# which means that the observations are independent.

# All the assumptions for ANOVA are hence verified for the price.

### d) Analyze the effect of brand and touch screen characteristics together on the price. Analyze
### whether the interaction of two term is significant. Interpret your findings. (Do not forget to
### confirm the assumptions!) (25 p)

# As demonstrated previously (section b) price is normal. Hence, we proceed to
# use anova to verify the interaction of the two terms.

model_interaction <- aov(filtered_data$Price ~
                           filtered_data$Cpu_brand * filtered_data$TouchScreen)

plot(model_interaction, 2)
leveneTest(model_interaction)

# On the qqplot we can see that the values are not following a normal
# distribution. Furthermore if we check the Levene's test on the interaction
# model we see that we have enough evidence to reject the null hypothesis of
# homoscedasticity, so the assumption is not fulfilled

# As we fail to verify the assumptions we can not consider the ANOVA test for
# further conclusions.

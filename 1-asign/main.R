################################
### SMDE Assignment 1 Part 1 ###
################################

### a) Import data set to R assigning the type of each variable correctly. (5p) ###
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

data <- read.csv(file_path, colClasses = col_classes)
head(data)

### b) Create a dataset including only types of laptops: Ultrabook, Notebook and 2 in 1 Convertible. (5p) ###
filtered_data <- data[data$TypeName %in% c("Ultrabook", "Notebook", "2 in 1 Convertible"), ]
head(filtered_data)

### c) Summarize the variables Weight, Price and the categorical variables in the new created data set. (10p) ###
summary(filtered_data$Weight)
summary(filtered_data$Price)

table(filtered_data$Company)
table(filtered_data$TypeName)
table(filtered_data$Cpu_brand)
table(filtered_data$Gpu_brand)
table(filtered_data$Os)

### d) Cross classify the variables type of computer and the touch screen indicator in a table. ###
### Compute and interpret the conditional probability tables. (15p)                             ###
# 1.)
contingency_table <- table(filtered_data$TypeName, filtered_data$TouchScreen)
contingency_table

# P(TouchScreen | TypeName)
conditional_prob_TypeName_to_TouchScreen <- prop.table(contingency_table, 1)  # Margin 1 is for rows (TypeName)

# P(TypeName | TouchScreen)
conditional_prob_TouchScreen_to_TypeName <- prop.table(contingency_table, 2)  # Margin 2 is for columns (TouchScreen)

conditional_prob_TypeName_to_TouchScreen
conditional_prob_TouchScreen_to_TypeName

# 2.) It is really likely that if you buy a "2 in 1 Convertible" it will have a TouchScreen enabled,
# whereas Ultrabook and Notebook follow the oposite trend, specially in Notebook.

### e) Is there an association between the type of computer and the touch screen characteristic of a ###
### computer. Analyze it by using proper statistical method. (10 p)                                  ###

# In order to test this we could use a chi square test, the assumption we take for this test is:
# Assumption 1: Both variables are categorical.
class(filtered_data$TypeName)
class(filtered_data$TouchScreen)
# Yes they are both categorical

chisq.test(filtered_data$TypeName, filtered_data$TouchScreen)
# Yes, we observe a p-value lower than alpha (0.05) so we have enough evidence to reject the null hypothesis, 
# so we can state that there is an association.

### f) Check the distribution of Price first for all observations then for subgroups of type of laptop in ###
### the data set created in section (b). Does it follow a normal distribution? (15p)

plot(data$Price)
plot(filtered_data$Price)
# We could state by the plots that the data does not follow a normal distribution. But to be sure we can preform a 
# Shapiro wilk normality test 

shapiro.test(data$Price)
shapiro.test(filtered_data$Price)

# In both datasets we have enough evidence to reject the null hypothesis, so we can state that the distribution is not normal.

### g) Create a data frame just by including Ultrabooks and Notebooks. (5p) ###
ultra_note_df <- subset(data, TypeName %in% c("Ultrabook", "Notebook"))

### h) Make a boxplot to show the distribution of Price across two categories of Type: Ultrabook vs.
### Notebook. Interpret it. (10p) ###
boxplot(Price ~ TypeName, ultra_note_df)
# We can tell by the boxplot shown that Ultrabook is significantly more expensive than Notebook

### i) Compare the average price of Ultrabooks and Notebooks by using the appropriate method. Do
### not forget to test the assumptions. (25p)  ###
# When comparing the averages of two variables with non-normally distributed data, you can use non-parametric tests which 
# do not rely on the assumption of normality. One such test is the Wilcoxon test, with assumptions:

# 1.dunno about assumptions

only_ultra <- subset(data, TypeName == "Ultrabook")
only_note <- subset(data, TypeName == "Notebook")
wilcox.test(only_ultra$Price, only_note$Price)

# With this test we have enough evidence to reject the null hypothesis as the p-value (2.2e-16) is lower than alpha. 
# We can state that there is a significant difference between both means.

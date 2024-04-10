################################
### SMDE Assignment 1 Part 1 ###
################################



### a) Import data set to R assigning the type of each variable correctly.
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
head(data)

### b) Create a dataset including only types of laptops:
###    - Ultrabook
###    - Notebook
###    - 2 in 1 Convertible

filtered_data <- data[data$TypeName %in% c("Ultrabook",
                                           "Notebook",
                                           "2 in 1 Convertible"), ]
head(filtered_data)

### c) Summarize the variables Weight, Price and the categorical variables
###    in the new created data set.

summary(filtered_data$Weight)
summary(filtered_data$Price)

table(filtered_data$Company)
table(filtered_data$TypeName)
table(filtered_data$Ram)
table(filtered_data$TouchScreen)
table(filtered_data$Ips)
table(filtered_data$Cpu_brand)
table(filtered_data$HDD)
table(filtered_data$SSD)
table(filtered_data$Gpu_brand)
table(filtered_data$Os)

### d) Cross classify the variables type of computer and the touch screen
###    indicator in a table.
###    Compute and interpret the conditional probability tables.
contingency_table <- table(filtered_data$TypeName, filtered_data$TouchScreen)
contingency_table

# P(TouchScreen | TypeName)
# Margin 1 is for rows (TypeName)
conditional_prob_TypeName_to_TouchScreen <- prop.table(contingency_table, 1)

# P(TypeName | TouchScreen)
# Margin 2 is for columns (TouchScreen)
conditional_prob_TouchScreen_to_TypeName <- prop.table(contingency_table, 2)

conditional_prob_TypeName_to_TouchScreen
conditional_prob_TouchScreen_to_TypeName

# Obeserving the results obtained we can say that
# it is really likely that if you buy a "2 in 1 Convertible" it will have a
# TouchScreen available, whereas Ultrabooks and Notebooks follow the opposite
# trend, specially in Notebooks.

### e) Is there an association between the type of computer and the touch
###    screen characteristic of a computer.
###    Analyze it by using proper statistical method.

# To test an asssociation between the type of computer and the presence of a
# touch screen we can use a chi square test.
# The assumption we take for this test is that both variables are categorical.
class(filtered_data$TypeName)
class(filtered_data$TouchScreen)
# Yes they are both categorical

chisq.test(filtered_data$TypeName, filtered_data$TouchScreen)
# We observe a p-value (2.2e-16) lower than alpha (0.05) so we have enough
# evidence to reject the null hypothesis, so we can state that there is an
# association.

### f) Check the distribution of Price first for all observations then for
###    subgroups of type of laptop in the data set created in section (b).
###    Does it follow a normal distribution? (15p)

# All data
hist(data$Price,
     breaks = seq(min(data$Price), max(data$Price), length.out = 100),
     xlab = "Price",
     main = "Distribution of Prices")
shapiro.test(data$Price)

# Regarding all the observations, the hisograms does not present a bell shape
# and Shapiro-Wilk confirms that it's not normal.

filtered_ultra <- subset(data, TypeName == "Ultrabook")
filtered_notebook <- subset(data, TypeName == "Notebook")
filtered_2in1 <- subset(data, TypeName == "2 in 1 Convertible")

# Filtered data - Ultrabook
hist(filtered_ultra$Price,
     breaks = seq(min(filtered_ultra$Price),
                  max(filtered_ultra$Price),
                  length.out = 100),
     xlab = "Price",
     main = paste("Distribution of Prices for Ultrabooks in Filtered Dataset"))
shapiro.test(filtered_ultra$Price)

# Filtered data - Notebooks
hist(filtered_notebook$Price,
     breaks = seq(min(filtered_notebook$Price),
                  max(filtered_notebook$Price),
                  length.out = 100),
     xlab = "Price",
     main = paste("Distribution of Prices for Notebooks Filtered Dataset"))
shapiro.test(filtered_notebook$Price)

# The price for both Ultrabooks and Notebooks is normal.
# We checked this both visually, checking that the graph present a bell shape,
# and using the shaprio test in which they presenter a p-value grater than 0.05

# Filtered data - 2 in 1 Convertible
hist(filtered_2in1$Price,
     breaks = seq(min(filtered_2in1$Price),
                  max(filtered_2in1$Price),
                  length.out = 100),
     xlab = "Price",
     main = paste("Distribution of Prices for 2 in 1 Convertible in Filtered Dataset"))
shapiro.test(filtered_2in1$Price)

# In the case of 2 in 1 Convertible the visual analysis already hinted a non
# normal distribution.
# Also the shaprio test confirms this as we rejected the null hypothesis with a
# p-value smaller than 0.05 (p-value = 0.001445)

### g) Create a data frame just by including Ultrabooks and Notebooks.
ultra_note_df <- droplevels(subset(data, TypeName %in% c("Ultrabook",
                                                         "Notebook")))

### h) Make a boxplot to show the distribution of Price across two categories
###    of Type: Ultrabook vs. Notebook. Interpret it.
boxplot(Price ~ TypeName, data = ultra_note_df,
        xlab = "Type of Computer",
        ylab = "Price",
        main = "Distribution of Prices Across Ultrabooks and Notebooks")
# We can tell by the shown boxplot that Ultrabook is on average more
# expensive than Notebook.
# We can see that in both categories there are outliers. In the case of
# Notebook, the outliers are more expensive while in the case of Ultrabooks the
# only outlier is cheaper.

### i) Compare the average price of Ultrabooks and Notebooks by using the
###    appropriate method. Do not forget to test the assumptions.

# To compare the average price of data that are normally distributed we can
# use the t-test.
# The assumptions for t-tests are:
# 1 - The data is continuous
#     This is the case for the price.
# 2 - Homogenity of variance
#     To test this property we use the f-test (which also assumes normality)

var.test(filtered_ultra$Price,
         filtered_notebook$Price)

#     As the p-value is 3.784e-13 we can reject the null hypotesis and state
#     the variances are different. As we can also see this from the boxplot in
#     section (1.h). Therefore, we need to apply the Welch t-test by using the
#     option: var.equal = FALSE
# 3 - Normality
#     We already checked in point (1.f) that the data prices data for Notebooks
#     and Ultrabooks are normally distributed.

t.test(filtered_ultra$Price,
       filtered_notebook$Price,
       var.equal = FALSE)

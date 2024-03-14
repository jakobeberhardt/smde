################################
### SMDE Assignment 1 Part 1 ###
################################

### a) Import data set to R assigning the type of each variable correctly. (5p) ###
file_path <- "./data/laptop_data_cleaned.csv"
col_classes <- c("factor",    # Company
                 "factor",    # TypeName
                 "integer",   # Ram
                 "numeric",   # Weight
                 "numeric",   # Price
                 "integer",   # TouchScreen
                 "integer",   # Ips
                 "numeric",   # Ppi
                 "factor",    # Cpu_brand
                 "integer",   # HDD
                 "integer",   # SSD
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

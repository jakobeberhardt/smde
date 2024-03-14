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

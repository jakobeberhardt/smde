################################
### SMDE Assignment 1 Part 2 ###
################################

# a - Read the laptop price data set and create a sub dataset including
# only laptop brands “Dell”, “Acer” and “Hp”. Summarize the variable
# “company” and checked the overall distribution of Price and Weight
# for this subset.

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

filtered_data <- data[data$Company %in% c("Dell", "Acer", "HP"), ]
filtered_data$Company <- droplevels(filtered_data$Company)

summary(filtered_data$Company)

summary(data$Price)
summary(filtered_data$Price)
summary(data$Weight)
summary(filtered_data$Weight)

# From the summary we can see that statistically there is not much
# difference between the whole dataset and the filtered one.

boxplot(list("All Data" = data$Price, "Filtered Data" = filtered_data$Price),
        main = "Price Distribution",
        ylab = "Price",
        col = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))

boxplot(list("All Data" = data$Weight, "Filtered Data" = filtered_data$Weight),
        main = "Weight Distribution",
        ylab = "Weight",
        col = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))

# Both statistically and graphically we can notice that the data distribution
# in the filtered dataset is slighly less skewed..

# Shapiro-Wilk test
shapiro.test(filtered_data$Price)
shapiro.test(filtered_data$Weight)

# From the shapiro test we can conclude that the price distribution on the
# filtered dataset is normal but the weight distributino on the filtered
# dataset is not.
# As the weight dataset presents many values outside of the third quartile,
# that are considered outliers, the test will be repeated after removing these
# values.

inter_quartile_range_weight <- IQR(filtered_data$Weight, na.rm = TRUE)

lower_third_quartile <- quantile(filtered_data$Weight, 0.25, na.rm = TRUE) -
  1.5 * inter_quartile_range_weight
upper_third_quartile <- quantile(filtered_data$Weight, 0.75, na.rm = TRUE) +
  1.5 * inter_quartile_range_weight

filtered_data_no_outliers <-
  filtered_data[filtered_data$Weight >= lower_third_quartile &
                filtered_data$Weight <= upper_third_quartile, ]

summary(data$Weight)
summary(filtered_data_no_outliers$Weight)

boxplot(list("All Data" = data$Weight,
             "Filtered Data" = filtered_data$Weight,
             "Without Outliers" = filtered_data_no_outliers$Weight),
        main = "Weight Distribution",
        ylab = "Weight",
        col = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5)))

shapiro.test(filtered_data_no_outliers$Weight)

# Even after removing the outliers the saphiro test still hints that the
# distribution is not normal.

# b - The objective is to analyze the relationship between the brand of the
# computer and its price and its weight. First test the assumptions of the
# statistical method by using corresponding test and plots. Write your
# conclusions about the assumptions. If there is any violated assumption,
# interpret what should be done? Are both variables suitable for the
# corresponding analysis? If not, explain why?

# c - After fulfilling the assumptions, apply the related statistical
# method and interpret your findings. Does the brand of the computer have
# significant effect on its price and its weight? (just consider the
# variable(s) that fulfill the assumptions tested in section (b))

# d - Analyze the effect of brand and touch screen characteristics together on
# the price. Analyze whether the interaction of two term is significant.
# Interpret your findings. (Do not forget to confirm the assumptions!

# NOTE: Do not forget to do multiple comparisons! Apply post hoc tests to see
# where the differences source from. Apply three different post hoc tests and
# compare their findings.
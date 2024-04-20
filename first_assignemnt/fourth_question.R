############################# PART 4

library(FactoMineR)
library(corrplot)

data(decathlon)

# 1) Define a PCA for the decathlon dataset and discuss why the model you own works well (or not) looking
# the variables chart. We recommend using FactoMineR package to do this analysis. Interpret variables and
# individuals charts to understand the relations between the different variables of interest and individuals
# scores on the dimensions

# Before performing the PCA we will check which Principal Components should we
# use with KMO test. The Kaiser-Meyer-Olkin (KMO) represents the degree to which
# each observed variable is predicted by the other variables in the
# dataset and with this indicates the suitability for factor analysis.

# As the variable "Competition" is not numerical, we can not use it for the PCA
# so we remove it from the dataset. We also omit "Rank" as it is the variable
# are aiming to predict.
normalized_data <- decathlon[, -13]
normalized_data <- normalized_data[, -11]

# Next we normalize and center the date of the remaining colums
normalized_data <- as.data.frame(scale(decathlon))

# If we check the KMO, the data is clearly not yet sutiable for a PCA as it yields 0.1154019.
kmo <- function(x)
{
  x <- subset(x, complete.cases(x))       # Omit missing values
  r <- cor(x)                             # Correlation matrix
  r2 <- r^2                               # Squared correlation coefficients
  i <- solve(r)                           # Inverse matrix of correlation matrix
  d <- diag(i)                            # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0               # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO = KMO, MSA = MSA))
}
kmo(normalized_data)

correlation_matrix <- cor(normalized_data)
corrplot(correlation_matrix, method = "circle")

# Looking at the correlation matrix, we can see the problem. Some discipilness
# have positivly correlated while others are negativly correlate. To adress
# this, we change the directions of negative ones.
normalized_data$`100m` <- max(normalized_data$`100m`) - normalized_data$`100m`
normalized_data$`400m` <- max(normalized_data$`400m`) - normalized_data$`400m`
normalized_data$`110m.hurdle` <- max(normalized_data$`110m.hurdle`) - normalized_data$`110m.hurdle`
normalized_data$`1500m` <- max(normalized_data$`1500m`) - normalized_data$`1500m`

correlation_matrix <- cor(normalized_data)
corrplot(correlation_matrix, method = "circle")

# KMO with normalized data
kmo(normalized_data)

# As the values are still very low we will remove the variable with the low KMO in order to improve the model.
# First we remove pole.vault
decathlon <- decathlon[, -8]
kmo(decathlon)

# As the KMO is still low we remove javeline
decathlon <- decathlon[, -8]
kmo(decathlon)

# As the KMO is still low we remove 1500m
decathlon <- decathlon[, -8]
kmo(decathlon)

# With a KMO value of 0.74 we can state that this is a reasonable model to perform a PCA

# pca <- PCA(normalized_data)

# print(pca)

# we can use KMO to test which PC should we use in a PCA, value should be 80 more or less. Nihan uses a function KMO from a professor (search)

# check pca$eig, look for eigenvalues greater than 1 --- "we can conclude that x PC are enough to explain the variance"

# check plot(pca, choix = "var") and plot(pca, choix = "ind")

# we can check the flag axes = x to check different dimensions comparison

# use dwtest() for checking independence of errors

# check pca$var$cos2 to know in which component are the variables explained most

# chech pca$var$contrib to know how each variable contribute to each dimension

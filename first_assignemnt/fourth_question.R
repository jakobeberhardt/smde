############################# PART 4

install.packages("psych")
library(FactoMineR)
library(corrplot)
library(psych)

data(decathlon)

# 1) Define a PCA for the decathlon dataset and discuss why the model you own works well (or not) looking
# the variables chart. We recommend using FactoMineR package to do this analysis. Interpret variables and
# individuals charts to understand the relations between the different variables of interest and individuals
# scores on the dimensions

# As the variable "Competition" is not numerical, we can not use it for the PCA
# so we remove it from the dataset. As we want to predict the variable "Rank,
# it would make no sense to use it in the prediciont model, for this reason,
# we elimnate it.
normalized_data <- decathlon[, -13]
normalized_data <- normalized_data[, -11]

# Before performing the PCA we will check which Principal Components we should
# use with KMO test. The Kaiser-Meyer-Olkin (KMO) represents the degree to which
# each observed variable is predicted by the other variables in the
# dataset and with this indicates the suitability for factor analysis.

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

# After the transformations all the variables follow the same direction.

# Before starting the PCA analysis we have to check that the data fullfill
# sphericity.
# We can check sphericity using Bartlett test. Bartlett uses the correlation
# matrix and the dimension of the data.
n <- nrow(normalized_data)
cortest.bartlett(correlation_matrix, n)

# As the p-value (5.837618e-67) < 0.05 we have enough evidence to reject the
# null hypothesys so the assumption of correlation between features is
# fulfilled

# KMO with normalized data
kmo(normalized_data)

# As the values are still very low we will remove the variable with the low KMO
# in order to improve the model.
# First we remove pole.vault
normalized_data <- normalized_data[, -8]
kmo(normalized_data)

# As the KMO is still low we remove javeline
normalized_data <- normalized_data[, -8]
kmo(normalized_data)

# As the KMO is still low we remove 1500m
normalized_data <- normalized_data[, -8]
kmo(normalized_data)

# With a KMO value of 0.74 we can state that this is a reasonable model to
# perform a PCA

pca <- PCA(normalized_data)
plot(pca, choix = "var")

# Looking at the PCA we can state that:
# 1 - The variable have a strong relation with the first dimensino which is
#     explaining over 51% of the variance of the data.
# 2 - It seems that the first two components are enough to explain the variance
#     of the data overall

# In order to know how many principal components are significant for the
# analysis of the variance of the data.

names(eigenvalues) <- colnames(normalized_data)
print(eigenvalues)

# As we can see through the eigenvalues, only 100 meters and long jump have a
# value higher than 1, so, we can conclude that these two principal components
# are enough to explain the variance of the data.

# Moreover, we want to visually check the conclusion using the Scree Plot
par(mfrow = c(1, 2))
plot(pca$eig[, 2],
     type = "b",
     ylab = "Proportion of variance explained")
plot(pca$eig[, 3],
     type = "b",
     ylim = c(0, max(pca$eig[, 3])),
     ylab = "Cumulative Proportion of variance explained")

# Moreover, we want to know in which component are the variables explained the
# most. We can see it through the cosen of the models.
pca$var$cos2

# As we can see the vast majority (6 out of 8) of the variables is more
# explained by the first principal component. Except 'shot.put' and 'discus'
# which are better explained by the second dimension. This could be related
# to the fact that the disciplines are similar (both involves thrusting an
# object) and measure the score in meter.
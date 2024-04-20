############################# PART 4

library(lmtest)
library(car)
library(FactoMineR)
library(corrplot)
library(psych)

data(decathlon)

# 1) Define a PCA for the decathlon dataset and discuss why the model you own works well (or not) looking
# the variables chart. We recommend using FactoMineR package to do this analysis. Interpret variables and
# individuals charts to understand the relations between the different variables of interest and individuals
# scores on the dimensions

# As the variable "Competition" is not numerical, we can not use it for the PCA
# so we remove it from the dataset.
normalized_data <- decathlon[, -13]
# As we only want to use the values that are related with a discipline, and as
# point is not related with a specific discipline: we remove it.
normalized_data <- normalized_data[, -12]
# As we want to predict the variable "Rank, it would make no sense to use it in
# the prediciont model, for this reason, we elimnate it.
normalized_data <- normalized_data[, -11]

# Before performing the PCA we will check which Principal Components (PCs) we should
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

# As the p-value (1.15685e-10) < 0.05 we have enough evidence to reject the
# null hypothesys so the assumption of correlation between features is
# fulfilled

# KMO with normalized data
kmo(normalized_data)

# As the values are still very low we will remove the variable with the low KMO
# in order to improve the model.
# First we remove pole.vault
normalized_data <- normalized_data[, -8]
kmo(normalized_data)

# As the KMO is still low we remove 1500m
normalized_data <- normalized_data[, -9]
kmo(normalized_data)

# As the KMO is still low we remove javeline
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

# In order to know how many PC are significant for the
# analysis of the variance of the data.

names(eigenvalues) <- colnames(normalized_data) # OTODO: check na
print(eigenvalues)

# As we can see through the eigenvalues, only 100 meters and long jump have a
# value higher than 1, so, we can conclude that these two PCs
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

# As we can see the vast majority (6 out of 7) of the variables is more
# explained by the first PC. Except 'shot.put' and 'discus'
# which are better explained by the second dimension. This could be related
# to the fact that the disciplines are similar (both involves thrusting an
# object) and measure the score in meter.

# 2) Now we are going to continue with PC regression. We want
#    to construct a linear regression model to predict the points of each
#    athlete. To do so you must
#    First  decide  the  number  of  principal  components  to  be  included
#    in  the  regression  as  independent  variables. Justify your answer.
#    Check the assumptions of the regression model.
#    Is the prediction accurate enough?

# As in the prvious section we have concluded that the first two principal
# components (100m and Long.jumo) are enough to explain the variance of
# the data.
# We will perform the linerar model with these two PC as independent
# variables.

model <- lm(decathlon$Points ~ decathlon$`100m` + decathlon$Long.jump)
summary(model) # adjusted R square 0.6035

# Homosedasticity
bptest(model)
# Adjusted R-squared: 0.638

# Normality of residuals
plot(model, 2)
shapiro.test(model$residuals)
# In this case the p-val(0.4211)>0.05 so we have enough evidence for accepting
# the null hypothesis, the assumption of normality in the residuals is
# fulfilled.
# Both from the Q-Q plot and from shapiro we can conclude that the residuals
# are normally distributed.

# No multicolinearity
vif(model)
# The correlation between 100m and Long.jump is 1.55864, we can state that the
# assumption of not multicolinearity between the independent variables
# (100m and Long.jump) is fulfilled

# Independence of errors
# We can use DurbinWatson test to verify the independence of errors
dwtest(model)
# The test yields a DW value equal to 1.70 means which confirms the independence
# of errors


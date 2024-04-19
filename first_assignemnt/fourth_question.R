############################# PART 4

library(FactoMineR)
data(decathlon)

# 1) Define a PCA for the decathlon dataset and discuss why the model you own works well (or not) looking
# the variables chart. We recommend using FactoMineR package to do this analysis. Interpret variables and
# individuals charts to understand the relations between the different variables of interest and individuals
# scores on the dimensions

# Before performing the PCA we will check which Principal Components should we use with KMO test. 
# The Kaiser-Meyer-Olkin (KMO) represents the degree to which each observed variable is predicted by the other variables in the 
# dataset and with this indicates the suitability for factor analysis.

# As the variable "Competition" is not numeric, we can not use it for the PCA so we will remove it from the dataset.
decathlon <- decathlon[, -13]

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
  return(list(KMO=KMO, MSA=MSA))
}

kmo(decathlon)

decathlon <- decathlon[, -11]

kmo(decathlon)

pca <- PCA(decathlon)



# we can use KMO to test which PC should we use in a PCA, value should be 80 more or less. Nihan uses a function KMO from a professor (search)

# check pca$eig, look for eigenvalues greater than 1 --- "we can conclude that x PC are enough to explain the variance"

# check plot(pca, choix = "var") and plot(pca, choix = "ind")

# we can check the flag axes = x to check different dimensions comparison

# use dwtest() for checking independence of errors

# check pca$var$cos2 to know in which component are the variables explained most

# chech pca$var$contrib to know how each variable contribute to each dimension

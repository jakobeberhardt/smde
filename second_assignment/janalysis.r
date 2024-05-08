data2017 <- read.csv("data/marathon_results_2017.csv")
data2017$total_secs <- as.numeric(as.difftime(unlist(data2017['Official.Time']), units = "secs"))
data2017["M.F"] <- as.factor(data2017["M.F"])
# summary of data in 3rd quantile
summary(data2017[data2017$total_secs > 15708,])
library("vvconverter")

filtered_marathon <- data2017[as.numeric(sapply(data2017["Bib"], destring)) < 10000,] #  & data2017["M.F"] == 'M'
exclude_outliers <- mean(filtered_marathon$total_secs) + 2*sd(filtered_marathon$total_secs)
filtered_marathon <-  filtered_marathon[filtered_marathon$total_secs < exclude_outliers,]

hist(unlist(filtered_marathon['total_secs']))

qqnorm(filtered_marathon$total_secs)
qqline(filtered_marathon$total_secs)


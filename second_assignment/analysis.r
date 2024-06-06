data2017 <- read.csv("data/marathon_results_2017.csv")

# Converting time to seconds
data2017$total_secs <- as.numeric(as.difftime(unlist(data2017['Official.Time']), units = "secs"))

# Changing gender to a factor
data2017["M.F"] <- as.factor(data2017["M.F"])

# Summary of data in 3rd quantile
summary(data2017[data2017$total_secs > 15708,])
library("vvconverter")

# Taking first 10000 runner based on their Bib number i.e. selecting elite runners
filtered_marathon <- data2017[as.numeric(sapply(data2017["Bib"], destring)) < 10000,]

# Excluding runners outside of 2 standard deviations
exclude_outliers <- mean(filtered_marathon$total_secs) + 2*sd(filtered_marathon$total_secs)
filtered_marathon <- filtered_marathon[filtered_marathon$total_secs < exclude_outliers,]

var(filtered_marathon$total_secs)
sqrt(var(filtered_marathon$total_secs))
mean(filtered_marathon$total_secs)

hist(unlist(filtered_marathon['total_secs']))

qqnorm(filtered_marathon$total_secs)
qqline(filtered_marathon$total_secs)

boxplot_secs <- boxplot(filtered_marathon$total_secs, plot=TRUE)
outliers_secs <- boxplot_secs$out
outliers_secs

# Further removing outliers
filtered_marathon <- filtered_marathon %>% filter(!(total_secs %in% outliers_secs))

pdf("./histogram.pdf")
hist(unlist(filtered_marathon['total_secs']), main="Histogram of Group Run Time", xlab="Time in Secs", ylab="Count", col="skyblue")
dev.off()

pdf("./qq.pdf")
qqnorm(filtered_marathon$total_secs, main = "Normal Q-Q Plot for total run time")
qqline(filtered_marathon$total_secs)
dev.off()

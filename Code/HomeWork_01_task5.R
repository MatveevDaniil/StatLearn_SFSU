library(ggplot2)

auto_path <- './Data/auto/Auto.data'
data <- read.table(
  auto_path, 
  header = TRUE, 
  colClasses = list('horsepower'='numeric'), 
  na.strings = c('NA', '?'))


#############################
# (a)
# there are NA values only in the horsepower column
# we can fill NA with mean:
#############################

hp <- data$horsepower
mean_hp <- mean(hp[!is.na(data)])
data$horsepower[is.na(data$horsepower)] <- mean_hp
print(mean(data$horsepower))


#############################
# (b)
# let's create histogram and  calculate sample statistics (min, max, q1, ...)
#############################

whist <- ggplot(data.frame(data$weight), aes(x = data$weight)) +
  geom_histogram(binwidth = 125, fill = 'lightblue', color = 'black') +
  labs(title = 'Weight histogram', x = 'weight', y = 'frequency')
ggsave('./PlotsTables/hw_01_5_auto_weight_hist.png', whist)

summary_stats <- summary(data$weight)
min_value <- min(data$weight)
max_value <- max(data$weight)
q1_value <- quantile(data$weight, 0.25)
median_value <- median(data$weight)
q3_value <- quantile(data$weight, 0.75)
variance_value <- var(data$weight)
std_deviation_value <- sd(data$weight)
stats_data <- data.frame(
  Statistic = c('Minimum', 'Maximum', 'Q1', 'Median', 'Q3', 'Standard Deviation', 'Variance'),
  Value = c(min_value, max_value, q1_value, median_value, q3_value, std_deviation_value, variance_value)
)
write.csv(stats_data, './PlotsTables/hw_01_5_auto_weight_statistics.csv', row.names = FALSE)


#############################
# (c)
# Let's create with plot with linear regression approximation
#############################

auto_mpg_scatter <- ggplot(data, aes(x = data$weight, y = data$mpg)) +
  geom_point() +
  geom_smooth(aes(color = 'Linear Regression'), method = 'lm', se = FALSE) +
  scale_color_manual(values = 'blue', guide = guide_legend(title = NULL)) +
  labs(title = 'cars in Weight-MPG space', x = 'Weight', y = 'MPG') + 
  theme(legend.position = c(0.85, 0.85)) 
ggsave('./PlotsTables/hw_01_5_auto_mpg_scatter.png', auto_mpg_scatter)


#############################
# (d)
# identify the name, mpg, weight for the subjects whose mpg is above 
# 40 from the scatterplot generated in the previous part.
#############################

mpg_ge_40 <- data[data$mpg > 40, c('name', 'mpg', 'weight')]
write.csv(mpg_ge_40, './PlotsTables/hw_01_5_auto_mpg_ge_40.csv', row.names = FALSE)

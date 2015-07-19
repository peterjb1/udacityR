library(ggplot2)
data(diamonds)
#scatterplot of price vs x
ggplot(aes(x = x,y = price),data = diamonds) +
  geom_point()
names(diamonds)
?with
with(data = diamonds,cor.test(x,price))
# data:  x and price
# t = 440.16, df = 53938, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.8825835 0.8862594
# sample estimates:
#   cor 
# 0.8844352
with(data = diamonds,cor.test(y,price))
with(data = diamonds,cor.test(z,price))

#scatterplot of price vs depth
ggplot(aes(x=depth,y=price),data=diamonds) +
  geom_point(alpha = 1/100) + scale_x_continuous(breaks = seq(44,80,2))

with(data=diamonds,cor(price,depth))
# -0.01
# #The pearson correlation coefficient is very close to 0. 
# However, this correlation reflects linear relationship, 
# and so there might be another relationship present.

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.
# Create a new variable for volume in the diamonds data frame.

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x=volume,y=price),data = diamonds) +
  geom_point()

#Correlation with subsets
#Condition: Volume not 0 and less than 800
with(subset(diamonds,volume > 0 & volume < 800),cor(volume,price))

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot.
x.vsub = subset(diamonds,volume > 0 & volume < 800)
ggplot(aes(x=volume ,y = price),data = x.vsub) +
  geom_point(alpha = 1/100) + geom_smooth(method = 'lm')

# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.
# Name the data frame diamondsByClarity
# The data frame should contain the following
# variables in this order.
#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n
# where n is the number of diamonds in each
# level of clarity.
?summarise
library(dplyr)
diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = as.numeric(median(price)),
            min_price = min(price),
            max_price = max(price),
            n = n()
  )
# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.
library(gridExtra)
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_by_color <- group_by(diamonds, color)
head(diamonds_by_color)
p1 = ggplot(aes(x=clarity),data = diamonds_by_clarity) + geom_bar()
p2 = ggplot(aes(x=color),data = diamonds_by_color) + geom_bar()
grid.arrange(p1,p2)
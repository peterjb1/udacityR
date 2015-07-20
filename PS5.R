library(ggplot2)
data(diamonds)
head(diamonds)
Q1:
  # Create a histogram of diamond prices.
  # Facet the histogram by diamond color
  # and use cut to color the histogram bars.
  # Note: In the link, a color palette of type
  # 'qual' was used to color the histogram using
  # scale_fill_brewer(type = 'qual')
ggplot(aes(x=price),data=diamonds) + geom_histogram(aes(color = cut),binwidth = 40) + 
    facet_wrap(~color) + scale_fill_brewer(type = 'qual')
install.packages('scales')
library(scales)
q2:
  # Create a scatterplot of diamond price vs.
  # table and color the points by the cut of
  # the diamond.
ggplot(aes(x=table,y=price),data=diamonds) + geom_point(aes(color = cut)) +
  scale_x_continuous(limits = c(50,80), breaks = pretty_breaks(n = 15))

q3:
  # Create a scatterplot of diamond price vs.
  # volume (x * y * z) and color the points by
  # the clarity of diamonds. Use scale on the y-axis
  # to take the log10 of price. You should also
  # omit the top 1% of diamond volumes from the plot.
  # Note: In the link, a color palette of type
  # 'div' was used to color the scatterplot using
  # scale_color_brewer(type = 'div')
  
  ggplot(aes(x=x*y*z,y=log10(price)),data=diamonds) + geom_point(aes(color=clarity)) +
  xlim(0,quantile(diamonds$x*diamonds$y*diamonds$z,.99)) + scale_color_brewer(type='div')
  
q4:
  # Many interesting variables are derived from two or more others.
  # For example, we might wonder how much of a person's network on
  # a service like Facebook the user actively initiated. Two users
  # with the same degree (or number of friends) might be very
  # different if one initiated most of those connections on the
  # service, while the other initiated very few. So it could be
  # useful to consider this proportion of existing friendships that
  # the user initiated. This might be a good predictor of how active
  # a user is compared with their peers, or other traits, such as
  # personality (i.e., is this person an extrovert?).
  
# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.
setwd('~/R')
pf = read.csv('pseudo_facebook.tsv',sep = '\t')
pf$prop_initiated <- pf$friendships_initiated / pf$friend_count

q5:
  # Create a line graph of the median proportion of
  # friendships initiated ('prop_initiated') vs.
  # tenure and color the line segment by
  # year_joined.bucket.

  pf$year_joined <- floor(2014 - pf$tenure / 365)
pf$year_joined.bucket <- cut(pf$year_joined,c(2004,2009,2011,2012,2014))

ggplot(aes(x=tenure,y=prop_initiated),data=pf) + 
  geom_line(aes(color = pf$year_joined.bucket),
            stat ="summary", 
            fun.y=median) 
#both fun.y and stat are necessary to have a nice graph
#the lies represent median proportion of friendships initiated

q6:
  # Smooth the last plot you created of
  # of prop_initiated vs tenure colored by
  # year_joined.bucket. You can use larger
  # bins for tenure or add a smoother to the plot.
  ggplot(aes(x=30*round(tenure/30),y=prop_initiated),data=pf) + 
  geom_line(aes(color = pf$year_joined.bucket),
            stat ="summary", 
            fun.y=median) 
  
  #group 2012,2014's mean proportion of friends initiated
  summary(subset(pf$prop_initiated,pf$year_joined.bucket == "(2012,2014]"))
 
  q7:
    # Create a scatter plot of the price/carat ratio
    # of diamonds. The variable x should be
    # assigned to cut. The points should be colored
    # by diamond color, and the plot should be
    # faceted by clarity.
    
    # The plot should look something like this.
    # http://i.imgur.com/YzbWkHT.jpg.
    
    # Note: In the link, a color palette of type
    # 'div' was used to color the histogram using
  # scale_color_brewer(type = 'div')
  
  ggplot(aes(x=cut,y=price/carat),data=diamonds) +
    geom_jitter(aes(color = color)) + facet_wrap(~clarity) +
    scale_color_brewer(type='div')
  
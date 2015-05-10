#PROBLEM SET 4
setwd("C:/Users/Nick/Udacity")
# READ IN DIAMOND DATA
library("ggplot2")
library("reshape2")
library("dplyr")

data(diamonds)

# In this problem set, you'll continue
# to explore the diamonds data set.

# Your first task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.

# This assignment is not graded and
# will be marked as correct when you submit.

# SCATTER PLOT OF DIAMOND PRICE VS. X

      ggplot(aes(x = x, y = price), data = diamonds) +
            geom_point(color = 'blue', position = position_jitter(h=0)) +
            xlab("X") +
            ylab("Price") +
            ggtitle("Relationship between Diamond Prices and X") +
            theme(axis.title.y = element_text(angle=0))
      ggsave("price_scatter.png")
            
# THERE APPEARS TO BE A POSITIVE RELATIONSHIP BETWEEN PRICE AND X. BUT IT'S
# NOT NECESSARILY LINEAR. 

with(diamonds, cor.test(price, x))
with(diamonds, cor.test(price, y))
with(diamonds, cor.test(price, z))

# Create a simple scatter plot of price vs depth.

# This assignment is not graded and
# will be marked as correct when you submit.

# SCATTER PLOT OF PRICE VS. DEPTH

ggplot(aes(x = depth, y = price), data = diamonds) +
      geom_point(color = 'blue', position = position_jitter(h=0)) +
      xlab("Depth") +
      ylab("Price") +
      ggtitle("Relationship between Diamond Prices and Depth") +
      theme(axis.title.y = element_text(angle=0))
ggsave("pricedepth_scatter.png")

# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. See the instructor notes
# for two hints.

#ADJUST SCATTERPLOT TO 1/100 TRANSPARENCY, MAKE X AXIS DISCRETE BY 2
ggplot(aes(x = depth, y = price), data = diamonds) +
      geom_point(color = 'blue', alpha=1/100, position = position_jitter(h=0)) + 
      scale_x_continuous(limits=c(40, 80), breaks = seq(40,80,2)) +
      xlab("Depth") +
      ylab("Price") +
      ggtitle("Relationship between Diamond Prices and Depth") +
      theme(axis.title.y = element_text(angle=0))
ggsave("pricedepth_scatter.png")

quantile(diamonds$depth, probs=c(.05,.95))

with(diamonds, cor.test(depth, price))

# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.

# This assignment is not graded and
# will be marked as correct when you submit.

# SCATTERPLOT: PRICE BY CARAT, REMOVE TOP 1% of price, carat
y <- as.numeric(quantile(diamonds$price, probs=.99))
z <- as.numeric(quantile(diamonds$carat, probs=.99))
data <- subset(diamonds, price<17378.22)
data2 <- subset(data, carat<2.18)

ggplot(aes(x = carat, y = price), data = data2) +
      geom_point(color = 'blue', alpha=1/100, position = position_jitter(h=0)) + 
      xlab("Carats") +
      ylab("Price") +
      ggtitle("Relationship between Diamond Prices and Carats") +
      theme(axis.title.y = element_text(angle=0))
ggsave("pricecarats_scatter.png")

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

# Create a new variable for volume in the diamonds data frame.
# This will be useful in a later exercise.

# Don't make any adjustments to the plot just yet.

# This assignment is not graded and
# will be marked as correct when you submit.

# SCATTERPLOT, PRICE VS VOLUME
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

ggplot(aes(x = volume, y = price), data = diamonds) +
      geom_point(color = 'blue', alpha=1/100, position = position_jitter(h=0)) + 
      xlab("Volume") +
      ylab("Price") +
      ggtitle("Relationship between Diamond Prices and Volume") +
      theme(axis.title.y = element_text(angle=0))
ggsave("pricevolume_scatter.png")

data <- subset(diamonds, volume>0 & volume<800)
with(data, cor.test(price,volume))

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)

# We encourage you to think about this next question and
# to post your thoughts in the discussion section.

# Do you think this would be a useful model to estimate
# the price of diamonds? Why or why not?

ggplot(aes(x = volume, y = price), data = data) +
      geom_point(color = 'blue', alpha=1/20, position = position_jitter(h=0)) + 
      xlab("Volume") +
      ylab("Price") +
      ggtitle("Relationship between Diamond Prices and Volume") +
      theme(axis.title.y = element_text(angle=0)) +
      geom_smooth(method="lm")
ggsave("pricevolume_scatterlin.png")

#No, the relationship between price and volume does not appear to be linear. 
# It might be useful to model price as a function of volume, and
# Volume**2. 

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

# DPLYR


price_clarity <- group_by(diamonds, clarity)
diamondsByClarity <- summarise(price_clarity,
                               mean_price = mean(price),
                               median_price = median(as.numeric(price)),
                               min_price = min(price),
                               max_price = max(price),
                               n=n())


diamondsByClarity <- summarise(price_clarity,
                               mean_price = mean(price),
                               median_price = median(price),
                               min_price = min(price),
                               max_price = max(price))

pf$age_with_months <- pf$age + (12-pf$dob_month)/12

age_months_groups <- group_by(pf, age_with_months)
pf.fc_by_age_months <- summarise(age_months_groups, 
                                 friend_count_mean = mean(friend_count),
                                 friend_count_median = median(friend_count),
                                 n = n())
pf.fc_by_age_months <- arrange(pf.fc_by_age_months, age_with_months)


# We've created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

# This assignment is not graded and
# will be marked as correct when you submit.


data(diamonds)
library(dplyr)
library(gridExtra)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))


g1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
      geom_bar(stat="identity")

g2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +
      geom_bar(stat="identity")

grid.arrange(g1,g2)


# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to continue the investigation you did at the
# end of Problem Set 3 or you can start fresh and choose a different
# data set from Gapminder.

# If you're feeling adventurous or want to try some data munging see if you can
# find a data set or scrape one from the web.

# In your investigation, examine pairs of variable and create 2-5 plots that make
# use of the techniques from Lesson 4.

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. the variable(s) you investigated, your observations, and any summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots


housing_12 <- read.csv("2012_Housing_Inventory.csv")

with(housing_12, cor.test(NETUNITS, AFF_HSG))

#UNSURPRISINGLY, NUMBER OF NET UNITS IS POSITIVELY CORRELATED
# WITH AFFORDABLE HOUSING UNITS

ggplot(aes(x = NETUNITS, y = AFF_HSG), data = housing_12) +
      geom_point() +
      xlab('NET HOUSING UNITS') +
      ylab('AFFORDABLE HOUSING UNITS') +
      ggtitle('NET HOUSING UNITS BY AFFORDABLE HOUSING UNITS')

by_district <- group_by(housing_12, SUPEDISTRICT)
sum_by_district <- summarise(by_district, sum_units = sum(AFF_HSG, na.rm = TRUE),
                             sum_all = sum(NETUNITS, na.rm = TRUE))

ggplot(aes(x = SUPEDISTRICT, y = sum_units), data = sum_by_district) +
      geom_bar(stat="identity", fill="blue") +
      scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11)) +
      xlab("Supervisor District") +
      ylab("Number of Affordable Housing Units") +
      ggtitle("Affordable Housing Units by District")

# CONCENTRATED IN DISTRICT 6 (SOMA)





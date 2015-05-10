#PROBLEM SET 3
setwd("C:/Users/Nick/Udacity")
# READ IN DIAMOND DATA
library("ggplot2")
library("reshape2")
data(diamonds)
names(diamonds)

# Create a histogram of the price of
# all the diamonds in the diamond data set.
# EVER MISSING?
sum(is.na(diamonds$price))

ggplot(diamonds, aes(x=price)) +
      geom_histogram(breaks=(seq(0,20000, by =500)),
                     col="black",
                     fill="blue") + 
      xlab("Price") +
      ylab("Number of Diamonds") +
      ggtitle("Distribution of Diamond Prices") +
      theme(axis.title.y = element_text(angle=0))
ggsave("price_hist.png")

# Summarize Diamond Price
summary(diamonds$price)


# How many cost less than 500?
dim(diamonds[diamonds["price"]<500,])

# How many are more than 15,000
dim(diamonds[diamonds["price"]>=15000,])

# Explore the largest peak in the
# price histogram you created earlier.

# Try limiting the x-axis, altering the bin width,
# and setting different breaks on the x-axis.

# There won't be a solution video for this
# question so go to the discussions to
# share your thoughts and discover
# what other people find.

# CREATE A HISTOGRAM OF DIAMOND PRICES WITH LARGEST PEAK
# ($500-$1000)

ggplot(diamonds, aes(x=price)) +
      geom_histogram(breaks=(seq(500,1000, by =25)),
                     col="black",
                     fill="blue") + 
      xlab("Price") +
      ylab("Number of Diamonds") +
      ggtitle("Distribution of Diamonds Priced between $500 & $1,000") +
      theme(axis.title.y = element_text(angle=0)) +
      coord_cartesian(xlim = c(500,1000))

ggsave("price_hist2.png")

# The price ranges with the largest number of diamonds are $675-700 and $700-725

# Break out the histogram of diamond prices by cut.

# You should have five histograms in separate
# panels on your resulting plot.

# SPLIT DIAMOND PRICES BY CUT, LIMIT TO prices below 
# 10,000 due to long tail. 

ggplot(diamonds, aes(x=price)) +
      geom_histogram(breaks=(seq(0,10000, by =1000)),
                     col="black",
                     fill="blue") + 
      xlab("Price") +
      ylab("Number of Diamonds") +
      ggtitle("Distribution of Diamond Prices by Cut") +
      theme(axis.title.y = element_text(angle=0)) +
      facet_wrap(~ cut, ncol=2) +
      coord_cartesian(xlim = c(0,10000))
ggsave("price_hist3.png")

# The distributions of very good, premium, & ideal look similar. 
# ideal diamond cuts are most prevalent
by(diamonds$price, diamonds$cut, summary)
by(diamonds$price, diamonds$cut, max)

# In the two last exercises, we looked at
# the distribution for diamonds by cut.

# Run the code below in R Studio to generate
# the histogram as a reminder.

# ===============================================================

qplot(x = price, data = diamonds) + facet_wrap(~cut)

# ===============================================================

# In the last exercise, we looked at the summary statistics
# for diamond price by cut. If we look at the output table, the
# the median and quartiles are reasonably close to each other.

# diamonds$cut: Fair
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     337    2050    3282    4359    5206   18570 
# ------------------------------------------------------------------------ 
# diamonds$cut: Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     327    1145    3050    3929    5028   18790 
# ------------------------------------------------------------------------ 
# diamonds$cut: Very Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     336     912    2648    3982    5373   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Premium
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326    1046    3185    4584    6296   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Ideal
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326     878    1810    3458    4678   18810 

# This means the distributions should be somewhat similar,
# but the histograms we created don't show that.

# The 'Fair' and 'Good' diamonds appear to have 
# different distributions compared to the better
# cut diamonds. They seem somewhat uniform
# on the left with long tails on the right.

# Let's look in to this more.

# Look up the documentation for facet_wrap in R Studio.
# Then, scroll back up and add a parameter to facet_wrap so that
# the y-axis in the histograms is not fixed. You want the y-axis to
# be different for each histogram.


#ADJUST Y AXIS

ggplot(diamonds, aes(x=price)) +
      geom_histogram(breaks=(seq(0,20000, by =1000)),
                     col="black",
                     fill="blue") + 
      xlab("Price") +
      ylab("Number of Diamonds") +
      ggtitle("Distribution of Diamond Prices by Cut") +
      theme(axis.title.y = element_text(angle=0)) +
      facet_wrap(~ cut, ncol=2, scales="free") +
      coord_cartesian(xlim = c(0,20000))
ggsave("price_hist4.png")

# DISTRIBUTION FOR FAIR IS MUCH DIFFERENT, GOOD IS SIMILAR TO OTHERS.

# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get
# started.

# Adjust the bin width and transform the scale
# of the x-axis using log10.

# PRICE PER CARAT LOG TRANSFORM

ggplot(diamonds, aes(x=price/carat)) +
      geom_histogram(breaks=(seq(3,4.3, by =.1)),
                     col="black",
                     fill="blue") + 
      xlab("Price per Carat") +
      ylab("Number of Diamonds") +
      ggtitle("Distribution of Price per Carat by Cut") +
      theme(axis.title.y = element_text(angle=0)) +
      facet_wrap(~ cut, ncol=2, scales="free") +
      scale_x_log10()
ggsave("price_hist5.png")

# SIMILAR DISTRIBUTION by CUT

#HOW DOES PRICE VARY BY COLOR?

ggplot(diamonds, aes(x=color, y=price, fill=color)) + 
      geom_boxplot() +
      xlab("Diamond Color Rating") +
      ylab("Diamond Price") +
      ggtitle("Distribution of Diamond Price by Color") +
      theme(axis.title.y = element_text(angle=0))
ggsave("price_boxplot_color.png")

by(diamonds$price, diamonds$color, summary)
by(diamonds$price, diamonds$color, IQR)

# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.

# There won't be a solution video for this
# exercise so go to the discussion thread for either
# BOXPLOTS BY CLARITY, BOXPLOT BY COLOR, or BOXPLOTS BY CUT
# to share you thoughts and to
# see what other people found.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')


# HOW DOES PRICE PER CARAT VARY BY COLOR
ggplot(diamonds, aes(x=color, y=price/carat, fill=color)) + 
      geom_boxplot() +
      xlab("Diamond Color Rating") +
      ylab("Diamond Price per Carat") +
      ggtitle("Distribution of Diamond Price per Carat by Color") +
      theme(axis.title.y = element_text(angle=0)) +
      scale_y_log10()
ggsave("pricecarat_boxplot_color.png")

# FREQUENCY POLYGON OF CARATS

ggplot(diamonds, aes(x=carat)) +
      # add a sensity line
      geom_freqpoly(binwidth=.01) +
      xlab("Diamond Carat") +
      ylab("Number of Diamonds") +
      ggtitle("Distribution of Carats") 
      theme(axis.title.y = element_text(angle=0)) +
      coord_cartesian(xlim = c(0,2))

ggsave("pricecart_freqpoly.png")
      
ggplot(diamonds, aes(x=carat)) +
             # add a sensity line
             geom_freqpoly(binwidth=.01) +
             xlab("Diamond Carat") +
             ylab("Number of Diamonds") +
             ggtitle("Distribution of Carats") +
             theme(axis.title.y = element_text(angle=0)) +
            coord_cartesian(xlim = c(1.5,1.7))
 ggsave("pricecart_freqpoly.png")

# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to download a data set of your choice
# and create 2-5 plots that make use of the techniques from Lesson 3.

# You might use a simple histogram, a boxplot split over a categorical variable,
# or a frequency polygon. The choice is yours!

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. any questions you answered, your observations, and summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')



# LABOR FORCE PARTICIPATION OF AGE 65 AND OVER
# HAS THE DISTRIBUTION CHANGED OVER TIME? MAYBE 
# THOSE IN DEVELOPED COUNTRIES ARE WORKING LONGER. ON THE OTHER HAND, 
# AS GDP PER CAPITA INCREASES IN SOME LARGE DEVELOPING COUNTRIES,
# MAYBE SOME PEOPLE ARE ABLE TO SAVE MONEY FOR RETIREMENT AND WORK 
# LESS. 

g65 <- read.csv("g65.csv")
names(g65)

# CHOOSE EVERY 5 YEARS
remain <- c("Total.65..labour.to.population....","X1980", "X1985","X1990",
            "X1995","X2000","X2005")

g65 <- g65[,remain]

# RESHAPE
g65_l <- melt(g65, na.rm=TRUE, id="Total.65..labour.to.population....",
              measure=c("X1980", "X1985","X1990",
                        "X1995","X2000","X2005"), variable="year", value.name="lfp")
#FIX YEAR VARIABLE
g65_l[,"year"] <-substr(g65_l[,"year"],2,5)


#BOXPLOT
ggplot(g65_l, aes(x=year, y=lfp, fill=year)) + 
      geom_boxplot() +
      xlab("Year") +
      ylab("Labor Force Participation") +
      ggtitle("Distribution of Age 65+ Country Labor Force Participation Rates Across Time") +
      theme(axis.title.y = element_text(angle=0)) 
ggsave("lfp_boxplot_color.png")

#FREQENCY POLYGON

qplot(lfp, data = g65_l, geom = "freqpoly", binwidth = 5,
      color=year,
      main="Distribtion of 65+ LFP Across Time",
      xlab="Labor Force Participation",
      ylab="Number of Countries")
ggsave("lfp_freqpoly_color.png")

# LABOR FORCE PARTICIPATION RATES ARE DROPPING
# SLIGHTLY OVER TIME FOR THOSE AGED 65 and over. 

# Your task is to investigate the distribution of your friends'
# birth months and days.

# Here some questions you could answer, and we hope you think of others.

# **********************************************************************

# How many people share your birthday? Do you know them?
# (Reserve time with them or save money to buy them a gift!)

# Which month contains the most number of birthdays?

# How many birthdays are in each month?

# Which day of the year has the most number of birthdays?

# Do you have at least 365 friends that have birthdays on everyday
# of the year?


#BIRTHDAYS
bday <-read.csv("bday.csv")

#REFORMAT BIRTHDAYS
bday$betterDates <- as.Date(bday$dates,
                       format = "%m/%d/%y")

#PULL OUT MONTHS, DAYS
bday$month <-format(bday$betterDates, "%b")
bday$day <-as.numeric(format(bday$betterDates,"%d"))

# HOW MANY SHARE BIRTHDAY?
j <-bday[bday["month"]=="Nov" & bday["day"]==1,]
nrow(j)

# 3 Share Birthday

# How many birthdays are in each month?

ggplot(bday, aes(x=month)) +
      geom_histogram(col="black",
                     fill="blue") +
      xlab("Month") +
      ylab("Number of Birthdays") +
      ggtitle("Distribution of Birthdays by Month") +
      theme(axis.title.y = element_text(angle=0)) 
      
# MARCH HAS LARGEST NUMBER OF BIRTHDAYS

# How many birthdays are in each month?
table(bday$month)

# Which day of the year has the most number of birthdays?
j <- as.data.frame(table(bday$dates))
k <- max(j$Freq)
l <- j[j["Freq"]==k,]
l[,"Var1"]

# THERE ARE 3: 2/6/14, 5/22/14, 7/16/14

# Do you have at least 365 friends that have birthdays on everyday
# of the year?
str(bday$dates)

# No. If this were the case, then the str function would return a factor with 365
# levels. 





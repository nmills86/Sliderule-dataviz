# PROBLEM SET 5
setwd("C:/Users/Nick/Udacity")
library("ggplot2")
library("dplyr")
library("reshape2")

# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the histogram using
# scale_fill_brewer(type = 'qual')

data(diamonds)

ggplot(aes(x=price), data=diamonds) +
      geom_histogram(aes(fill=cut)) +
      xlab("Price") +
      ylab("Number of Diamonds") +
      ggtitle("Distribution of Diamond Prices by Cut") +
      theme(axis.title.y = element_text(angle=0)) +
      scale_fill_brewer(type = 'qual') +
      facet_wrap(~ color)

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the scatterplot using
# scale_color_brewer(type = 'qual')


ggplot(aes(x = table, y = price), data = diamonds) +
      geom_point(aes(color=cut)) +
      ylab("Price") +
      xlab("Table") +
      ggtitle("Diamonds Prices by Table") +
      theme(axis.title.y = element_text(angle=0)) +
      scale_color_brewer(type = 'qual')
      

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.

# The plot should look something like this.
# http://i.imgur.com/excUpea.jpg

# Note: In the link, a color palette of type
# 'div' was used to color the scatterplot using
# scale_color_brewer(type = 'div')

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

data <- subset(diamonds, volume < quantile(volume, 0.99))


ggplot(aes(y = price, x = volume), data = data) +
      geom_point(aes(color = clarity)) +
      scale_y_log10() +
      ylab("Price") +
      xlab("Volume") +
      ggtitle("Diamonds Prices by Volume") +
      theme(axis.title.y = element_text(angle=0)) +
      scale_color_brewer(type = 'div')
      
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
pf <- read.csv("pseudo_facebook.tsv", sep='\t')

pf$prop_initiated <- pf$friendships_initiated/pf$friend_count

# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5
# by first creating year_joined from the variable tenure.
# Then, we used the cut function on year_joined to create
# four bins or cohorts of users.

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

pf$year_joined <- floor(2014 - pf$tenure/365)

pf$year_joined.bucket <- cut(pf$year_joined, c(2004,2009,2011,2012,2014))



ggplot(aes(x = tenure, y = prop_initiated),
       data = pf) +
       geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median) +
       xlab("Tenure") +
       ylab("Proportion of Friendships Initiated") +
       ggtitle("% of Friendships Initiated by Tenure") +
       theme(axis.title.y = element_text(angle=0)) 
      
      
# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can use larger
# bins for tenure or add a smoother to the plot.

# There won't be a solution image for this exercise.
# You will answer some questions about your plot in
# the next two exercises.

ggplot(aes(x = tenure, y = prop_initiated),
       data = pf) +
      geom_smooth(aes(color = year_joined.bucket), stat = 'smooth', fun.y = median) +
      xlab("Tenure") +
      ylab("Proportion of Friendships Initiated") +
      ggtitle("% of Friendships Initiated by Tenure") +
      theme(axis.title.y = element_text(angle=0)) 

# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

# The plot should look something like this.
# http://i.imgur.com/YzbWkHT.jpg.

ggplot(aes(x=cut, y=price/carat), data=diamonds) +
      geom_point(aes(color=color)) +
      xlab("Cut") +
      ylab("Price/Carat Ratio") +
      ggtitle("Price/Carat Ratio by Cut") +
      theme(axis.title.y = element_text(angle=0))  +
      facet_wrap(~ clarity)

      
# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to continue the investigation you did at the
# end of Problem Set 4 or you can start fresh and choose a different
# data set from Gapminder.

# If you're feeling adventurous or want to try some data munging see if you can
# find a data set or scrape one from the web.

# In your investigation, examine 3 or more variables and create 2-5 plots that make
# use of the techniques from Lesson 5.

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. the variable(s) you investigated, your observations, and any summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# EVICTIONS BY NEIGHBORHOOD ACROSS TIME IN SF, from 1997-2014

data <- read.csv("Eviction_Notices.csv")

#CLEAN UP DATE VARIABLE 

#PULL OUT YEAR
data$year <-as.numeric(substr(as.character(data$File.Date),7,11))

#CODE EVICTION CATEGORIES

data$breach <- ifelse(data$Breach=="true",1,0)
data$ellis <-  ifelse(data$Ellis.Act.WithDrawal=="true", 1, 0)
data$nopay <- ifelse(data$Non.Payment=="true", 1, 0)
data$own <- ifelse(data$Owner.Move.In=="true", 1, 0)
data$condo <- ifelse(data$Condo.Conversion=="true",1,0)
data$unapr_sb <- ifelse(data$Unapproved.Subtenant=="true",1,0)
data$nuis <- ifelse(data$Nuisance=="true",1,0)
data$room <- ifelse(data$Roommate.Same.Unit=="true",1,0)
data$dev <- ifelse(data$Development=="true",1,0)
data$late <- ifelse(data$Late.Payments=="true",1,0)
data$other <- ifelse(data$Other.Cause=="true",1,0)
data$illegal <-ifelse(data$Illegal.Use=="true",1,0)

# COLLAPSE BY YEAR, NEIGHBORHOOD

a1 <- group_by(data, year, Neighborhood)
yeardata <- summarise(a1, number_evict=n(), ellis=sum(ellis),
                      nopay=sum(nopay), own=sum(own), condo=sum(condo),
                      unapr_sub=sum(unapr_sb), breach=sum(breach),
                      nuis=sum(nuis), room=sum(room), dev=sum(dev),
                      late=sum(late), other=sum(other), illegal=sum(illegal))

# 2015 is incomplete
yeardata <- subset(yeardata,year<2015)
j <- as.data.frame(table(data$Neighborhood))

j <- j[order(j$Freq, decreasing=TRUE),]

# PLOT # EVICTIONS BY YEAR, NEIGHBORHOOD, only those
# in the top 12 to avoid crowding. 

hoods <- c("Mission", "Sunset/Parkside", "Outer Richmond",
           "Tenderloin", "Castro/Upper Market", "Hayes Valley",
           "South of Market", "Haight Ashbury", "Nob Hill",
           "Marina", "Noe Valley", "Inner Sunset")


ggplot(aes(x = year, y=number_evict), data = subset(yeardata, Neighborhood %in% hoods)) +
      geom_line() +
      scale_x_continuous(breaks=(seq(1997,2014, by =5))) +
      facet_wrap(~ Neighborhood) +
      xlab("Year") +
      ylab("# of Eviction Notices") +
      ggtitle("Number of Eviction Notices by Year & Neighborhood")

# HAVE CATEGORIES CHANGED? 

#FIRST, MAKE SOME PERCENTAGES. THEN RESHAPE, THEN PLOT. 

yeardata$pct_ellis <- yeardata$ellis/yeardata$number_evict
yeardata$pct_norent <- yeardata$nopay/yeardata$number_evict
yeardata$pct_leasebreach <- yeardata$breach/yeardata$number_evict
yeardata$pct_ownermovein <- yeardata$own/yeardata$number_evict
yeardata$pct_nuisance <- yeardata$nuis/yeardata$number_evict

y_long <- melt(yeardata, id=c("year", "Neighborhood"),
               measure.vars=c("pct_ellis", "pct_norent", "pct_leasebreach", "pct_ownermovein" ),
               variable.name="pct",
               value.name="category")

ggplot(aes(x = year, y=category), data = subset(y_long, Neighborhood %in% hoods)) +
      geom_line(aes(color=pct)) +
      scale_x_continuous(breaks=(seq(1997,2014, by =5))) +
      facet_wrap(~ Neighborhood) +
      xlab("Year") +
      ylab("# of Eviction Notices") +
      ggtitle("Number of Eviction Notices by Year & Neighborhood")


# INTERESTING THAT THERE WERE A SIZEABLE NUMBER OF EVICTIONS IN THE 
# LATE 1990s, also that it was primarily driven by owner moveins. 
# ELLIS ACT EVICTION NOTICES ARE A MINORITY OF TOTAL EVICTION NOTICES, 
# BUT IS RISING. LASE BREACH IS ALSO HIGH in MANY NEIGHBORHOODS. 
# CAVEAT: THESE ARE EVICTION NOTICES, NOT ACTUAL EVICTIONS. 












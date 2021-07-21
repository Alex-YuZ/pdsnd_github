# load related packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)

# load in the data
ny = read.csv('new-york-city.csv', stringsAsFactors=FALSE)
wash = read.csv('washington.csv', stringsAsFactors=FALSE)
chi = read.csv('chicago.csv', stringsAsFactors=FALSE)

# `New York` data frame overview
head(ny)

# `Washington` data frame overview
head(wash)

# `Chicago` data frame overview
head(chi)

# add `Gender` and `Brith.Year` to `wash` and filled with `NA`
wash$Gender <- c(rep(NA, nrow(wash)))
wash$Birth.Year <- c(rep(NA, nrow(wash)))

# add `City` feature in the dataset 
ny$City <- c(rep("New_York", nrow(ny)))
wash$City <- c(rep("Washington", nrow(wash)))
chi$City <- c(rep("Chicago", nrow(chi)))

df <- rbind(chi, ny, wash)

# Validation: data frame overview after combination
str(df)


# convert to datetime data type
df$Start.Time <- as_datetime(df$Start.Time)
df$End.Time <- as_datetime(df$End.Time)

# validation after conversion
class(df$Start.Time)
class(df$End.Time)

# convert to `integer` type
df$Birth.Year <- as.integer(df$Birth.Year)

# derive a `Age` feature from `Birth.Year`
df$Age <- 2017 - df$Birth.Year

# check lelvels of `Gender`
unique(df$Gender)

# how many counts in each level
df %>%
  count(Gender)

# filter out the invalid data under `Gender`
df %>%
  filter(Gender %in% c("Male", "Female")) %>%
  count(Gender)

# this data frame will be used for gender-related analysis
df_CleanGender <- df %>%
  filter(Gender %in% c("Male", "Female"))

df_CleanGender %>%
  count(Gender)

# check levels of `User.Type`
unique(df$User.Type)

# filter out the invalid data in `User.Type`
# The following data frame will be used for UserType-related analysis
df_CleanUserType <- df %>%
  filter(User.Type %in% c("Customer", "Subscriber"))

# Set figure size globally
options(repr.plot.width = 8, repr.plot.height = 4)

# Question 1
# view by month
ggplot(df, aes(x=month(df$Start.Time))) +
  geom_bar(color='black', fill='deepskyblue', width=.6) +
  scale_x_continuous(breaks=seq(1, 6)) +
  scale_y_continuous(labels=scales::comma) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=.4)) +
  labs(title = "User Counts in the First Half of a Year", x="Month", y="Number of Users")

by(df, month(df$Start.Time), count)

# view by weeday
ggplot(df, aes(x=wday(df$Start.Time))) +
  geom_bar(color='black', fill='deepskyblue', width=.6) +
  scale_x_continuous(breaks=seq(1, 7)) +
  scale_y_continuous(labels=scales::comma) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=.4)) +
  labs(title = "User Counts in the Days of Week", x="Weekday", y="Number of Users")

# view by hour
ggplot(df, aes(x=hour(df$Start.Time))) +
  geom_bar(color='black', fill='deepskyblue') +
  scale_x_continuous(breaks=seq(0,23,1)) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=.4)) +
  labs(title = "User Counts in Hours of a Day", x="Hour in a Day", y="Number of Users")

# Question 2
# histogram plot of trip  duration by Hour unit
ggplot(df, aes(x=Trip.Duration/60)) +
  geom_histogram(binwidth=1, color='black', fill='deepskyblue') +
  scale_x_continuous(limits=c(0, 2421/60), breaks=seq(1, 40)) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=.4)) +
  labs(title = "Distribution of Duration of a Trip (in Hour)", x="Trip Duration", y="Number of Users")

# histogram plot of trip  duration facetted by City
ggplot(df, aes(x=Trip.Duration/60)) +
  geom_histogram(binwidth=1, color='black', fill='deepskyblue') +
  scale_x_continuous(limits=c(0, 2421/60), breaks=seq(1, 40, 5)) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=.4)) +
  labs(title = "Distribution of Duration of a Trip (in Hour)", x="Trip Duration", y="Number of Users") +
  facet_wrap(~City)

# boxplot by city
ggplot(df, aes(x=City, y=Trip.Duration/60)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0, 2200/60)) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=.4)) +
  labs(title = "Duration of a Trip in Different Cities", y="Trip Duration (Hour)", x="City")

# statistics report
by(df$Trip.Duration/60, df$City, summary)

# Question3
summary_userTypePct <- df_CleanUserType %>%
  group_by(City, User.Type) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count))

ggplot(summary_userTypePct, aes(x=factor(City), y=pct*100, fill=factor(User.Type))) +
  geom_bar(stat='identity', width=.6) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=.4), legend.title=element_text(size=10), 
        axis.title=element_text(size = 12)) +
  labs(title = "User Type Relative Difference in Different Cities", x="City", y="Percent", fill='UserType')

summary_userTypePct

ggplot(subset(df_CleanGender, User.Type!='Dependent' & !User.Type==''), aes(x=Gender, y=Trip.Duration/60)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0, 2200/60)) +
  facet_grid(~City~User.Type) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust=.4), legend.title=element_text(size=12), 
        axis.title=element_text(size = 12)) +
  labs(title = "Trip Duration Pattern Among Users", x="Gender", y="Trip Duration (Hour)")




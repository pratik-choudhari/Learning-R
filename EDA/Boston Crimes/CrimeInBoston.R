library(DataExplorer)
library(dplyr)
library(Hmisc)
library(tibble)

# Dataset of Boston over 2015-2018
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
crime <- read.csv("crime.csv")
# Offense Code and their corresponding Names
offense <- read.csv("offense_codes.csv")

# Join the data from offense.csv and crime.csv by the attribute OFFENSE_CODE
# and store in a DataFrame
df <-
  left_join(crime, offense %>% rename(OFFENSE_CODE = CODE), by = "OFFENSE_CODE")

# String representation of dataset nd quick glimpse
str(df)
# Examine the first few observations of the three variables
head(df)
colnames(df)

# Retrive Dimensions of our Dataset
dim(df)

# Find out mean, median, Modes, min, max, quartiles, Number of NULL Values and other information about our data
summary(df)
glimpse(df)
colSums(is.na(df))


# CLEANING
# Remove irrelevant column from dataset
df$SHOOTING <- NULL
df$OCCURRED_ON_DATE <- NULL

colSums(is.na(df))

# Create a logical vector which contains rows that have NULL values for any column
row.has.na <- apply(df, 1, function(x) {
  any(is.na(x))
})
# Find number of rows that have NULL values
sum(row.has.na)
# Exclude NULL valued rows from our dataset
df <- df[!row.has.na,]

dim(df)
colSums(is.na(df))

# Convert factor to characters as part of data cleaning preprocessing
df$UCR_PART <- as.factor(df$UCR_PART)
df$YEAR <- as.factor(df$YEAR)
df$MONTH <- as.factor(df$MONTH)
df$DAY_OF_WEEK <- as.factor(df$DAY_OF_WEEK)

glimpse(df)

# EDA

# Plot of how crimes are carried out on various days over numerous hours.
plot(as.factor(df$DAY_OF_WEEK),
     df$HOUR,
     xlab = "Days of the Week",
     ylab = "Hours of the Day")


# plot introduction of our dataset
# Shows us column, row, and observation metrics such as completeness and missingness
plot_intro(df)

# Plots number of crimes on the basis of years.
plot_bar(df$YEAR, title = "Crime Frequency vs Years", order_bar = FALSE)

# plots frequency of crime with respect to hour of the day
plot_bar(as.factor(df$HOUR), title = "Hour of the Day vs Crime Frequency")

# plots frequency of crime with respect to month of the year
plot_bar(df$MONTH, title = "Month vs Crime Frequency")

# plots frequency of crime with respect to day of the week
plot_bar(df$DAY_OF_WEEK)

# The Uniform Crime Reporting (UCR) program compiles official data on crime in
# the United States, published by the Federal Bureau of Investigation (FBI).
plot_bar(df$UCR_PART)


# In Part I, the UCR indexes reported incidents of index crimes which are broken
# into two categories: violent and property crimes. Aggravated assault, forcible
# rape, murder, and robbery are classified as violent while arson, burglary,
# larceny-theft, and motor vehicle theft are classified as property crimes.
# Part 1 crimes are collectively known as Index crimes, this name is used because
# the crimes are considered quite serious, tend to be reported more reliably than
# others, and the reports are taken directly by the police and not a separate
# agency which aggregates the data and does not necessarily contribute to the UCR.
seriouscrimes <- df[df$UCR_PART == "Part One", ]

# Here we see that Larceny is the most common crime in boston amongts serious crimes
plot_bar(seriouscrimes$OFFENSE_CODE_GROUP, title = "Serious Severity Crime Frequency")


# In Part II, the following categories are tracked: simple assault, curfew
# offenses and loitering, embezzlement, forgery and counterfeiting, disorderly
# conduct, driving under the influence, drug offenses, fraud, gambling, liquor
# offenses, offenses against the family, prostitution, public drunkenness,
# runaways, sex offenses, stolen property, vandalism, vagrancy, and weapons offenses.
modcrimes <- df[df$UCR_PART == "Part Two", ]

# We see that assault and vandalism are the most common type of crime in the
# moderate crimes
plot_bar(modcrimes$OFFENSE_CODE_GROUP, title = "Moderate Severity Crime Frequency")


lessercrimes <- df[df$UCR_PART == "Part Three", ]
plot_bar(lessercrimes$OFFENSE_CODE_GROUP, title = "Less Severity Crime Frequency")

# here we see year wise distribution of serious crimes in boston
ggplot(data = seriouscrimes) +
  geom_bar(mapping = aes(x = OFFENSE_CODE_GROUP), ) +
  facet_wrap(~ YEAR, nrow = 1) +
  coord_flip() +
  xlab("") +
  ylab("Count")

# here we see the hour wise distribution of serious crimes
  ggplot(data = seriouscrimes) +
  geom_bar(mapping = aes(x = HOUR, fill = OFFENSE_CODE_GROUP)) +
  xlab("Hour") +
  ylab("Frequency of crimes")

# Here we see the hourly distribution of serious crimes over the year
ggplot(data = seriouscrimes) +
  geom_bar(mapping = aes(x = HOUR)) +
  facet_wrap(~ MONTH) +
  xlab("Hour") +
  ylab("Frequency of crimes") +
  ggtitle("Hourly Crime Rates by Month") +
  theme(plot.title = element_text(hjust = 0.5))
 
# here we see year wise distribution of moderate crimes in boston
ggplot(data = modcrimes) +
  # geom_bar(mapping = aes(x = OFFENSE_CODE_GROUP)) +
  geom_bar(mapping = aes(x = forcats::fct_infreq(OFFENSE_CODE_GROUP))) +
  xlab("Crime") +
  ylab("Frequency") +
  coord_flip()

# Here we see the distribution of top 5 lesser crimes per hour of the day
df %>% filter(OFFENSE_DESCRIPTION %in% (
  df  %>% count(OFFENSE_DESCRIPTION) %>% arrange(-n) %>% head(5) %>% pull(OFFENSE_DESCRIPTION)
)) %>%
  ggplot(aes(HOUR, fill = OFFENSE_DESCRIPTION)) +
  geom_bar() +
  xlab("Hour of the day") + 
  ylab("Frequency of crimes") + 
  # coord_flip() +
  scale_fill_ordinal()

# Stores the count of crimes by street
street = df %>% count(STREET) %>% arrange(-n) %>% head(10)

# a plot street-wise crime rates
# we see that Washington Street has way more crimes than other streets
ggplot(street) +
  geom_col(aes(x = reorder(STREET, n), y = n), fill = "navy") +
  coord_flip() +
  xlab("Street") +
  ylab("Count")

# we see the top 10 crimes plotted by magnitude
df  %>% count(OFFENSE_CODE_GROUP) %>% arrange(-n) %>% head(10) %>%
  ggplot(aes(reorder(OFFENSE_CODE_GROUP, n), n)) +
  geom_col(fill = "navy") +
  coord_flip() +
  labs(x = "Frequency", y = "Top 10 Crimes" )

# Top 5 crimes in 2015 plotted with magnitude
df2015 = df %>% filter(YEAR == 2015)
df2015 %>% count(OFFENSE_DESCRIPTION) %>% arrange(-n) %>% head(5) %>%
  ggplot(aes(reorder(OFFENSE_DESCRIPTION, n), n)) +
  geom_col(fill = "navy") +
  coord_flip() +
  xlab("Top 5 Crimes in 2015") +
  ylab("Crimes")

# Top 5 crimes in 2016 plotted with magnitude
df2016 = df %>% filter(YEAR == 2016)
df2016 %>% count(OFFENSE_DESCRIPTION) %>% arrange(-n) %>% head(5) %>%
  ggplot(aes(reorder(OFFENSE_DESCRIPTION, n), n)) +
  geom_col(fill = "navy") +
  coord_flip() +
  xlab("Top 5 Crimes in 2016") +
  ylab("Crimes")

# Top 5 crimes in 2017 plotted with magnitude
df2017 = df %>% filter(YEAR == 2017)
df2017 %>% count(OFFENSE_DESCRIPTION) %>% arrange(-n) %>% head(5) %>%
  ggplot(aes(reorder(OFFENSE_DESCRIPTION, n), n)) +
  geom_col(fill = "navy") +
  coord_flip() +
  xlab("Top 5 Crimes in 2017") +
  ylab("Crimes")

# Top 5 crimes in 2018 plotted with magnitude
df2018 = df %>% filter(YEAR == 2018)
df2018 %>% count(OFFENSE_DESCRIPTION) %>% arrange(-n) %>% head(5) %>%
  ggplot(aes(reorder(OFFENSE_DESCRIPTION, n), n)) +
  geom_col(fill = "navy") +
  coord_flip() +
  xlab("Top 5 Crimes in 2018") +
  ylab("Crimes")

# plots frequency of crime with respect to hour of the day
ggplot(data = df) +
  geom_bar(aes(HOUR,), fill = "navy") +
  xlab("Hours") +
  ylab("Frequency of crimes")

# a dataframe that contains hour-wise distribution of crime throughout the years
tbl <- with(df, table(YEAR, HOUR))
# a plot that displays hour-wise distribution of crime throughout the years
ggplot(as.data.frame(tbl), aes(factor(HOUR), Freq, fill = YEAR)) +
  geom_col(position = 'dodge') +
  xlab("Hours") +
  ylab("Frequency")

# Hour-wise distribution and frequency of top 10 crimes
top_crimes = df %>% filter(OFFENSE_CODE_GROUP %in% (
  df %>% count(OFFENSE_CODE_GROUP) %>% arrange(-n) %>% head(10) %>% pull(OFFENSE_CODE_GROUP)
))
ggplot(top_crimes) +
  geom_bar(aes(HOUR, fill = OFFENSE_CODE_GROUP))

# Hour-wise distribution and frequency of top 10 crimes for all 4 years
ggplot(top_crimes) +
  geom_bar(aes(HOUR, fill = OFFENSE_CODE_GROUP)) +
  facet_wrap(~ YEAR)

#setwd("C:/Users/User/Downloads/R/miniproject")

#Mini-Project Session 1 
#Choose Dataset from Kaggle, extracting data from large dataset

#extracting data from the csv file using read.csv() function
my_data = read.csv("23_Applestore.csv")

#view function takes single argument which should from the class of dataframe and then creates new file which displays whole dataset in tabular form
View(my_data)

#the dim function displays the dimensions of the table. The output takes the form of row, column
dim(my_data)

#head function shows the first n number of rows mentioned in the function
#extracting 1st 3000 rows from the dataset of 7197 rows 
z = head(my_data,3000)

#view function takes single argument which should from the class of dataframe and then creates new file which displays whole dataset in tabular form
View(z)

#class attribute, a character vector giving the names of the classes from which the object inherits.
class(z)

#the dim function displays the dimensions of the table. The output takes the form of row, column
dim(z)

#head function shows the first n number of rows mentioned in the function
head(z,5)

#the summary function to show each column, its data type and a few other attributes which are especially useful for numeric attributes. We can see that for all the numeric attributes, it also displays min, 1st quartile, median, mean, 3rd quartile and max values.
summary(z)



# Mini-Project Session 2:
# Cleaning of the Dataset

#class attribute, a character vector giving the names of the classes from which the object inherits.
class(my_data)



# The dplyr package makes these steps fast and easy:
# By constraining your options, it helps you think about your data manipulation challenges.
# It provides simple "verbs", functions that correspond to the most common data manipulation tasks, to help you translate your thoughts into code.
# It uses efficient backends, so you spend less time waiting for the computer.

#the glimpse function from the dplyr package. This will display a vertical preview of the dataset. It allows us to easily preview data type and sample data. 
library(dplyr) 
glimpse(z) 


#The visdat package is a great way to visualize the data type and missing data within a data frame. 
library(visdat)

#vis_miss create plot of values which are present and missing in each columns
vis_miss(z)
#Hurray! from above plot we have 100% present values which means there no missing values

#Checking for missing values in the entire dataframe
any(is.na(z))

#Checking for the total number of missing values in the entire dataframe
sum(is.na(z))
#Since there are no NAs values present in the dataset so we dont need to remove any particular rows or replace them

#Eliminating missing values completely from the entire dataframe
na.omit(z)

#Adding a new variable to include size by MB
#here we have performed mathematical operation on existing variable or column of the dataset size_bytes
z$size_mb = z$size_bytes/1024/1024




# Mini-Project Session 3:
# EDA on the dataset

#The vis_dat() function of the visdat package is a great way to visualize the data type and missing data within a data frame. 
library(visdat) 

#vis_dat create plot of numeric,integer and ordered factor of each column
vis_dat(z)


#It is a generic function, meaning, it has many methods which are called according to the type of object passed to plot()
#ylim is used to limit the range in y-axis,main is used to add title to the plot and same for xlab,ylab,col is used to add color to the plot
#We have used this plot to show the Frequency of Apps based on the category
plot(z$prime_genre,ylim = c(0,1500),main = "Frequency of Apps based on the category",xlab = "Apps",ylab = "Counts",col="darkolivegreen1")
#From the above plot, we see that the top app category in our data is Games


#unique returns a vector, data frame or array like x but with duplicate elements/rows removed.
#here we have applied unique function to prime_genre to see all different/unique categories present in the column
unique(z$prime_genre)

#the glimpse function from the dplyr package. This will display a vertical preview of the dataset. It allows us to easily preview data type and sample data. 
library(dplyr)
#here is glimpse is applied on a specific (prime_genre) of the dataset
glimpse(z$prime_genre)


# That's where the tools of Automated EDA comes very handy and one such popular tool for Automated EDA in R is DataExplorer by Boxuan Cui.
library(DataExplorer)
#if above mentioned library is not present 
# install.packages("DataExplorer")
# if above command doesn't work install directly from github using library devtools
# if (!require(devtools)) install.packages("devtools")
# devtools::install_github("boxuancui/DataExplorer", ref = "develop")


#creating a report using the create_report function from DataExplorer 
#this creates an html file which inlcudes all type of Exploratory data analysis visualization
DataExplorer::create_report(z)

# Exploratory Data Analysis
# Exploratory data analysis is the process to get to know your data, so that you can generate and test your hypothesis. Visualization techniques are usually applied.
#   
# To get introduced to your newly created dataset
introduce(z)
# To visualize the table above (with some light analysis)
plot_intro(z)
# Data
#If you want to quickly visualize the structure of all
plot_str(z)

#Missing values
# Real-world data is messy, and you can simply use plot_missing function to visualize missing profile for each feature
plot_missing(z)

# Distributions
# Histograms
# To visualize distributions for all continuous features
plot_histogram(z)

# Bar Charts
# To visualize frequency distributions for all discrete features
plot_bar(z)

# QQ Plot
# Quantile-Quantile plot is a way to visualize the deviation from a specific probability distribution. After analyzing these plots, it is often beneficial to apply mathematical transformation (such as log) for models like linear regression. To do so, we can use plot_qq function. By default, it compares with normal distribution.
# 
# Note: The function will take a long time with many observations, so you may choose to specify an appropriate smapled_rows
plot_qq(z)

# Correlation Analysis
# To visualize correlation heatmap for all non-missing features
plot_correlation(z)

# Principal Component Analysis
# While you can always do plot_prcomp(na.omit(final_data)) directly, but PCA works better with cleaner data. To perform and visualize PCA on some selected features
plot_prcomp(z)






# Mini-Project Session 4:
# Regression analysis



#The Pearson product moment correlation seeks to measure the linear association between two variables, x and y on a standardized scale ranging
#The correlation of x and y is a covariance that has been standardized by the standard deviations of x and y. This yields a scale-insensitive measure of the linear association of x and y.
library(dplyr)

# correlation between user_rating and rating_count_tot
#correlation between rating_count_tot and rating count on the latest version
to_correlate_1 <- z %>% dplyr::select(rating_count_tot,rating_count_ver)

cor(to_correlate_1)

#correlation between user_rating and user_rating on the lastest version
to_correlate_2 <- z %>% dplyr::select(user_rating,user_rating_ver)

cor(to_correlate_2)





##Testing a bivariate association
#Recall that the significance of correlations are computed on n???2 degrees of freedom.

cor.test(z$rating_count_tot, z$rating_count_ver)

cor.test(z$user_rating, z$user_rating_ver)

library(ggplot2);
library(ggfortify);
#visualizing the association
ggplot(to_correlate_2, aes(x=user_rating, y=user_rating_ver)) + geom_jitter(width=0.1,color="tomato") + stat_smooth(method="lm", se=FALSE)


# Keeping the correlations for further analysis.Here, we store all details of the bivariate correlation test as an R object ctest
ctest <- cor.test(z$rating_count_tot, z$rating_count_ver)
#Let's check the details of ctest
str(ctest)
#we can check specific attributes 
ctest$p.value



#We use a different correlation method (e.g., Spearman) using the method argument
#Total count of rating vs Total count of rating on the latest version
cor(to_correlate_1, method = "spearman")
#overall user rating vs user rating on the latest version
cor(to_correlate_2, method = "spearman")



#Single-predictor (simple) regression
#The standard linear regression model is implemented by the lm function in R. The #lm function uses ordinary least squares (OLS) which estimates the parameter by #minimizing the squared residuals.
#In simple regression, we are interested in a relationship of the form:
#Y=B0+B1X
#where Y is the dependent variable (criterion) and X is the predictor (covariate). #The intercept is represented by B0 and the slope for the X predictor by B1.

#overall user rating vs user rating on the latest version
library(ggplot2)
ggplot(z, aes(x=user_rating, y=user_rating_ver)) + 
  geom_point(color='lightgreen', size = 3) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='black', size=1.2) +
  labs(x="user rating ", y="user rating on the latest version")

#Total count of rating vs Total count of rating on the latest version
ggplot(z, aes(x=rating_count_ver, y=rating_count_tot)) + 
  geom_point(color='lightblue', size = 3) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='black', size=1.2) +
  labs(x="Total count of rating on the latest version", y="Total count of rating")


#Total count of rating vs Total count of rating on the latest version
# In R regression models, we use the ~ operator to denote 'regressed on'. It's not 
#especially intuitive, but we say the criterion is regressed on the predictor.
lm_count <- lm(rating_count_tot ~ rating_count_ver, data=z)
summary(lm_count)

#Regression diagnostics using plot() function from R library
plot(lm_count)

# The ggfortify package also provides an autoplot function that gives similar diagnostics within a handy ggplot-based graph.
library(ggfortify)
autoplot(lm_count)

#overall user rating vs user rating on the latest version
# In R regression models, we use the ~ operator to denote 'regressed on'. It's not 
#especially intuitive, but we say the criterion is regressed on the predictor.
lm_rating <- lm(user_rating ~ user_rating_ver, data=z)
summary(lm_rating)

#Regression diagnostics using plot() function from R library
plot(lm_rating)

# The ggfortify package also provides an autoplot function that gives similar diagnostics within a handy ggplot-based graph.
library(ggfortify)
autoplot(lm_rating)






# Mini-Project Session 5:
# Data Visualization using ggplot2



#here we are creating a new category for Apps which are Free or Paid
#Let's check a quick summary about free and paid apps
summary(z$price==0)
z$is_free = z$price == 0
#is_free column is created contain values as TRUE or FALSE based on the App whose is zero or not
z$is_free = ifelse(z$is_free==TRUE,"Free","Paid")
#In column is_free we have assigned value "Free" to all the TRUE and "Paid" to all FALSE

#More than 50% of our apps data are recorded for free applications, the rest is paid

summary(z)

library(ggplot2)

# define a mapping (using the aesthetic (aes) function), by selecting the variables to be plotted and specifying how to present them in the graph, e.g. as x/y positions or characteristics such as size, shape, color, etc.
# we have used fill in aes function to categorize each track_name in dataset based on the prime_genre category 
ggplot(z, mapping = aes(x = "track_name", fill = prime_genre)) + geom_bar() +
  labs(title="Frequncy of Apps based on the category",
       x="Categories",y="Count")
#From the above plot, we see that the top app category in our data is Games
# Creating a new variable is_game to seperate General apps and Games.
z$is_game <- z$prime_genre == 'Games'
z$is_game <- ifelse(z$is_game == TRUE, "Game", "General App")

summary(z$is_game)

# Showing frequency of apps in our data set based on app content category.
ggplot(z, mapping = aes(x = cont_rating,fill="lightsalmon")) + geom_bar() +
  labs (title="Frequncy of Apps based on the content type",
        x="Content Type",y="Count") 
#Most of our data focus on applications that are for +4 ages


# Showing frequency of apps in our data set based on app content category
z = subset(z,z$user_rating>0)
#We excluded 0 rating from our apps to better investigate the dataset
ggplot(z, mapping = aes(x = user_rating)) + geom_bar(fill="deepskyblue3")+
  labs (title="Frequncy of Apps based on the user rating",
        x="User Rating",y="Count") 
#Most of apps are rated 4.5 and very few are rated as average 5


# A plotting point showing the relation between price of an app 
# and it's mean user rating
#facet_wrap as is_game column
ggplot(z, mapping = aes(x = user_rating, y = price)) +
  geom_point(color="darkorchid",) +
  labs (x="Average User Rating", y="Price",
        title="Relation between price of an app and user rating.") +
  theme_light() +
  facet_wrap(facets = vars(is_game))
#Not so strong relation between price and user rating of app
#As we see there are some outer points 
#in general apps for 4-5 ratings, which fairly indicates that 
#user rating/quality for a general app may be the reason for increasing the price



# A plot line showing the relation between number of languages supported 
#by an app and it's quality/user rating
#stat_summary_bin operates on binned x or y, we have used for y(i.e lang.num) to find the mean of languages supported by easc apps
#fun in stat_summary_bin should take numeric vector and return single number
ggplot(z,mapping = aes(x=user_rating)) +
  stat_summary_bin(aes(y=lang.num),geom="line",fun = "mean",color="lightblue4",size=2) +
  labs (x="Average User Rating", y="Number of language supported",
        title="The relation between number of languages supported 
        by an app and it's quality/user rating") +
  theme_minimal()
summary(z$lang.num)

#Yes, number of lanuages supported by an app may affect on it's total rating
#75% of our data set have 10 language supported or less



# a boxlot plot for content rating as a categorical value and price for an app.
ggplot(aes(x=cont_rating,y=price), data =subset(z,z$price>0)) +
  ylim(0,25) +
  geom_boxplot(fill="palegreen",color = "palegreen4") +
  theme_minimal()
summary.factor(z$cont_rating,stats=TRUE)

# 25% of 12+ apps have prices less than $5, 
# 9+ and 17+ are nearly the same in regarding of the price.
# 4+ apps are nearly less than $3.
# There are some ouliers for apps high than $7



#Which category has the most highgest rating?

ggplot(aes(x=prime_genre,fill=prime_genre), data = z) +
  stat_summary_bin(geom = "bar",aes(y = user_rating),fun = "mean")+
  coord_flip() +
  labs (x="Categories", y="Average of user rating",
        title="Does mean user ratings  depend on app category?") + 
  facet_wrap(~is_free) + theme_minimal()

# * Games,Music and Health&Fitness have the highest average rating in free apps.
# * Cataloges and Shopping have the highest average rating in paid apps.
# 
# However,
# 
# We see that books in paid apps have high mean rating, however very less in 
# free apps. The same in Catalogs.




#Which category has the highgest total number of ratings?

ggplot(aes(x=prime_genre,fill=prime_genre), data = z) +
  stat_summary_bin(geom = "bar",aes(y = rating_count_tot),fun = "mean")+
  coord_flip() +
  labs (x="Categories", y="Average of total number of user rating",
        title="Does mean total number of user ratings  depend on app category?") + 
  facet_wrap(~is_free) + theme_minimal()


# Amazing result begins to appear!
# Users don't give feedback or rating to paid apps. However they do in free apps!


# A histogram plot answering  question of 
# Which category is more expensive than other apps?

ggplot(aes(x=prime_genre,fill=prime_genre), data = z) +
  stat_summary_bin(geom = "bar",aes(y = price),fun = "mean")+
  coord_flip() +
  labs (x="Categories", y="Average Price",
        title="Which category is more expensive than other apps?") + 
  theme_minimal()
# Medical category are the most expensive ones! 
#   Yes the category field can affect on the price of the app!!
#   Shopping apps have very less price than other apps


# A histogram showing the summary of size_mb (MEAN) and user rating.
ggplot(z,mapping = aes(x=user_rating)) +
  stat_summary_bin(geom = "bar",aes(y = size_mb),fun = "mean",fill = "orange3")+
  scale_x_continuous(breaks = seq(1.5,4.5,.5),limits = c(1,5)) +
  theme_minimal()


#yes! When app size increases, average user rating for the app increases as well.



q2 = ggplot(aes(x=size_mb,y=price), data = subset(z,z$price<25)) +
  geom_boxplot(aes(color = factor(user_rating))) + coord_flip() + labs (y="Price", x="Size (MB)" )

q1 = ggplot(aes(x=rating_count_tot/10000,y=price), data = subset(z,z$price<25)) +
  geom_boxplot(aes(color = factor(user_rating))) + coord_flip() + labs (y="Price", x="mod 10000 of total count of rating" )
library(gridExtra)
grid.arrange(q1,q2,ncol=2)


# We see that prices under $10 their sizes vary, their user rating stay good while sizes increase.
# Overall, average user rating remain good though the number of total ratings increase.

# line plot showing the relation between current version rating 
# and average user rating for an app.

ggplot(z,mapping = aes(x=user_rating_ver,y=user_rating)) +
  stat_summary_bin(geom = "line",aes(y = user_rating),fun = "mean")+
  geom_smooth() +
  labs (x="User rating of recent version", y="Total average user rating",
        title="Does the current version is always have more rating than 
        the total overall rating?")

cor(z$user_rating_ver,z$user_rating)


# Correlation between current version user rating and total 
# overall user rating is 0.6 which leads to a strong positive 
# correlation between them as the above plot shows.
# 




# Relation between price and different category colored by user rating to define
# which category has more price in comparing of user rating.
ggplot(z,mapping = aes(x=prime_genre,y=price)) +
  geom_line(aes(color = factor(user_rating))) + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(y="Price",x="Categories")

# Utilities are expensive apps which have low user ratings


#the end


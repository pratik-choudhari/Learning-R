#Reading the BKB.csv dataset
data <- read.csv("C:/Users/sansk/Desktop/Sanskriti/College/Data Analytics/BKB.csv")
data.head(n=3000)
#Basic summary of the data
str(data)
summary(data)
#Monthly Salary:
mean(data$Monthly.Salary)
median(data$Monthly.Salary)
var(data$Monthly.Salary)
sd(data$Monthly.Salary)
#Box and whisker plot to find out outliers
#Monthly Salary
quantile(data$Monthly.Salary)
boxplot(data$Monthly.Salary,horizontal = TRUE)
#EMI Affordable:
quantile(data$EMI.Affordable)
boxplot(data$EMI.Affordable,horizontal = TRUE)
#Alot of outliers are found in the columns Monthly Salary and EMI Affordabale.
#Loan Amount Requested
quantile(data$Loan.Amount.Requested)
boxplot(data$Loan.Amount.Requested,horizontal = TRUE)
#Loan Amount Requested doesn't have many outliers.

#Visualize the Loan Amount attribute
#Histogram for loan amount :
#A histogram is used for visualizing the loan amount as it gives a clear idea about the frequency of people who fall under each loan amount tab.These tabs or range of values are called bins.
hist(data$Loan.Amount.Requested)
#Changing bin size of histogram plot:
hist(data$Loan.Amount.Requested,breaks=10)
#Density plot for Loan Amount Requested:
plot(density(data$Loan.Amount.Requested))
#We can see that the density plot peeks as 800000 ,550000 which gives us an idea about the modality of the data.Its a multi-modal attribute.
#Dot plots or frequency polygons can also be used to represent the data.

#Visualize the distribution of Accomodation Type attribute (PieChart)
mytable <- table(data$Accomodation.Type)
lbls <- paste(names(mytable), mytable, sep=" ")
pie(mytable, labels = lbls,
    main="Pie Chart of Accomadation Type\n (with sample sizes)") 
#From the pie chart we get an idea that the number of people staying in rented houses in quite more than that in owned,family or company houses. A very small portion of the sample lives in houses provided by the company.
#Visualize the Gender attribute
barplot(table(data$Gender))
#From the bar plot above we can see that the number of men who apply for loans is far more than women loan appliers.
#Variation of Monthly Salaries with respect to EMI amount
plot(x = data$Monthly.Salary,y = data$EMI.Affordable,
     xlab = "Monthly Salary",
     ylab = "Emi Affordable",
     main = "Salary vs EMI"
)

#We need to consider that both the tables Monthly Salary and EMI have outliers which might make it difficult to judge the plot. However we notice the trend that Salary seems to not have a crucial affect on the affordable EMI for an individual.
MQ<-quantile(data$Monthly.Salary)
EQ<-quantile(data$EMI.Affordable)
E_iqr<-IQR(data$EMI.Affordable)
up <-  EQ[2]+1.5*E_iqr # Upper Range  
low<- EQ[1]-1.5*E_iqr # Lower Range
eliminated<- subset(data, data$EMI.Affordable > (low) & data$EMI.Affordable < (up))
plot(x = eliminated$Monthly.Salary,y = eliminated$EMI.Affordable,
     xlab = "Monthly Salary",
     ylab = "Emi Affordable",
     main = "Salary vs EMI"
)

#On removing the outliers wrt to EMI we notice very little relation between EMI and 
#Salary.It can be seen that with increase in salary the chances of having a higher EMI is more likely.Mostly all have EMI's at the lower range even if they have high salaries.

#Scatterplot matrix:

pairs(~Monthly.Salary+EMI.Affordable+Loan.Amount.Requested, data=data)
#With the above plot we can compare multiple variables and infer there corresponding relationships.

#A trend can be noticed where Loan amount requested increses as monthly salary increases.This seems justified as a higher salary ensures security when incase of being able to payback the loan.

#General descriptive statistics(mean, median, mode, range, standard deviation etc.) of the Monthly Salary attribute
mean(data$Monthly.Salary)
median(data$Monthly.Salary)
range(data$Monthly.Salary)
var(data$Monthly.Salary)
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
mode<-getmode(data$Monthly.Salary)
mode
#Mean monthly salary for females***
mean(data$Monthly.Salary[data$Gender=="Female"])
#Median for monthly salaries for Men
male<- subset(data, data$Gender=="Male")
median(male$Monthly.Salary)
#Mean monthly salaries, grouped by the Gender attribute. Using dplyr package.Median and range of Monthly salary grouped by the Gender attribute

#Summary statistic calculated using the aggregate function.

aggregate(data$Monthly.Salary,by=list(Gender=data$Gender),FUN=mean)
aggregate(data$Monthly.Salary,by=list(Gender=data$Gender),FUN=median)
aggregate(data$Monthly.Salary,by=list(Gender=data$Gender),FUN=min)
aggregate(data$Monthly.Salary,by=list(Gender=data$Gender),FUN=max)

#Skewness and kurtosis for the Monthly Salary attribute***

library("moments")
skewness(data$Monthly.Salary)
kurtosis(data$Monthly.Salary)
hist(data$Monthly.Salary)

#We can clearly infer that Monthly Salary is Positively skewed.As the kurtosis>3 we can conclude that the attribute is leptokurtic.
#Value of correlation between Loan amount and Down payment***

cor(data$Loan.Amount.Requested,data$Down.Payment)

#As the correlation coefficient is so close to 0 we can say that the Loan Amount and Down Payment have little to no correlation.

#Correlogram between the various attributes***

#Getting all numerical columns.

num_data<-data[-c(1:5)]
num_data

c<-cor(num_data)
library('corrplot')
corrplot(c,method="color")

#Perform PCA on the data

pc<-prcomp(num_data,center = TRUE,scale. = TRUE)
pcomp<-predict(pc)%>%round(2)
summary(pc)
head(pcomp)

#Hence we reduce the dimensions from 8 to 6 as they are enough to represent the most important features of the data.

#Visualization depicting PCA (ggbiplot)
library(ggbiplot)
ggbiplot(pc,groups=data$Gender,ellipse=TRUE)



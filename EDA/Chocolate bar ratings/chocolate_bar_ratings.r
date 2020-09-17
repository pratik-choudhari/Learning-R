#Dataset - Chocolate bar ratings
#Features in dataset:Company_Maker, Bar_Name, REF, Review_Date, Cocoa_Percentage, Company_Location, Rating, Bean_Type, Bean_Origin
#Loading libraries

library(ggplot2) # for data visualization
library(treemapify) # for treemap
library(tidyverse) # for dplyr functionality
library(corrplot) # for correlation plot
library(wordcloud) # for plotting word cloud
library(tm) # for data prep of word cloud

#Reading dataset
chocolate <- read.csv("C:/Users/pratik/Documents/notes/SEMESTER 8/R_lab/09_chocolate_bar_ratings.csv", na.strings=c(""," "," ","NA"), stringsAsFactors = FALSE, encoding = 'UTF-8')
head(chocolate)

#Rename columns for better understanding
names(chocolate)[1:9] <- c("Company_Maker", "Bar_Name","REF", "Review_Date", "Cocoa_Percentage", "Company_Location", "Rating", "Bean_Type", "Bean_Origin")

#check structure of data
str(chocolate)

# plot missing value percentage distribution
missing.values <- chocolate %>%
  tidyr::gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <-
  (missing.values  %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "Status", 
                    values = c('darkgreen', 'red'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")
percentage.plot

#50% of Bean_Type in NA, we exclude this column for now
chocolate = select (chocolate,-c(Bean_Type))

chocolate = chocolate %>% group_by(Company_Maker) %>% fill(Bean_Origin)

#now filter NA rows
chocolate = chocolate[complete.cases(chocolate), ]

#percentage field is a string field we will convert it to numeric value
chocolate$Cocoa_Percentage <- as.numeric(sub("%", "", chocolate$Cocoa_Percentage))
head(chocolate)

#What is the correlation between given features?
chocolate_cor <- data.frame(chocolate$REF, chocolate$Review_Date, chocolate$Cocoa_Percentage, chocolate$Rating)
names(chocolate_cor)[1:4] <- c('REF', 'Review Date', 'Cocoa %', 'Rating')

chocolate_cor <- round(cor(chocolate_cor), 4)

corrplot(chocolate_cor, method = 'circle', type = 'upper', tl.srt = 45)

#REF shows high correlation with Review date that is because of the fact that REF is just a reference number that 
#increases as more and more reviews are added day by day.
#next REF and Rating have high correlation

#Top 10 companies
companies = chocolate %>% group_by(Company_Maker) %>% 
  summarize(count=n(), .groups = 'drop') %>% arrange(desc(count)) %>% top_n(20)

ggplot(companies, aes(x = reorder(Company_Maker, -count), y = count))+
  geom_bar(stat = 'identity', width = 0.7, fill = "#009999", alpha=0.5) +
  labs(x = "Company", y = "Count", title = "Top 10 companies") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#treemap for Bar_Name and Bean_Origin
treemap_data = chocolate %>% group_by(Bar_Name, Bean_Origin) %>% summarise(count=n()) %>% arrange(desc(Bar_Name,count)) 
treemap_data = treemap_data[complete.cases(treemap_data), ]
top_5_origin = chocolate %>% group_by(Bar_Name) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(5)
treemap_data = treemap_data[treemap_data$Bean_Origin %in% pull(top_5_origin,Bar_Name), ]
treemap_data = treemap_data %>% group_by(Bean_Origin) %>% arrange(desc(count)) %>% top_n(5)
ggplot(treemap_data, aes(area = count, fill=count,label = Bar_Name,
                subgroup = Bean_Origin)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)
  
# cocoa percentage by origin
cocoa_percent = chocolate %>% group_by(Bean_Origin) %>% 
  mutate(Stat=mean(Cocoa_Percentage)) %>% arrange(desc(Stat)) %>% select(c('Bean_Origin', 'Stat'))
top_5_origin = chocolate %>% group_by(Bean_Origin) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(10)
cocoa_percent = cocoa_percent[cocoa_percent$Bean_Origin %in% pull(top_5_origin,Bean_Origin), ]
cocoa_percent = cocoa_percent %>% group_by(Bean_Origin) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(10)
ggplot(chocolate[chocolate$Bean_Origin %in% cocoa_percent$Bean_Origin,],aes(x = Bean_Origin, y = Cocoa_Percentage)) +
  geom_boxplot(aes(fill=Bean_Origin)) +
  coord_flip() +
  labs(x='Bean Origin',y='Cocoa percentage', title = "Cocoa percantage for Bean Origin")


#Checking which coutries have more chocolate companies and taking the top 10 in the form of barplot
select_country = chocolate %>% group_by(Company_Location) %>% 
  summarize(count=n(), .groups = 'drop') %>% arrange(desc(count)) %>% top_n(20)


ggplot(select_country, aes(x=reorder(Company_Location, count), y = count) ) +
  geom_segment(size=2,aes(x=reorder(Company_Location, count) ,xend=reorder(Company_Location, count), y=0, yend=count), color="grey") +
  geom_point(size=4, color="#009E73") +
  coord_flip() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Top 10 countries with most chocolate companies")

#Checking which coutries have more chocolate companies and taking the top 10 in the form of word cloud
word_choc <- gsub(" ", "",chocolate$Company_Maker)
corpus = Corpus(VectorSource(word_choc))

wordcloud(corpus, max.words = 60, random.order = TRUE, 
          scale = c(1.5,0.5), rot.per = 0.3, colors = brewer.pal(5, "Set1"))

#Where does the cocoa bean come from?
select_bean <- chocolate %>% group_by(Bean_Origin) %>% summarize(count = n(), .groups = 'drop') %>% arrange(desc(count)) %>% top_n(10)
select_bean

#With a boxplot we're going to see the relation between country and their ratings
country = as.character(select_country$Company_Location)
score_select_country = chocolate %>% filter(Company_Location %in% country) %>% 
  select(Company_Location, Rating) %>% arrange(Company_Location)

p1 = ggplot(score_select_country,aes(x = reorder(Company_Location, Rating, median), y = Rating)) +
  geom_boxplot(aes(fill = Company_Location)) +
  coord_flip() +
  labs(x='Country',y='Rating')

#As we did with countries we're going to see a boxplot with these countries and relate it with the Ratings means 
select_bean <- as.character(select_bean$Bean_Origin)
bean_score <- chocolate %>% filter(Bean_Origin %in% select_bean) %>% select(Bean_Origin, Rating) %>% arrange(Bean_Origin)
p2 = ggplot(bean_score, aes(x = reorder(Bean_Origin, Rating, mean), y = Rating)) +
  geom_boxplot(aes(fill = Bean_Origin)) +
  coord_flip() +
  labs(x = "Bean Origin", y = "Ratings") +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.6))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(p1,p2,cols=2)


# There are 74 chocolate bar without Bean Origin 
# ERROR
#Top 10 countries where the cocoa bean come from
select_bean <- chocolate %>% group_by(Bean_Origin) %>% summarize(count = n(), .groups = 'drop') %>% arrange(desc(count)) %>% top_n(10)
select_bean <- na.omit(select_bean)
ggplot(select_bean, aes(x = reorder(Bean_Origin, -count), y = count))+
  geom_bar(stat = 'identity',width = 0.8, col='black',fill = "#c693ff") +
  labs(x = "Country", y = "Count", title = "Top 10 countries where the cocoa bean come from")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.6))

#We can also verify how these ratings are distributed
ratings_sum <- chocolate %>% group_by(Rating) %>% summarize(count=n(), .groups = 'drop') %>% arrange(desc(count))
p1 = ggplot(data=ratings_sum, aes(x=Rating, y=count, group=1)) +
    geom_line(colour='#f7ca89', size=2)+
    geom_point(colour='#ff8c00', size=2)+
    geom_text(colour='#4d4d4d',
    label=ratings_sum$count, 
    nudge_x = 0.15, nudge_y = 0.25, 
    check_overlap = T) +
    labs(x='',y = "Count") 

# We can se that there are only 2 Chocolate bars with the highest grade and 4 with the lowest
#chocolate[chocolate$Rating == 5.00,]
#chocolate[chocolate$Rating == 1.00,]
p2 = ggplot(chocolate, aes(x=Rating))+
  geom_histogram(aes(y=..density..), color="black", fill="white", bins = 18)+
  geom_density(alpha=0.5, fill="#00b8ff")+
  labs(x="Ratings", y="Density")
multiplot(p1, p2)

#Comparison of Means
#Now we're going to take the top 5 coutries that produces more chocolate and compare their rating distribuition.
usa_rating <- chocolate %>% group_by(Company_Location) %>% filter("U.S.A." %in% Company_Location) %>% select(Rating)
france_rating <- chocolate %>% group_by(Company_Location) %>% filter("France" %in% Company_Location) %>% select(Rating)
canada_rating <- chocolate %>% group_by(Company_Location) %>% filter("Canada" %in% Company_Location) %>% select(Rating)
uk_rating <- chocolate %>% group_by(Company_Location) %>% filter("U.K." %in% Company_Location) %>% select(Rating)
italy_rating <- chocolate %>% group_by(Company_Location) %>% filter("Italy" %in% Company_Location) %>% select(Rating)

top_five <- rbind(usa_rating, uk_rating, france_rating, canada_rating, italy_rating)
top_five <- as.data.frame(top_five)
top_five[,1] <- as.factor(top_five[,1])

#a density plot of ratings for each location
ggplot(top_five, aes(x = Rating)) +
  geom_density(aes(group = Company_Location, fill = Company_Location), alpha = 0.6)+
  labs(x = "Ratings", y = "Density", fill = "Company Location")

#facet of a density plot of ratings for each location
ggplot(top_five, aes(x = Rating)) +
  geom_density(aes(group = Company_Location, fill = Company_Location), alpha = 0.6)+
  labs(x = "Ratings", y = "Density", fill = "Company Location")+
  facet_wrap(~Company_Location)


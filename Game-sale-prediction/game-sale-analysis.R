####
#---
#title: "Game Sale Prediction"
#author: "Savas Turkoglu"
#date: "May 2020"
#---
### instruction   ###

  
## This DAta Science project created for Harvard DAta Science Certification program at  Edx by Savas Turkoglu
  
# We will analysis and visualizate dataset that about video game sales arround the world over years
  
#The video game industry is growing so fast that some believe it will reach over $300 billion by 2025.
#With billions of dollars in profit and over 2.5 billion gamers around the world,
#we can expect video game platforms to continue developing in 2020.
#Besides the consistent and impressive growth of the industry,
#it is interesting to note that there has been a shift in revenue sources 
#in the gaming space lately. The gaming industry used to make most of its 
#money by selling games but today its revenue is coming from a different perspective.

#----- source: Forbes https://www.forbes.com/sites/ilkerkoksal/2019/11/08/video-gaming-industry--its-revenue-shift/#8569649663e5

### data set ###

# I get data from kaggle https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings
# We have more than 16000 row data about video game industry that  contain can  give us an idea about sales such as ->
# platform,genre, publisher, rating, Name, sales by region ( NA_sale, EU_Sales... ) etc..
# names ->"Name"  "Platform" "Year_of_Release" "Genre"  "Publisher" "NA_Sales""EU_Sales" "JP_Sales" "Other_Sales" "Global_Sales"  "Rating
# but there are many missing data in the dataset
# we have to deal with this missing data every columns , even ve can drop some columns if thre are a lot of gaps
# wee'll visualie this data for get some idea about this industry.
# wee'ww try estimate sale performance

### 

##load libraryes
## Loading packages for data exploration, visualization, preprocessing, 
##  keras deep leraning package

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")

if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")


##ml
library(randomForest)
library(kernlab)
library(caret)

# data dealing
library(dplyr)
library(tidyverse) 

# plot
library(data.table)
library(ggplot2)
library(knitr)



knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)


#### DATA OVERVIEW ###
## data from kaggle https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings
#Context
#Motivated by Gregory Smith's web scrape of VGChartz Video Games Sales, this data set simply extends the 
#number of variables with another web scrape from Metacritic. 
#Unfortunately, there are missing observations as Metacritic only covers a subset of the platforms.
#Also, a game may not have all the observations of the additional variables discussed below. Complete cases are ~ 6,900






## load data from external source


url<- 'https://likyapix.com/game-data.csv'

data <- read.csv(url)

# take a look data
head(data)


#names
names(data)


#summary
 summary(data)
 
 
 # chack duplicarion
 duplicated(data) %>%sum()
 
 
 #dimensions
 dim(data)

## DATA CONTENT ## 

#Alongside the fields:
#Name, Platform,
#YearofRelease, 
#Genre, 
#Publisher, 
#NASales,
#EUSales,
#JPSales, 
#OtherSales, 
#Global_Sales, 

#Rating - The ESRB ratings
#Acknowledgements





#check missing mavlue
sapply(data, function(x) sum(is.na(x)))


# check empty values
sapply(data, function(x) sum(x==''))

# as we can see there are  many missing data in columns suc as Critical_count, Critical_Score, User_Score, User_Count ,Developer columns 
# and this  missing datas more than half of dataset and will not give us an idea about dataset
#therefore we will ignore tihs columns during analysis an visualization bu we handle this columns on prediction


data <- data %>% filter( as.numeric(Year_of_Release) < 2019) 



#### ANALYSIS and VISUALIZE DATA ###


###    ---------    Data analysis and visualization   By Platform   ----------  ###

# first we'll look at platform column
# There are several popular video game plarform such as Nintendo, PS, XOne Among video game lovers 
# and there is hard competiton between these companies 
# let's look  up

unique(data$Platform)
# there are more than 20 differen game platform in the dataset

platform_ <- data %>% group_by(Platform) %>% 
  summarize(count       = n(),
            Global_sales = sum(Global_Sales),
            NA_Sales     = sum(NA_Sales),
            EU_Sales    = sum(EU_Sales),
            JP_Sales    = sum(JP_Sales),
            )
platform_ %>% arrange(desc(Global_sales))%>% knitr::kable()

# as expected PlayStation Series at the top, X360 and Nintendo  following 


## top 10 platform

platform_ %>% arrange(desc(Global_sales)) %>% head(10) %>% ggplot(aes(x=Platform, y=count)) + geom_bar(stat = "identity") 

# look up all platform at table

platform_ %>% arrange(desc(Global_sales))%>% knitr::kable()


###    ---------    Data analysis and visualization   By Sale   ----------  ###

game_ <- data %>% group_by(Name) %>% 
             summarize(count       = n(),
            Global_sales = sum(Global_Sales),
            NA_Sales     = sum(NA_Sales),
            EU_Sales    = sum(EU_Sales),
            JP_Sales    = sum(JP_Sales),
  )

 ### ------  global sale by game
 game_global_ <- game_ %>% arrange(desc(Global_sales)) %>% head(10) 
 
 # over view global sales
 game_global_ %>%  knitr::kable()
 
 # top 10 global sale
 game_global_ %>% head(10) %>% ggplot(aes(x=Name, y=count)) + geom_bar(stat = "identity") 
 
 
 ### ------  EU sale by game
 game_eu_ <- game_ %>% arrange(desc(EU_Sales)) %>% head(10) 
 
 # over view global sales
 game_eu_ %>%  knitr::kable()
 
 # top 10 global sale
 game_eu_ %>% head(10) %>% ggplot(aes(x=Name, y=count)) + geom_bar(stat = "identity") 
 
 
 
 
 ### ------  NA sale by game
 game_na_ <- game_ %>% arrange(desc(NA_Sales)) %>% head(10) 
 
 # over view global sales
 game_na_ %>%  knitr::kable()
 
 # top 10 global sale
 game_na_ %>% head(10) %>% ggplot(aes(x=Name, y=count)) + geom_bar(stat = "identity") 
 
 



 ### ------  JP sale by game
 game_jp_ <- game_ %>% arrange(desc(JP_Sales)) %>% head(10) 
 
 # over view global sales
 game_jp_ %>%  knitr::kable()
 
 # top 10 global sale
 game_jp_ %>% head(10) %>% ggplot(aes(x=Name, y=count)) + geom_bar(stat = "identity") 


#Distribution of Global Sales across Genres and Rating


data %>% 
  ggplot(aes(x=Genre,y=log(Global_Sales),col=Rating))+
  geom_boxplot(varwidth=TRUE)+facet_wrap(~Rating)+
  theme(axis.text.x=element_text(angle=90),panel.background = element_rect(fill="black"), panel.grid.major = element_blank() , 
  panel.grid.minor=element_blank())


###    ---------    Data analysis and visualization   By Genre   ----------  ###
             
genre_ <- data %>% group_by(Genre) %>% 
  summarize(count       = n(),
            Global_sales = sum(Global_Sales),
            NA_Sales     = sum(NA_Sales),
            EU_Sales    = sum(EU_Sales),
            JP_Sales    = sum(JP_Sales),
  )

genre_ %>% head()

genre_  %>% arrange(desc(Global_sales)) %>% head(10) %>%  knitr::kable()

genre_ %>% ggplot(aes(x=Genre, y=count)) + geom_bar(stat = "identity") +
xlab("Genre)") + ylab("Game Count") + ggtitle('Sales By Genre')





###    ---------    Data analysis and visualization   By Publisher   ----------  ###

publisher_ <- data %>% group_by(Publisher)%>% filter(!is.na(Publisher)) %>% 
  summarize(count       = n(),
            Global_sales = sum(Global_Sales),
            NA_Sales     = sum(NA_Sales),
            EU_Sales    = sum(EU_Sales),
            JP_Sales    = sum(JP_Sales),
  )

publisher_  %>% arrange(desc(Global_sales))  %>%  knitr::kable()

# top 10 publisher  globaly

publisher__ <- publisher_ %>% arrange(desc(Global_sales))  %>% head(10)

publisher__ %>% head()

publisher__  %>% ggplot(aes(x=as.character(Publisher), y=Global_sales)) + geom_bar(stat = "identity") +
  xlab("Publisher)") + ylab("Global SAle") + ggtitle('Top 10 publisher global sales') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




###    ---------    Data analysis and visualization   By Year   ----------  ###

##  let'look at the change game industry over year

# game sale ovr year
year_ <- data %>% group_by(Year_of_Release) %>% 
  summarize( count       = n(),
             Global_sales = sum(Global_Sales),
             NA_Sales     = sum(NA_Sales),
             EU_Sales    = sum(EU_Sales),
             JP_Sales    = sum(JP_Sales),
  )



year_  %>% ggplot(aes(x=as.numeric(as.character(Year_of_Release)), y=count)) + geom_path() +
  xlab("Year)") + ylab("Game Count") + ggtitle('Game Count per year') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# publisher  change overyear

year_publisher_ <- data %>% group_by(Year_of_Release, Publisher) %>% group_by(Year_of_Release) %>%
  summarize( count       = n() )

year_publisher_  %>% head()

year_publisher_   %>% ggplot(aes(x=Year_of_Release, y=count)) + geom_bar(stat = "identity") +
  xlab("Year)") + ylab("Game Count") + ggtitle('Sales By Yaer') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# sales over year

year_sale_ <- data %>% group_by(Year_of_Release) %>%

  summarize( count       = n(),
             Global_sales = sum(Global_Sales),
             NA_Sales     = sum(NA_Sales),
             EU_Sales    = sum(EU_Sales),
             JP_Sales    = sum(JP_Sales),
  )


#What is the total number of games released every year?


ggplot(data[data$Year_of_Release!="N/A",], aes(x=Year_of_Release, fill=..count..)) +
  geom_bar()+
  scale_color_gradient(low="purple", high="orange")+
  scale_fill_gradient(low="purple", high="orange")+
  labs(title="Number of Games Released every Year", x= "Game", 
       y= "Total Number of Games")+
  geom_text(stat='count',aes(label=..count..), hjust=-0.1,color="darkblue", size=2.5)+
  coord_flip() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))






###  Sales over year by    By Region  




year_sale_ <- data  %>% group_by(Year_of_Release) %>%
  
  summarize( count       = n(),
             Global_sales = sum(Global_Sales),
             NA_Sales     = sum(NA_Sales),
             EU_Sales    = sum(EU_Sales),
             JP_Sales    = sum(JP_Sales),
  )

year_sale_



ggplot(year_sale_, aes(as.numeric(as.character(Year_of_Release)))) + 
  geom_line(aes(y = Global_sales, colour = "Global")) + 
  geom_line(aes(y = NA_Sales, colour = "North America")) +
  geom_line(aes(y = EU_Sales, colour = "Euro")) +
  geom_line(aes(y = JP_Sales, colour = "Japan")) 



###### PREDİCTİON MODELLİNG   ###########

#wee will try predict the sales of future games based on the pattern that can be learned from this data set.
#At the moment, the analysis focuses only on the sales in EURO Sales.

# prepare dat afor modellig

# --- remove missing val in Genre & Name
data <- data %>% filter(!is.na(Genre)&!is.na(Name))



## Train-test spilit



indexes<-createDataPartition(y=data$Global_Sales,p=0.7,list=FALSE)
train_set<-data[indexes,]
test_set<-data[-indexes,]

nrow(train_set)
nrow(test_set)


## lineer regression

ln_fit <- train(EU_Sales~Year_of_Release+Genre+Critic_Score+
                  Critic_Count+User_Score+User_Count+Platform,
                data=train_set,
                 na.action = na.omit,
                method="lm")

test_p_b <- predict(ln_fit,test_set)
ln_rmse<- RMSE(test_p_b, test_set$EU_Sales) # 

ln_rmse


rmse_results <- data_frame(method = "Lineer regression", RMSE =  ln_rmse)


## Sport vector machine
control <- trainControl(method='none')

set.seed(666)
sv_fit <- train(EU_Sales~Year_of_Release+Genre+Critic_Score+
                 Critic_Count+User_Score+User_Count+Platform,
               data=train_set,
               na.action = na.omit,
               trControl=control,
               method="svmLinear" )


test_p_svm<- predict(sv_fit,test_set)

sv_rmse<- RMSE(test_p_svm, test_set$EU_Sales) # 

sv_rmse

rmse_results <- bind_rows(rmse_results, data_frame(method = "Support vectore meachine", RMSE =  sv_rmse))

#10 folds repeat 3 times
control <- trainControl(method='none')
## random forest 

set.seed(666)
#Number randomely variable selected is mtry
mtry <- sqrt(ncol(train_set))
tunegrid <- expand.grid(.mtry=mtry)
rf_fit <- train(EU_Sales~Year_of_Release+Genre+Critic_Score+
                      Critic_Count+User_Score+User_Count+Platform,
                    data=train_set, 
                    method='rf', 
                    na.action = na.omit,
                   
                    tuneGrid=tunegrid, 
                    trControl=control)

rf_prd<- predict(rf_fit,test_set)

rf_rmse<- RMSE(rf_prd, test_set$EU_Sales) 


rf_rmse

rmse_results <- bind_rows(rmse_results, data_frame(method = "Random  forest", RMSE =  rf_rmse))


## check results 
rmse_results %>% knitr::kable()

### Results
#Random forest  models with the best performance.
# performance are actually very close to each other.
#lineer regression  has a slightly worse performance compared to the two models. support vector machne (linear) has the worst performance.

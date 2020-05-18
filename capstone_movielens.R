
##############################
# Author: Savaş Türkoğlu
# Movielens Project for HarvardX Data Science Capstone at Edx
# date: may 2020

########################

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(dplyr)
library(tidyverse) 
library(caret)
library(data.table)
library(ggplot2)
library(knitr)



# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

## I collected edx provided data at my driv
#movies_drive  <- 'https://drive.google.com/open?id=1P0ryeVFLw0ilGtK7elW48lm0ZXvJxc4M'
#ratings_drive   <- 'https://drive.google.com/open?id=1FqpjaayvsVZsTG_PZwAlqpa79CHSk42K'
#local paths
#rt<- readLines('~/R/ml-10m/ml-10M100K/ratings.dat')
#mv<- readLines('~/R/ml-10m/ml-10M100K/movies.dat')
#ratings <- fread(text = gsub("::", "\t",rt ), col.names = c("userId", "movieId", "rating", "timestamp"))
#movies <- str_split_fixed(mv, "\\::", 3)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)



ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


# Validation set will be 10% of MovieLens data
set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm( ratings, movies, test_index, temp, movielens, removed)


# my computer sources was not enough for proccess 10m rows data, therefore i used  dataset partially during development

 #edx <- head(edx,1000000)  #if you get memory error ue this code




######### ----------- prepare data ------------ #############
#### !! Edx gave us prepared data but we need some more preparation !! 

##--First we have to dealing with date --  ##

## Converting date-time to human readable  --

edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")

## get year and month from date

edx$rate_year <- format(edx$date,"%Y")
edx$rate_month <- format(edx$date,"%m")

validation$rate_year <- format(validation$date,"%Y")
validation$rate_month <- format(validation$date,"%m")




## --now wee'll deal with title we have to separate title and release year  -- ##

## + in edx datasets

edx <- edx %>%
  mutate(title = str_trim(title)) %>%
  extract(title, c("title_", "release"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  mutate(release = if_else(str_length(release) > 4,
                           as.integer(str_split(release, "-",
                                                simplify = T)[1]),
                           as.integer(release))
  ) %>%
  mutate(title = if_else(is.na(title_), title, title_)
  )
 edx <- edx %>% select(-title_)

 ## + in validaton datasets
 
 validation <- validation %>%
   mutate(title = str_trim(title)) %>%
   extract(title, c("title_", "release"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
   mutate(release = if_else( str_length(release) > 4,
                             as.integer(str_split(release, "-",
                             simplify = T)[1]),
                             as.integer(release))) %>%
   mutate(title = if_else(is.na(title_), title, title_))
 
 validation <- validation %>% select(-title_)
 



 ##--- we have to  modify the genres vars in the edx & validation dataset by column_separated   --##
 
 
 genre_split_edx  <- edx  %>% separate_rows(genres, sep = "\\|")
 genre_split_valid <- validation  %>% separate_rows(genres, sep = "\\|")

 genres<- genre_split_edx %>%
   group_by(genres) %>%
   summarize(count = n()) 
 
 genres<- genres  %>% arrange(desc(count))
 genres
 
 genre_names <- genres$genres
 



## # remove unnecessary column from datasets
 
 edx <- edx %>% select(-date, -timestamp)


##########------------- DATA OVERVIEW ---------------###############


##  take look to data by date 
edx_year <- edx %>% group_by(rate_year) %>% 
     summarize(total_rate = sum(rating), count = n()) 

 ## sort by total rate
 
 edx_year %>% arrange(desc(total_rate)) %>% knitr::kable()

 ## plot date - raiting count                     
 ggplot(edx_year, aes(y=count, x=rate_year)) + 
     geom_bar(stat = "identity")+
     labs(title = "Rate count by year ", x = "Year", y = "Raiting number")
 


## take look by rate  (1, 1.5, 2)
 edx_by_rating <- edx %>% group_by(rating) %>% 
   summarize(total_rate = sum(rating), count = n()) 
 

 
 ## plot rate - raiting count                     
 ggplot(edx_by_rating, aes(x=rating, y=total_rate)) + 
   geom_bar(stat = "identity")+
   labs(title = "Rate count by ratig ", x = "Rate", y = "Raiting number")
 
 
 
 ## rateing count by release year 
 
 movies_per_year <- edx %>%
   select(movieId, release) %>% 
   group_by(release) %>% 
   summarise(count = n())  %>%
   arrange(release)
 
 # plot
 movies_per_year %>%
   ggplot(aes(x = release, y = count)) +
   geom_line(color="blue")
 


 
 
 ## take look by genre
 edx_by_genre <- genre_split_edx %>% group_by(genres) %>% 
    summarize(total_rate = sum(rating), count = n()) 
 
 
 
 ## plot genre  - raiting count                     
 ggplot(edx_by_genre, aes(y=count, x=genres)) + 
    geom_bar(stat = "identity")+
    labs(title = "Rate count by Genre ", x = "Genre", y = "Raiting number")
 
 
 
 
 ##########-------------PREDICTİON MODELLING---------------###############
 
 # we'll use this RMSE 
 RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2,na.rm = T))
 }
 
 
 ## Dataset’s mean rating is used to predict the same rating for all movies
 mu <- mean(edx$rating)  
 mu
 
 
 ### BASELINE MODEL ###
 baseline_model_rmse <- RMSE(validation$rating,mu)
 ## Test results 
 baseline_model_rmse
 
 ## create table that we vcollect methods and RMSEs 
 
 rmse_results <- data_frame(method = "Baseline model", RMSE =  baseline_model_rmse)
 
 #check
 rmse_results
 
 
 
 ### MOVIE  MODEL ###
 #rating  are different effected from different movies
 
 
 movie_av <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu))
 
 
 #rediction
 
 pred_movie_av <- validation %>% 
    left_join(movie_av, by='movieId') %>%
    mutate(pred = mu + b_i) 
 
 
 # calculate RMSE
 model_movie_av_rmse <- RMSE(validation$rating,pred_movie_av$pred)
 model_movie_av_rmse
 
 # add to table
 rmse_results <- bind_rows(rmse_results,
                           data_frame(method="Movie Effect Model",  
                                      RMSE =  model_movie_av_rmse ))
 # check results
 rmse_results %>% knitr::kable()
 
 
 
 
 
 ### USER AND MOVIE MODEL ###
 
 # different users can interest different movies and this could effect to rating
 
 user_av <- edx %>% 
    left_join(movie_av, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))
 

 # prediction
 pred_movie_user <- validation %>% 
    left_join(movie_av, by='movieId') %>%
    left_join(user_av, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>% .$pred
 
 
 # calculate RMSE
 model_user_movie_rmse <- RMSE(validation$rating,  pred_movie_user)
 
 rmse_results <- bind_rows(rmse_results,
                           data_frame(method="User & Movie Model",  
                                      RMSE = model_user_movie_rmse ))
 rmse_results %>% knitr::kable()

 
 
 ###  REGULARİZATİON USİNG USER AND MOVİE  ####
 
 # We have learned on Edx  Machine learning course Regularization is very usefull method  to imprıve our results we'll use this method 
 #using user and movie
 
 
 
 
 lambdas <- seq(0, 10, 0.25)
 
 # this loop will take time
 rmses <- sapply(lambdas, function(l){
    
    mu <- mean(edx$rating)
    
    b_i <- edx %>% 
       group_by(movieId) %>%
       summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- edx %>% 
       left_join(b_i, by="movieId") %>%
       group_by(userId) %>%
       summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    pred_rating <- validation %>% 
       left_join(b_i, by = "movieId") %>%
       left_join(b_u, by = "userId") %>%
       mutate(pred = mu + b_i + b_u) %>%
       .$pred
    
    return(RMSE(validation$rating,pred_rating))
 })
 
 # now we'ill get lowest value from rmses
 lambda <- lambdas[which.min(rmses)]
 lambda
 
 
 # now we can compute regularized estimates of b_i using this lowest lambda
 mov_av_reg <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())
 
 # also we can compute regularized estimates of b_u using  this lowest lambda
 user_av_reg <- edx %>% 
    left_join(mov_av_reg, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), n_u = n())
 
 
 # Prediction
 pred_ratings_reg <- validation %>% 
    left_join(mov_av_reg, by='movieId') %>%
    left_join(user_av_reg, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%     .$pred
 
 
 #  get rmse
 reg_rmse <- RMSE(validation$rating,pred_ratings_reg)
 
 # add to table
 rmse_results <- bind_rows(rmse_results, data_frame(method="Regulariz User & Movie Model",  RMSE = reg_rmse ))
 
 #check results
 rmse_results %>% knitr::kable()
 
 
 ### REGULARİZATİON USER , MOVIE , YEAR, GENRE ###
 
 
 lambdas <- seq(0, 10, 0.5)
 # this part will take time
 rmses <- sapply(lambdas, function(l){
    
    mu <- mean(edx$rating)
    
    b_i <-  genre_split_edx %>% 
       group_by(movieId) %>%
       summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <-  genre_split_edx %>% 
       left_join(b_i, by="movieId") %>%
       group_by(userId) %>%
       summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    b_y <-  genre_split_edx %>%
       left_join(b_i, by='movieId') %>%
       left_join(b_u, by='userId') %>%
       group_by(release) %>%
       summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda), n_y = n())
    
    b_g <-  genre_split_edx %>%
       left_join(b_i, by='movieId') %>%
       left_join(b_u, by='userId') %>%
       left_join(b_y, by = 'release') %>%
       group_by(genres) %>%
       summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda), n_g = n())
    
    # prediction
    reg_predict <- genre_split_valid %>% 
       left_join(b_i, by='movieId') %>%
       left_join(b_u, by='userId') %>% 
       left_join(b_y, by = 'release') %>%
       left_join(b_g, by = 'genres') %>%
       mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
       .$pred
    
    return(RMSE(genre_split_valid$rating,reg_predict))
 })

 
 # now we'ill get lowest value from rmses
 lambda_all <- lambdas[which.min(rmses)]
 lambda_all
 
 
 # now we can copmute rmse with tihs lowet lambda value
 
 
 movie_reg_av_ <- genre_split_edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda_all), n_i = n())
 
 user_reg_av_ <- genre_split_edx %>% 
    left_join(movie_reg_av_, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_all), n_u = n())
 
 year_reg_av_ <- genre_split_edx %>%
    left_join(movie_reg_av_, by='movieId') %>%
    left_join(user_reg_av_, by='userId') %>%
    group_by(release) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda_all), n_y = n())
 
 genre_reg_av_ <- genre_split_edx %>%
    left_join(movie_reg_av_, by='movieId') %>%
    left_join(user_reg_av_, by='userId') %>%
    left_join(year_reg_av_, by = 'release') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda_all), n_g = n())
 
   
 
 
 #prediction
 req_pred <- genre_split_valid %>% 
    left_join(movie_reg_av_, by='movieId') %>% 
    left_join(user_reg_av_, by='userId') %>%
    left_join(year_reg_av_, by = 'release') %>%
    left_join(genre_reg_av_, by = 'genres') %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
    .$pred
 
 #compute rmse
 model_4_rmse <- RMSE(genre_split_valid$rating,req_pred)
 
 
 rmse_results <- bind_rows(rmse_results,
                           data_frame(method="Regulariztion User, Year,  Movie, Genre  Model",  
                                      RMSE = model_4_rmse ))
 
 ## check results 
 rmse_results %>% knitr::kable()
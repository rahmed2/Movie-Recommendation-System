if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

############################################################################
## naive approach (with mean only)
mu<- mean(edx$rating)
naive_rmse<-RMSE(validation$rating,mu) 
rmse_results <- data.frame(method = "Just the average", RMSE = naive_rmse)
############################################################################
## with movie effects
movie_avgs<- edx %>%
  group_by(movieId) %>%
  summarize(b_i= mean(rating-mu))
# Histogram of movie effects estimates
movie_hist<-movie_avgs %>%
ggplot(aes(b_i)) + 
  geom_histogram(bins = 10, color = "black")

# Prediction with movie effects
predicted_ratings_m <- validation %>%
  left_join(movie_avgs, by ="movieId") %>%
  mutate(pred=mu+b_i) %>%
  pull(pred)
# Resulting RMSE
rmse_m<-RMSE(predicted_ratings_m, validation$rating) 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = rmse_m))
####################################################################
## with movie + user effect
# Histogram of user effects estimates
user_hist<-edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

user_avgs<- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u=mean(rating-mu-b_i))
# Prediction with with movie + user effect
predicted_ratings_um <- validation %>%
  left_join(movie_avgs, by ="movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred=mu+b_i+b_u) %>%
  pull(pred)
# Resulting RMSE
rmse_um<-RMSE(predicted_ratings_um, validation$rating) 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effect Model",  
                                     RMSE = rmse_um))
######################################################################
#how rating changes with genres
genre_hist<-edx %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_g)) + 
  geom_histogram(bins = 30, color = "black")
#################################################################
## with movie + user + genre effect
genre_avgs <- edx %>%
  left_join(movie_avgs, by ="movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g=mean(rating-mu-b_i-b_u))
# Prediction with movie + user + genre effect
predicted_ratings_new <- validation %>%
  left_join(movie_avgs, by ="movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(pred=mu+b_i+b_u+b_g) %>%
  pull(pred)
rmse_umg<-RMSE(predicted_ratings_new, validation$rating)  
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Genre Effect Model",  
                                     RMSE = rmse_umg))
#################################################################
### With regularization
# Finding optimal lambda
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_g <- edx %>%
    left_join(b_i, by ="movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g=sum(rating-mu-b_i-b_u)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

lambda_plot<-qplot(lambdas, rmses)
l <- lambdas[which.min(rmses)]
l

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

b_g <- edx %>%
  left_join(b_i, by ="movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g=sum(rating-mu-b_i-b_u)/(n()+l))
# Prediction using movie + user + genre effect with regularization
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
# Resulting RMSE
rmse_reg<-RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User + Genre Effect Model",  
                                     RMSE = rmse_reg))






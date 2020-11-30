library(tidyverse)
library(caret)
library(knitr)
library(ggplot2)
library(data.table)

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

# Define RMSE function
RMSE <- function(predicted_rating, actual_rating){
  sqrt(mean((predicted_rating - actual_rating)^2))
}

dim(edx)

head(edx)

c(unique_users = length(unique(edx$userId)), unique_movies = length(unique(edx$movieId)))

#Graph for number of rating distribution
user_avg_n <- edx %>%
  group_by(userId) %>%
  summarize(n = n()) %>%
  summarize(avg = mean(n)) %>%
  pull(avg)

edx %>%
  group_by(userId) %>%
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() +
  geom_vline(xintercept = user_avg_n, color = "red") +
  annotate("text", x = 500, y = 5500, label = "Average number of ratings", color = "Red") +
  xlab("Number of ratings (log 10 scale)") +
  ylab("Number of users") +
  ggtitle("User - Number of Ratings Distribution") +
  theme_clean()

movie_avg_rating <- edx %>%
  group_by(movieId) %>%
  summarize(n = n()) %>%
  summarize(avg = mean(n)) %>%
  pull(avg)

edx %>%
  group_by(movieId) %>%
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(xintercept = movie_avg_rating, color = "red") +
  annotate("text", x = 5000, y = 1250, label = "Average number of ratings", color = "red") +
  scale_x_log10() +
  xlab("Number of Ratings (log 10 scale)") +
  ylab("Number of Movies") +
  ggtitle("Movies - Number of Ratings Distribution") +
  theme_clean()

#Graph for distribution of ratings

edx %>%
  group_by(rating) %>%
  summarize(n = n()) %>%
  ggplot(aes(rating, n)) +
  geom_line() +
  geom_vline(xintercept = 3.512465, color = "red") +
  annotate("text", x = 4.25, y = 100, label = "Average Rating", color = "Red") +
  xlab("Ratings") +
  ylab("Number of ratings") +
  theme_clean() +
  ggtitle("Rating Distribution")

edx %>%
  group_by(movieId) %>%
  summarize(rating = mean(rating), n = n(), movieId = first(movieId)) %>%
  ggplot(aes(n, rating)) +
  scale_x_sqrt() +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  xlab("Number of Ratings (square root scale)") +
  ylab("Ratings") +
  theme_clean()  +
  ggtitle("Rating Distribution for Individual Movies")

edx %>%
  group_by(userId) %>%
  summarize(rating = mean(rating), n = n(), userId = first(userId)) %>%
  ggplot(aes(n, rating)) +
  scale_x_sqrt() +
  geom_point(alpha = 0.3) +
  xlab("Number of Ratings (square root scale)") +
  ylab("Ratings") +
  theme_clean() +
  ggtitle("Rating Distribution for Individual Users")

#Graph for standard deviation of ratings

edx %>%
  group_by(movieId) %>%
  summarize(movieId = first(movieId), n = n(), stan_dev = sd(rating)) %>%
  mutate(group = cut(n,
                     breaks = c(0, mean(n), Inf),
                     labels = c("< than 843", "> than 843"))) %>%
  ggplot(aes(stan_dev, fill = group)) +
  geom_density(alpha = 0.5) +
  xlab("Standard Deviation") +
  ylab("Count") +
  theme_clean() +
  ggtitle("Standard Deviation of Rating in Movies",
          subtitle = "Average number of ratings = 843")

edx %>%
  group_by(userId) %>%
  summarize(userId = first(userId), n = n(), stan_dev = sd(rating)) %>%
  mutate(group = cut(n,
                     breaks = c(0, mean(n), Inf),
                     labels = c("< 129", "> 129"))) %>%
  ggplot(aes(stan_dev, fill = group)) +
  geom_density(alpha = 0.5) +
  xlab("Standard Deviation") +
  ylab("Density") +
  theme_clean() +
  ggtitle("Standard Deviation of Ratings for Users",
          subtitle = "Average number of ratings = 129")

#Build test and train set

set.seed(5, sample.kind = "Rounding")

#Create 10% of data partition for testing
ind <- createDataPartition(edx$rating, times = 1, p = 0.1, list = FALSE)

train_set <- edx[-ind,]
temp_test <- edx[ind,]

#Validate to make sure entries in test set are also in train set.

test_set <- temp_test %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Add the removed rows back into the training set
removed <- anti_join(temp_test, test_set)
train_set <- rbind(train_set, removed)

rm(temp_test)

#Define Mu and baseline for test

mu <- mean(train_set$rating)

result <- data.frame(Method = "Project Goal", 
                     RMSE = 0.86490)
result <- bind_rows(result,
                    data.frame(Method = "Baseline",
                               RMSE = RMSE(mu, test_set$rating)))

#Graph for b_i

b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

b_i %>%
  ggplot(aes(b_i)) +
  geom_histogram(color = "white") +
  ggtitle("Movie Bias") +
  theme_clean()

#applying b_i to model
pred_b_i <- mu + test_set %>%
  left_join(b_i, by = "movieId") %>%
  .$b_i

result <- bind_rows(result,
                    data.frame(Method = "With Movie Bias",
                               RMSE = RMSE(pred_b_i, test_set$rating)))
result

#applying b_i and b_u to model
b_u <- train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu))

b_u %>%
  ggplot(aes(b_u)) +
  geom_histogram(color = "white", fill = "blue") +
  theme_clean() +
  ggtitle("User Bias")

pred_bui <- test_set %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_i, by = "movieId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

result <- bind_rows(result,
                    data.frame(Method = "With Movie and User bias",
                               RMSE = RMSE(pred_bui, test_set$rating)))
result

#Regularization

titles <- train_set %>%
  select(movieId, title) %>%
  distinct()

b_i %>%
  left_join(titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  group_by(title) %>%
  summarize(count = n()) %>%
  slice(1:10)
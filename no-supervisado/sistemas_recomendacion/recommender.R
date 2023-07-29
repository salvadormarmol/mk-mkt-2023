library(dplyr)
library(ggplot2)
library(knitr)
library(recommenderlab)

data(MovieLense)
MovieLense

class(MovieLense)

slotNames(MovieLense)
class(MovieLense@data)

head(names(colCounts(MovieLense)))

vector_ratings <- as.vector(MovieLense@data)
kable(table(vector_ratings), caption="Rating frequency")

vector_ratings = vector_ratings[vector_ratings != 0]
hist(vector_ratings, main="Histogram of Ratings", xlab="Rating Value")

ratings = MovieLense[rowCounts(MovieLense) > 50, colCounts(MovieLense) > 100]
dim(ratings)

ratings.n = normalize(ratings)
ratings.n.vec = as.vector(ratings.n@data)
ratings.n.vec = ratings.n.vec[ratings.n.vec != 0]
hist(ratings.n.vec, main="Histogram of Normalized Ratings", xlab="Rating")

percent_train = 0.8
#min(rowCounts(ratings.n))
items_to_keep = 15        # items to use for each user
rating_threshold = 3      # good rating implies >=3
n_eval = 1                # number of times to run eval

eval_sets = evaluationScheme(data = ratings, method = "split",
                             train = percent_train, given = items_to_keep,
                             goodRating = rating_threshold, k = n_eval)
eval_sets

eval_recommender = Recommender(data = getData(eval_sets, "train"),
                               method = "ALS", parameter = NULL)
items_to_recommend = 10
eval_prediction = predict(object = eval_recommender,
                          newdata = getData(eval_sets, "known"),
                          n = items_to_recommend,
                          type = "ratings")
eval_accuracy = calcPredictionAccuracy(x = eval_prediction,
                                       data = getData(eval_sets, "unknown"),
                                       byUser = TRUE)
head(eval_accuracy)

pred <- predict(object = eval_recommender,
                newdata = getData(eval_sets, "known"),
                n = items_to_recommend,
                type = "topNList")

as(pred, "list")[c(1:3)]


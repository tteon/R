# soucre ; https://www.r-bloggers.com/trump-vs-clinton-interpretable-text-classifier/ 

install.packages('xgboost')
install.packages('text2vec')

library(readr)
library(lime)
library(xgboost) # the classifier
library(caret)
library(dplyr)
library(tibble)
library(text2vec)
library(qdapRegex) # removes urls from text

tweets <- read_csv("D:/DATASETS/201_427_compressed_tweets.csv/tweets.csv")
dim(tweets)

head(tweets)

# Quick glimpse on the class balance, which looks very good, BTW.

table(tweets$handle)

'''
Finally, lets clean the data a little: select only tweets text and author,
change column names to something more readable and remove URLs from text.
'''

all_tweets <- tweets %>% 
  rename(author = handle) %>% 
  select(author, text) %>% 
  mutate(text = qdapRegex::rm_url(text)) %>% #removes URLs from text
  na.omit()

head(as.data.frame(all_tweets))

'''
OK, once were happy with the data, its time to split it into train and test set
- caret package, as always, does a great job here.
'''

set.seed(1234)
trainIndex <- createDataPartition(all_tweets$author, p = .8, 
                                  list = FALSE, 
                                  times = 1)
# `i` must have one dimension, not 2. -> tibble converts to dataframe format

str(trainIndex)
all_tweets <- as.data.frame(all_tweets)
train_tweets <- all_tweets[trainIndex,]
test_tweets <- all_tweets[-trainIndex,]

str(train_tweets)
str(test_tweets)

'''
In order to build the model, we need to tokenize our data and transform it to Document Term Matrices.
'''


# tokenizes text data and creates DTM

get_matrix <- function(text) {
  it <- itoken(text, progressbar = FALSE)
  create_dtm(it, vectorizer = hash_vectorizer())
}

dtm_train= get_matrix(train_tweets$text)
dtm_test = get_matrix(test_tweets$text)

'''
Now, time for the model. I used Extreme Gradient Boosting tree model for classification,
which usually gives very good result, even with standard parameters:
'''

param <- list(max_depth = 7, 
              eta = 0.1, 
              objective = "binary:logistic", 
              eval_metric = "error", 
              nthread = 1)

set.seed(1234)

xgb_model <- xgb.train(
  param, 
  xgb.DMatrix(dtm_train, label = train_tweets$author == "realDonaldTrump"),
  nrounds = 50,
  verbose=0
)
### official 

xgb_model2 <- xgb.train(list(max_depth = 7, eta = 0.1, objective = "binary:logistic",
                            eval_metric = "error", nthread = 1),
                       xgb.DMatrix(dtm_train, label = train_tweets$author == "realDonaldTrump"),
                       nrounds = 50)
# How does the model do? Would you trust it based on accuracy alone?

# We use a threshold of 0.5
predictions <- predict(xgb_model, dtm_test) > 0.5
test_labels <- test_tweets$author == "realDonaldTrump"

# Accuracy
print(mean(predictions == test_labels))


'''
The model was accurate in 84% of cases, which is quite impressive,
given how little we did in terms of data pre-processing and feature engineering.
Now, what clues did the classifier pick on? Are they reasonable?

In order to understand this, i will run lime_explainer() only on correctly predicted instances 
while ignoring misclassified observations. I will pick first 5 observations for interpretation.
'''

# select only correct predictions
predictions_tb = predictions %>% as_tibble() %>% 
  rename_(predict_label = names(.)[1]) %>%
  tibble::rownames_to_column()

correct_pred = test_tweets %>%
  tibble::rownames_to_column() %>% 
  mutate(test_label = author == "realDonaldTrump") %>%
  left_join(predictions_tb) %>%
  filter(test_label == predict_label) %>% 
  pull(text) %>% 
  head(5)

'''
Now, this is important. BEfore we run explainer(), we need to detach dplyr package,
which is a bit of a pain when you go back and forth between different chunks of code that require or hate dplyr, but there you go.
There are worse things that can happen to your code, I suppose.
'''

detach("package:dplyr", unload=TRUE) # explainer will not run with dplyr in the workspace

explainer <- lime(correct_pred, model = xgb_model, 
                  preprocess = get_matrix)

corr_explanation <- lime::explain(correct_pred, explainer, n_labels = 1, 
                                  n_features = 6, cols = 2, verbose = 0)

plot_features(corr_explanation)

###

str(train_tweets)
str(test_tweets)
###

sentences2 <- head(test_tweets[test_tweets$author == "realDonaldTrump", "text"], 1)
explainer2 <- lime(train_tweets$author, xgb_model2, get_matrix)

corr_explanation <- lime::explain(correct_pred, explainer2, n_labels = 1, 
                                  n_features = 6, cols = 2, verbose = 0)























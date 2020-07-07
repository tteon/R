# source ; https://kkulma.github.io/2017-11-07-automated_machine_learning_in_cancer_detection/

# Loading cancer data

library(mlbench)

data("BreastCancer")
str(BreastCancer)

# data pre-processing

library(dplyr)
install.packages('janitor')
install.packages("h2o")
library(janitor)
library(h2o)




bc_edited =
  BreastCancer %>%
  janitor::clean_names() %>% # changes dots in colnames to underscores
  mutate_if(is.factor, factor, ordered = F) %>% # changes all columns to unordered factors 
  select(class, everything(), -id) %>%
  na.omit()

str(bc_edited)

h2o.init() # initalizes Java Virutal Machine (JVM)


h2o.no_progress() #Trun off output of progress bars

# and split the data into training, validation and testing datasets.

bc_h2o <- as.h2o(bc_edited)

split_h2o <- h2o.splitFrame(bc_h2o, c(0.7, 0.15), seed = 13 ) # splits data into random 70% / 15% 15% chunks

split_h2o

train_h2o <- h2o.assign(split_h2o[[1]], "train") # 70%
valid_h2o <- h2o.assign(split_h2o[[2]], "valid") # 15%
test_h2o <- h2o.assign(split_h2o[[3]], "test") # 15%

'''
Finally, we can now use the famous h2o.automl() function and set the model up : 
set the target, feature names, training and validation sets, as well as how long we wnat the algorithm to run for
(for this you can use either max_runtime_secs argument, like I did here, or max_models, see h2o.automl() )
'''

# Sets target and feature names for h2o
y <- 'class'
x <- setdiff(names(train_h2o), y)

# Run the automated machine learning 
models_h2o <- h2o.automl(
  x = x, # predictors
  y = y, # labels
  training_frame    = train_h2o, # training set
  leaderboard_frame = valid_h2o, # validation set
  max_runtime_secs  = 60 # run-time can be increased/decreased according to your needs
)

# leaderboard

lb <- models_h2o@leaderboard
lb

'''
Finally, you can use the Leader to predict labels of the testing set:
'''

automl_leader <- models_h2o@leader

h20_pred <- h2o.predict(automl_leader, test_h2o)

library(tibble)

test_performance <- test_h2o %>%
  tibble::as_tibble() %>%
  select(class) %>%
  tibble::add_column(prediction = as.vector(h20_pred$predict)) %>%
  mutate(correct = ifelse(class == prediction, 'correct', 'wrong')) %>%
  mutate_if(is.character, as.factor)

str(test_h2o)
head(test_h2o)
str(test_performance)
head(test_performance)

confusion_matrix <- test_performance %>% select(-correct) %>% table()
confusion_matrix

# more detailed performance of the model.

# Performance analysis 
tn <- confusion_matrix[1]
tp <- confusion_matrix[4]
fp <- confusion_matrix[3]
fn <- confusion_matrix[2]

accuracy <- ( tp + tn ) / ( tp + tn + fp + fn)
missclassification_rate <- 1 - accuracy
recall <- tp / (tp + fn)
precision <- tp / (tp + fp)
null_error_rate <- tn / (tp + tn + fp + fn)

library(purrr)

tibble(
  accuracy,
  missclassification_rate,
  recall,
  precision,
  null_error_rate
) %>%
  purrr::transpose()

'''
Given that it is cancer data I did be happier if recall was higher, but longer running times
would improve this. Now, can we understand the nn - the king of 'black-box'learners - that produced those predictions?
'''

# MAKING OBSCURE LESS OBSCURE USING library(lime)

test_h2o_df = as.data.frame(test_h2o)

test_h2o_2 = test_h2o_df %>%
  as.data.frame() %>%
  mutate(sample_id = rownames(test_h2o_df))

test_correct <- test_performance %>%
  mutate(sample_id = rownames(test_performance)) %>%
  filter(correct == 'correct') %>%
  inner_join(test_h2o_2) %>%
  select(-c(prediction, correct, sample_id))

test_wrong <- test_performance %>%
  mutate(sample_id = rownames(test_performance)) %>%
  filter(correct == 'wrong') %>%
  inner_join(test_h2o_2) %>%
  select(-c(prediction, correct, sample_id))

# Now, let's perpare lime so that it works with H@O model correctly. No one will explain this step better than MATT (the author せせせせ)

'''
The lime package implements LIME in R. One thing to note is that it is not setup out-of-the-box to work with h2o. The good news is with a few functions we can get everything working properly.
model_type ; Used to tell lime what type of model we are dealing with. It could be classification , regression , survival, etc
predict_model ; used to allow lime to perform predictions that its algorithm can interpret.
'''
install.packages('lime')
library(lime)

# Setup lime::model_type() function for h2o
model_type.H2OBinomialModel <- function(x, ...) {
  return("classification")
}

# Setup lime::predict_model() function for h2o
predict_model.H2OBinomialModel <- function(x, newdata, type, ...) {
  pred <- h2o.predict(x, as.h2o(newdata))
  # return probs
  return(as.data.frame(pred[,-1]))
}

# WE're nearly there! Let's just define our explainer

predict_model(x = automl_leader, newdata = as.data.frame(test_h2o[,-1]), type = 'raw' ) %>%
  tibble::as_tibble()

explainer <- lime::lime(
  as.data.frame(train_h2o[,-1]),
  model = automl_leader,
  bin_continous = F
)
'''
Lets start explaining ! All our work so far lead to this feature importance plots(not the same for each case!).
The green bars mean that the feature supports predicted label, and the red bars contradict it. Lets have a look at the correctly predicted labels:
'''
explanation_corr <- explain(
  test_correct[1:9, -1],
  explainer = explainer,
  n_labels = 1,
  n_features = 5,
  kernel_width = 0.5
)

plot_features(explanation_corr , ncol = 3)

'''
You can see that smaller and more regular cells with low values of bare nuclei
(bare_nuclei) correctly indicate benign cells, whereas big, irregular cells with higher values of clump thickness(cl_thickness)
support malignant label. It all makes sense.
'''

# bengin
# malignant 

# What about the misclassified labels?

explanation_wrong <- explain(
  test_wrong[1:6, -1],
  explainer = explainer,
  n_labels = 1,
  n_features = 5,
  kernel_width = 0.5
)

plot_features(explanation_wrong)

'''
And heres where the true power of lime package is ; understanding what made model missclasify labels.
All the wrong cases were predicted to be bengin while they were malignant, why? it looks like they were mainly small and quite regular cells,
altough some malignant characterestics were still present(e.g. higher values of bare nuclei and clump thickness).
What a great improvement of our understanding of how the [black box] model works and why it makes mistakes.
Even though it doesnt product [fixed] feature importance plot, it allows you to make a good educated guess of which features matter. 
'''










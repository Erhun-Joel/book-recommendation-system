# Loading neccessary libraries
library(tidyverse)
library(tidymodels)
library(tidytext)
library(SnowballC)

# Also load text recipe library for later
library(textrecipes)

# Loading data
# Loading data to use
emotion.data <-
read_delim(
  "https://raw.githubusercontent.com/Erhun-Joel/book-recommendation-system/refs/heads/main/Data/Kaggle%20Emotions%20Datasets/train.txt",
  col_names = c("text", "emotion")
)
emotion.data

# We already know that random forest perform wonderfully. Lets see two things:
# 1. Simpler model set ups can perform similarily
# 2. The effects of regularization on the random forest itself

# Lets do some preprocessing at once. This ensures our resampling process goes faster. For our final model, we will redefine the recipe
emotion.model.data <-
emotion.data %>%
  mutate(emotion_truth = as.factor(emotion)) %>%
  select(emotion_truth) %>%
  bind_cols(
    emotion.data %>%
      mutate(
        sentence = row_number(),
        text = str_replace_all(text, " don t ", " do not "),
        text = str_replace_all(text, " i m ", "  i am "),
        text = str_replace_all(text, " can t ", "  can not "),
        text = str_replace_all(text, " it s ", "  it is "),
        str_replace_all(text, "'", "")
      ) %>%
      unnest_tokens(word, text) %>%
      mutate(word = wordStem(word)) %>%
      filter(word != "") %>%
      count(sentence, word) %>%
      bind_tf_idf(term = word, document = sentence, n = n) %>%
      select(sentence, word, tf_idf) %>%
      pivot_wider(names_from = word, values_from = tf_idf, values_fill = 0) %>%
      select(-sentence)
  )
emotion.model.data

# Data budgeting
set.seed(123)
emotion.split <- emotion.model.data %>%
  initial_split(strata = emotion)
emotion.split

# Get out training and testing data
emotion.train <- training(emotion.split)
emotion.test <- testing(emotion.split)
emotion.test

# Create folds for testing
# v is reduce to 5 due to extremely long computation time
emotion.folds <- vfold_cv(emotion.train, v = 5)
emotion.folds

# Define grid for penalty and mixture terms of logistic models
logistic.grid <- grid_regular(penalty(range = c(0, 2), trans = NULL), mixture(), levels = c(10, 4))
logistic.grid

# Define spec involve normalization
logistic.recipe <- recipe(emotion_truth ~ ., data = emotion.train) %>%
  step_zv(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())
logistic.recipe

# Declare spec
logistic.spec <- multinom_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")
logistic.spec

# Combine everything and train model
logistic.resamples <- workflow(
  preprocessor = logistic.recipe,
  spec = logistic.spec
) %>%
  tune_grid(
    resamples = emotion.folds,
    grid = logistic.grid,
    metrics = metric_set(roc_auc, accuracy),
    control = control_resamples(verbose = TRUE)
  )
logistic.resamples

# Check metrics
logistic.resamples %>%
  collect_metrics

# Plot results
autoplot(logistic.resamples)
# It seems the models with the strongest performance had smaller regularization applied to them
# While logistic model performance is strong, it doesn't compare to that observed initially via random forests

# Tuning random forest due to processor speed constraints remains outside the scope of this project
# Lets remake and save our best performing model

# Create spec
rf.spec <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf.spec

# Get out training split again
set.seed(126)
emotion.split <-
emotion.data %>%
  mutate(emotion = as.factor(emotion)) %>%
  rename(emotion_truth = emotion) %>%
  initial_split(strata = emotion_truth)
emotion.split

# Get out trainning data
split.train <- training(emotion.split)
split.train

# Draft preprocessing steps
preprocessor <- recipe(emotion_truth ~ text, data = split.train) %>%
  step_tokenize(text) %>%
  step_stem(text) %>%
  step_tokenfilter(text, max_tokens = 2000) %>%
  step_tfidf(text) %>%
  step_normalize(all_numeric_predictors())
preprocessor

# Define workflow
final.workflow <- workflow(
  preprocessor = preprocessor,
  spec = rf.spec
)
final.workflow

# Train and test data
set.seed(421)
rf.result <- last_fit(
  final.workflow,
  emotion.split,
  control = control_last_fit(verbose = TRUE)
)
rf.result

# Check metrics
rf.result %>%
  collect_metrics

# Plot out the confusion matrix using a heat map
rf.result %>%
  collect_predictions() %>%
  conf_mat(truth = emotion_truth, estimate = .pred_class) %>%
  autoplot(type = "heatmap")
# We see that populations with great number of samples are predicted quite well.
# Overall, an roc_auc of over 97% is quite good

# Save workflow
#rf.result %>% extract_workflow() %>% saveRDS(file = "")

# NOW TO RUN THIS MODEL ON THE DESCRIPTIONS OF BOOKS IN THE DATA FROM GUTENBERG API
# This would generate probalilities used to match text to book to be read

# Read book library into local environment
book.library <-
read.csv(
  "https://raw.githubusercontent.com/Erhun-Joel/book-recommendation-system/refs/heads/main/Gutendex%20API%20response/book_library_first.csv"
) %>%
  as_tibble() %>%
  bind_rows(
    read.csv(
      "https://raw.githubusercontent.com/Erhun-Joel/book-recommendation-system/refs/heads/main/Gutendex%20API%20response/book_library_second.csv"
    ) %>%
      as_tibble
  ) %>%
  select(-X)
book.library

# Read rds model back from github
rf.fit <-
readRDS(gzcon(url("https://raw.githubusercontent.com/Erhun-Joel/book-recommendation-system/refs/heads/main/rf_fit.rds")))
rf.fit

# Get list of probabilities using rf.result's model
probabilities <-
book.library %>%
  select(id) %>%
  bind_cols(
    rf.fit %>%
      predict(
        book.library %>%
          select(description) %>%
          rename(text = description),
        type = "prob"
      )
  )
probabilities

# Two things needs to be rectified for this dataset to be usable
# 1. Occassional existence of two titles for a single book
# 2. The extreme class imbalance of anger from this model


# The extreme class imbalance would be handled in the actual implementation by penalizing a certain value

#write.csv(probabilities, "---probabilities.csv")
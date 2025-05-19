# Loading required libraries
library(tidyverse)
library(tidymodels)
library(tidytext)
library(SnowballC)
library(widyr)

# Loading data to use
emotion.data <-
read_delim(
  "https://raw.githubusercontent.com/Erhun-Joel/book-recommedation-system/refs/heads/main/Data/Kaggle%20Emotions%20Datasets/train.txt",
  col_names = c("text", "emotion")
)
emotion.data

# Check number of emotions present
emotion.data %>%
  count(emotion)

# Beginning text analysis
emotion.count <- emotion.data %>%
  unnest_tokens(word, text) %>%
  mutate(
    word = str_replace_all(word, "'", ""),
    word = wordStem(word)
  ) %>%
  count(emotion, word, sort = TRUE)
emotion.count

# Computing term frequencies
emotion.tfidf <-
emotion.data %>%
  mutate(
    id = 1:dim(emotion.data)[1]
  ) %>%
  unnest_tokens(word, text) %>%
  count(id, emotion, word) %>%
  bind_tf_idf(term = word, document = id, n = n)
emotion.tfidf

# Checking out words with highest separation grades
emotion.tfidf %>%
  arrange(-tf_idf)

# Setting out training sections of the data
set.seed(457)
train = sample(1:dim(emotion.data)[1], size = dim(emotion.data)[1]*0.8)
head(train)

# Defining pre-processing steps
emotion.recipe <-
recipe(emotion ~ text, data = emotion.data[train,]) %>%
  step_tokenize(text) %>%
  step_stem(text) %>%
  step_tokenfilter(text, max_tokens = 3000) %>%
  step_tfidf(text) %>%
  step_normalize(all_numeric_predictors())
emotion.recipe

# Check out list of tokens being used
selected.tokens <-
emotion.recipe %>%
  prep %>%
  juice %>%
  select(-emotion) %>%
  colnames() %>%
  str_remove("tfidf_text_")
selected.tokens

# Setting out resamples
set.seed(231)
emotion.folds <- vfold_cv(emotion.data[train,])
emotion.folds

# Try random forest
rf.spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")
rf.spec

# Now fit to resamples
doParallel::registerDoParallel()
set.seed(124)
rf.results <- workflow(
  preprocessor = emotion.recipe,
  spec = rf.spec
) %>%
  fit_resamples(
    emotion.workflow,
    resamples = emotion.folds,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )
rf.results

# Check results
rf.results %>%
  collect_metrics()
# !!!

# See out confusion matrics
conf_mat_resampled(rf.results, tidy = FALSE)
# Seems too good to be true

# Lets get the random forest fit and test!

# Getting test numerics
data.rows <- 1:dim(emotion.data)[1]
length(data.rows)

test <- data.rows[!data.rows %in% train]
test

# Making fit
first.results <-
workflow(
  preprocessor = emotion.recipe,
  spec = rf.spec
) %>%
  last_fit(
    make_splits(
      x = list(
        analysis = train,
        assessment = test
      ),
      data = emotion.data
    )
  )
first.results

# Check out the result
first.results %>%
  collect_metrics

# Extract workflow
bundled.workflow <-
first.results %>%
  extract_workflow()
bundled.workflow

# Testing on actual data
bundled.workflow %>%
  predict(tibble(text = c("I had a good day today", "I did not have a good day today")))
# We see that the model does not understand context
# Instead of using a more complicated model, lets work on the features to improve it

test.results <-
bundled.workflow %>%
  predict(emotion.data[-train,]) %>%
  bind_cols(predict(bundled.workflow, emotion.data[-train,], type = "prob")) %>%
  bind_cols(
    emotion.data[-train,] %>%
      select(emotion) %>%
      mutate(emotion = as.factor(emotion))
  )
test.results

# Create custom metrics function
my.metric <- metric_set(roc_auc, sensitivity, specificity, accuracy)
my.metric

# Check out metrics
test.results %>%
  my.metric(
    truth = emotion,
    estimate = .pred_class,
    .pred_anger:.pred_surprise
  )
# Impressive for first try.


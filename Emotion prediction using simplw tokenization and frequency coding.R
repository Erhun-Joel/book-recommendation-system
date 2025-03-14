# Loading required libraries
library(tidyverse)

# Loading data to use
emotion.data <-
read_delim(
  file = "C:/Users/Erhun/Documents/Data Analysis/Projects/Reccomendation System/Data/Kaggle Emotions Datasets/train.txt",
  delim = ";",
  col_names = FALSE
)
emotion.data

# Changing column names
colnames(emotion.data) = c("text", "emotion")
emotion.data

# Check number of emotions present
emotion.data %>%
  count(emotion)

# Begining text analysis
library(tidytext)
emotion.data %>%
  mutate(
    id = 1:dim(emotion.data)[1]
  ) %>%
  unnest_tokens(word, text) %>%
  count(id, emotion, word) %>%
  bind_tf_idf(term = word, document = id, n = n)

# Loading modeling libraries
library(tidymodels)
library(textrecipes)

# Defining preprocessing steps
emotion.recipe <-
recipe(emotion ~ text, data = emotion.data) %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = 2000) %>%
  step_tfidf(text) %>%
  step_normalize(all_numeric_predictors())
emotion.recipe

# Setting out training sections of the data
set.seed(457)
train = sample(1:dim(emotion.data)[1], size = dim(emotion.data)[1]*0.8)
head(train)

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

# Also apply smote to balance out unbalanced classes
library(themis)

# Redefining recipe
emotion.recipe <-
recipe(emotion ~ text, data = emotion.data) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tokenfilter(text, max_tokens = 2000) %>%
  step_tfidf(text) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_smote(emotion)
emotion.recipe

# Getting out final results

# Getting test numerics
data.rows = 1:dim(emotion.data)[1]
length(data.rows)

test = data.rows[!data.rows %in% train]
test

# Making final fit results
final.fit.results <-
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
final.fit.results

# Check out the result
final.fit.results %>%
  collect_metrics

# Extract workflow
bundled.workflow <-
final.fit.results %>%
  extract_workflow()
bundled.workflow

# Testing on actual data
bundled.workflow %>%
  predict(tibble(text = c("I had a good day today", "I did not have a good day today")))
# We see that the model does not understand context. Instead of using a more complicated model, lets work on the features to improve it
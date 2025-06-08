# Loading libraries
library(tidyverse)
library(tidytext)
library(SnowballC)
library(widyr)

# Loading data from github repo
emotion.data <-
read_delim(
  "https://raw.githubusercontent.com/Erhun-Joel/book-recommendation-system/refs/heads/main/Data/Kaggle%20Emotions%20Datasets/train.txt",
  col_names = c("text", "emotion")
)
emotion.data

# Two changes where to be made in this variation of finding word embeddings
# 1. Word stemming
# 2. Increase the number of dimensions used

emotions.tokens <-
emotion.data %>%
  mutate(
    sentence = row_number(),
    text = str_replace_all(text, " don t ", " do not "),
    text = str_replace_all(text, " i m ", "  i am "),
    text = str_replace_all(text, " can t ", "  can not "),
    text = str_replace_all(text, " it s ", "  it is "),
    sentence = row_number()
  ) %>%
  select(sentence, emotion, text) %>%
  unnest_tokens(word, text) %>%
  mutate(word = wordStem(word)) %>%
  filter(word != "")
emotions.tokens

# Calculate Pointwise Mutual Information for every pair of words
# Note: Our context remains each sentence
emotions.pmi <- emotions.tokens %>%
  pairwise_pmi(item = word, feature = sentence, sort = TRUE)
emotions.pmi

# Let us also use counts as compared to pmi
emotions.counts <- emotions.tokens %>%
  pairwise_count(item = word, feature = sentence, sort = TRUE)
emotions.counts

# From now on, we will perform every set of operations on these two columns

# Create matrix for easier operations
emotions.pmi.matrix <-
emotions.pmi %>%
  pivot_wider(names_from = item2, values_from = pmi, values_fill = 0) %>%
  select(-item1) %>%
  as.matrix()
emotions.pmi.matrix[1:10, 1:10]

emotions.counts.matrix <-
emotions.counts %>%
  pivot_wider(names_from = item2, values_from = n, values_fill = 0) %>%
  select(-item1) %>%
  as.matrix()
emotions.counts.matrix[1:10, 1:10]

# Rename the columns with respective token
rownames(emotions.pmi.matrix) <- emotions.pmi %>%
  pivot_wider(names_from = item2, values_from = pmi, values_fill = 0) %>% pull(item1)

rownames(emotions.counts.matrix) <- emotions.counts %>%
  pivot_wider(names_from = item2, values_from = n, values_fill = 0) %>% pull(item1)

# Now lets perform Singular Value Decomposition extracting 500 dimensions with the highest variance
emotions.pmi.eigen <- RSpectra::eigs(t(emotions.pmi.matrix) %*% emotions.pmi.matrix, k = 500)
emotions.counts.eigen <- RSpectra::eigs(t(emotions.counts.matrix) %*% emotions.counts.matrix, k = 500)
emotions.pmi.eigen$vectors[1:6, 1:6]

# These contain list of eigen vectors which serve as the dimensions of the newly rotated space

# Lets name the vectors columns accordingly
colnames(emotions.pmi.eigen$vectors) <- paste0("dim_", 1:500)
colnames(emotions.counts.eigen$vectors) <- paste0("dim_", 1:500)

# Lets label the rows as corresponding tokens
rownames(emotions.pmi.eigen$vectors) <- emotions.pmi %>%
  pivot_wider(names_from = item2, values_from = pmi, values_fill = 0) %>%
  pull(item1)

rownames(emotions.counts.eigen$vectors) <- emotions.counts %>%
  pivot_wider(names_from = item2, values_from = n, values_fill = 0) %>%
  pull(item1)
emotions.pmi.eigen$vectors[1:6, 1:6]

# Lets see the relationship between vectors in these vectors
# We use a function pervious constructed in the initial word embedding part of this project with minor adjustments due to having raw svd
cosine.similarity <- function(data, token){

  data.matx = data

  # Get token matrix
  token.matx = matrix(data.matx[rownames(data.matx) == token,], nrow = 1)[rep(1, nrow(data.matx)),]

  # Perform matrix operation
  result = rowSums(data.matx * token.matx) / (sqrt(rowSums(data.matx ^ 2)) * (sqrt(rowSums(token.matx ^ 2))))

  output = tibble(
    word = names(result),
    cosine = result
  )

  return(output)

}
cosine.similarity(emotions.pmi.eigen$vectors, token = wordStem("pleased")) %>%
  arrange(-cosine)
# Not too entusiastic with the result as given word stumps don't look to be similar in any way.
# Perhaps the dataset is too small
# Still, lets make a prediction to see if it works

# Lets start with counts of tokens with both embedding matrixes projected to it.
token.counts.matrix <-
emotions.tokens %>%
  count(sentence, word) %>%
  cast_dfm(document = sentence, term = word, value = n)
token.counts.matrix

# Computing model inputs
pmi.model.inputs <- token.counts.matrix %*% emotions.pmi.eigen$vectors
pmi.model.inputs[1:6, 1:6]

counts.model.inputs <- token.counts.matrix %*% emotions.counts.eigen$vectors
counts.model.inputs

# These are the projections of our token matrix on the 500 D spaces we constructed

# We continue on with counts.model.input
# Load tidymodels library
library(tidymodels)

# Create split for resampling
set.seed(145)
emotion.split <-
emotion.data %>%
  select(emotion) %>%
  mutate(emotion = as.factor(emotion)) %>%
  bind_cols(
    counts.model.inputs %>%
      as.matrix %>%
      as_tibble
  ) %>%
  initial_split(strata = emotion)
emotion.split

# Get out data
emotion.train <- training(emotion.split)
emotion.test <- testing(emotion.split)
emotion.train

# Set out random forest spec
rf.spec <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")
rf.spec

# Create folds of training data
set.seed(134)
emotion.folds <- vfold_cv(emotion.train, strata = emotion)
emotion.folds

# Train resamples
set.seed(189)
rf.resamples <-
workflow() %>%
  add_formula(emotion ~ .) %>%
  add_model(rf.spec) %>%
  fit_resamples(
    resamples = emotion.folds,
    control = control_resamples(save_pred = TRUE, verbose = TRUE),
    metrics = metric_set(roc_auc, accuracy, sensitivity, specificity)
  )
rf.resamples

# Check out performance
rf.resamples %>%
  collect_metrics()
# As suspected.

# In conclusion, this method is not appropraite for this dataset.
# Most possibly due to limited amount of tokens to appropraitely differentiate
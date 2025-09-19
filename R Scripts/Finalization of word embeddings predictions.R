# Loading required libraries
library(tidyverse)
library(tidytext)
library(SnowballC)

# Load required data for data modelling
emotions.train <- readLines("https://github.com/Erhun-Joel/book-recommendation-system/raw/refs/heads/main/Data/Kaggle%20Emotions%20Datasets/train.txt") %>%
  as_tibble() %>%
  separate(col = "value", into = c("text", "emotion"), sep = ";")
emotions.test <- readLines("https://github.com/Erhun-Joel/book-recommendation-system/raw/refs/heads/main/Data/Kaggle%20Emotions%20Datasets/test.txt") %>%
  as_tibble() %>%
  separate(col = "value", into = c("text", "emotion"), sep = ";")
str(emotions.test)

# --- Creating word embeddings using Singular Value Decomposition ---

# Creating tfidf values for all word documents
emotions.tfidf <-
emotions.train %>%
  mutate(
    sentence = row_number()
  ) %>%
  select(sentence, everything()) %>%
  unnest_tokens(output = word, input = text) %>%
  mutate(word = wordStem(word)) %>%
  group_by(sentence) %>%
  count(word) %>%
  filter(word != "") %>%
  bind_tf_idf(term = word, document = sentence, n = n) %>%
  select(sentence, word, tf_idf) %>%
  cast_sparse(row = sentence, column = word, value = tf_idf)
emotions.tfidf[1:7, 1:7]

# Breaking tfidf values down using SVD
tfidf.svd.results <-
irlba::irlba(emotions.tfidf, nv = 500)

# Getting documment embeddings from matrix factorization
document.embedding <-
emotions.train["emotion"] %>%
  bind_cols(
    tfidf.svd.results$u %*% diag(tfidf.svd.results$d) %>%
      as_tibble()
  )
document.embedding

# Lets pause and use this to check out the validity of the dimensions gotten by using a multinomial model as a base model

# Loading modeling libraries
library(tidymodels)

# Creating folds
set.seed(1223)
multinomial.folds <- vfold_cv(document.embedding, strata = emotion)
multinomial.folds

# Getting results from fitting simple model to document embedding
base.results <-
workflow(
  preprocessor = recipe(emotion ~ ., data = document.embedding),
  spec = multinom_reg(engine = "glmnet", penalty = 0.0000000001)
) %>%
  fit_resamples(
    resamples = multinomial.folds,
    control = control_resamples(verbose = TRUE, save_pred = TRUE),
    metrics = metric_set(accuracy, roc_auc)
  )
base.results

# Checking out results
base.results %>%
  collect_metrics()

# Check out folds confusion matrix
base.results %>%
  collect_predictions() %>%
  filter(id == "Fold01") %>%
  conf_mat(truth = emotion, estimate = .pred_class)
# Overall not bad
# This embeddings allow for quicker computation of otherwise complex mathematical operations

# Lets now apply a neural network to this using the keras3 library
library(keras3)

# Construct dense neural network
neural.network <- keras_model_sequential() %>%
  layer_dense(units = 1000, activation = "relu", input_shape = 500) %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 6, activation = "softmax")
neural.network

# Compile for fitting
neural.network %>%
  compile(
    optimizer = "adam",
    metrics = c("accuracy", "AUC"),
    loss = "categorical_crossentropy"
  )

# Prepare the data for training
input.matrix <-
document.embedding %>%
  select(-emotion) %>%
  as.matrix()

output.matrix <-
document.embedding %>%
  select(emotion) %>%
  fastDummies::dummy_cols() %>%
  select(-emotion) %>%
  as.matrix()

neural.result <-
neural.network %>%
  fit(
    x = input.matrix,
    y = output.matrix,
    batch_size = 500,
    epochs = 20
  )
neural.result

# Check neural network result on training data
neural.network %>%
  predict(input.matrix) %>%
  as.tibble() %>%
  rename(
    emotion_anger = V1,
    emotion_fear = V2,
    emotion_joy = V3,
    emotion_love = V4,
    emotion_sadness = V5,
    emotion_surprise = V6
  ) %>%
  mutate(
    instance = row_number()
  ) %>%
  pivot_longer(cols = emotion_anger:emotion_surprise) %>%
  group_by(instance) %>%
  top_n(n = 1, wt = value) %>%
  ungroup() %>%
  mutate(
    name = str_remove(name, "emotion_")
  ) %>%
  select(.pred_class = name) %>%
  bind_cols(document.embedding["emotion"]) %>%
  bind_cols(
    neural.network %>%
      predict(input.matrix) %>%
      as.tibble() %>%
      rename(anger = V1,
        fear = V2,
        joy = V3,
        love = V4,
        sadness = V5,
        surprise = V6)
  ) %>%
  mutate_if(is.character, as.factor) %>%
  metrics(truth = emotion, estimate = .pred_class, anger:surprise)
# It does exceptionally well on training data. In fact, it seems it does too well. Lets check its performance on testing data.

# Firstly, get list of words available in the svd dimension
words.available <- colnames(emotions.tfidf)
words.available[1:10]

# Firstly, project test data in the dimensional space set out by svd
emotions.test.tfidf <-
emotions.test %>%
  mutate(
    sentence = row_number()
  ) %>%
  select(sentence, everything()) %>%
  unnest_tokens(word, text) %>%
  filter(
    word != "",
    word %in% words.available
  ) %>%
  group_by(sentence) %>%
  count(word) %>%
  ungroup() %>%
  bind_tf_idf(term = word, document = sentence, n = n)
emotions.test.tfidf

# Now add null words that the svd computation expects
test.tfidf <-
emotions.test.tfidf %>%
  select(sentence, word, tf_idf) %>%
  add_row(
    tibble(
      sentence = 1,
      word = colnames(emotions.tfidf)[!colnames(emotions.tfidf) %in% unique(emotions.test.tfidf$word)],
      tf_idf = 0
    )
  ) %>%
  cast_sparse(row = sentence, column = word, value = tf_idf)
test.tfidf

# Therefore, test emotions embeddings are as follows:
test.embeddings <-
(test.tfidf[, colnames(emotions.tfidf)] %*% tfidf.svd.results$v %*% diag(1 / tfidf.svd.results$d)) %*% diag(tfidf.svd.results$d)
dim(test.embeddings)

# Predict with this using the neural network
neural.network %>%
  predict(test.embeddings) %>%
  as.tibble() %>%
  rename(
    anger = V1,
    fear = V2,
    joy = V3,
    love = V4,
    sadness = V5,
    surprise = V6
  ) %>%
  mutate(
    instance = row_number()
  ) %>%
  pivot_longer(cols = anger:surprise) %>%
  group_by(instance) %>%
  top_n(n = 1, wt = value) %>%
  ungroup() %>%
  select(.pred_class = name) %>%
  bind_cols(emotions.test["emotion"]) %>%
  mutate_if(is.character, as.factor) %>%
  bind_cols(
    neural.network %>%
      predict(test.embeddings) %>%
      as.tibble() %>%
      rename(anger = V1,
        fear = V2,
        joy = V3,
        love = V4,
        sadness = V5,
        surprise = V6)
  ) %>%
  metrics(truth = emotion, estimate = .pred_class, anger:surprise)

# How does our base model test?
base.model <-
workflow(
  preprocessor = recipe(emotion ~ ., data = document.embedding),
  spec = multinom_reg(engine = "glmnet", penalty = 0.0000000001)
) %>%
  fit(
    data = document.embedding
  )
base.model

predict(base.model, test.embeddings %>% as.matrix %>% as.tibble()) %>%
  bind_cols(mutate_if(emotions.test["emotion"], is.character, as.factor)) %>%
  bind_cols(
    predict(base.model, test.embeddings %>% as.matrix %>% as.tibble(), type = "prob")
  ) %>%
  metrics(
    truth = emotion,
    estimate = .pred_class,
    .pred_anger:.pred_surprise
  )

# The test results are quite disappointing regardless of the model type used.
# Perhaps its time to look at the strength of the word embedding generated using SVD

# First pull out actual word embeddings
word.embeddings <-
tfidf.svd.results$v %*% diag(tfidf.svd.results$d) %>%
  as.tibble() %>%
  mutate(
    word = colnames(emotions.tfidf)
  ) %>%
  select(word, everything())
word.embeddings

# Recreating a cosine similarity function
cosine.similarity <- function(token, data = word.embeddings){
  

  # Checking if word exist
  if(dim(filter(word.embeddings, word == token))[1] == 0) stop("Word does not exist in vocabulary. Sorry :)")

  # Making matrix
  data.matx <- as.matrix(word.embeddings[,-1])

  # Label rows
  rownames(data.matx) = word.embeddings %>% pull(1)

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
cosine.similarity(wordStem("joy")) %>%
  arrange(-cosine) %>%
  print(n = 20)
# Hmm. The word embeddings show slight alignment but it is obvious that there is not enough data to improve results.
# i.e., for some reason, airport is associated with joy.
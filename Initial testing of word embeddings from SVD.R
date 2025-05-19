# Loading required libraries
library(tidyverse)
library(tidytext)
library(widyr)

# Loading data
emotion.data <-
read_delim(
  file = "C:/Users/Erhun/Documents/Data Analysis/Projects/Reccomendation System/Data/Kaggle Emotions Datasets/train.txt",
  delim = ";",
  col_names = FALSE
) %>%
  rename(text = X1, emotion = X2)
emotion.data

# Lets try to implement word embeddings into our emotion prediction
# Since this project isn't heavy duty, we could use simple dimension reduction techniques

# Get tokens
emotions.tokens <-
emotion.data %>%
  mutate(
    sentence = row_number(),
    text = str_replace_all(text, " don t ", " dont "),
    text = str_replace_all(text, " i m ", "  im "),
    text = str_replace_all(text, " can t ", "  cant ")
  ) %>%
  unnest_tokens(word, text) %>%
  filter(word != "")
emotions.tokens

# Calculate pmi
emotions.pmi <- pairwise_pmi(emotions.tokens, item = word, feature = sentence)
emotions.pmi

# Inspect calculations
emotions.pmi %>%
  arrange(pmi)

# Create matrix for easier calculations
emotions.matrix <- emotions.pmi %>%
  pivot_wider(names_from = item2, values_from = pmi, values_fill = 0) %>%
  select(-item1) %>%
  as.matrix()
emotions.matrix[1:5, 1:5]

# Label rows
rownames(emotions.matrix) <- emotions.pmi %>%
  pivot_wider(names_from = item2, values_from = pmi, values_fill = 0) %>%
  pull(item1)
emotions.matrix[1:5, 1:5]

# Eigen transformation to find word embeddings
emotions.eigen <- RSpectra::eigs(t(emotions.matrix) %*% emotions.matrix, k = 200)
str(emotions.eigen)

# View new dimensions
emotions.eigen$vectors[1:5, 1:5]

# Create projections of emotions.pmi
emotions.matrix[1:5, 1:5]

# View variation magnitudes
emotions.eigen$values %>%
  summary()

# Each column coresponds to new dimensions ranked by amount of variance
# Lets name the columns and rows appopraitely
colnames(emotions.eigen$vectors) <- paste0("dim_", 1:200)
rownames(emotions.eigen$vectors) <- emotions.pmi %>%
  pivot_wider(names_from = item2, values_from = pmi, values_fill = 0) %>%
  pull(item1)
emotions.eigen$vectors[1:5, 1:5]

# Create tidy format for dimensions
emotions.eigen.tidy <- emotions.eigen$vectors %>%
  as_tibble(rownames = "item1") %>%
  pivot_longer(cols = dim_1:dim_200) %>%
  rename(dimension = name) %>%
  mutate(dimension = str_remove(dimension, "dim_"))
emotions.eigen.tidy

# To check if it worked well by seeing vectors close to various vectors
cosine.similarity <- function(token, data = emotions.eigen.tidy){
  # Pivot data to widyr format and convert to matrix
  data.pivot = data %>%
    mutate(dimension = paste0("dim_", dimension)) %>%
    pivot_wider(names_from = dimension, values_from = value)
  
  # Making matrix
  data.matx <- as.matrix(data.pivot[,-1])

  # Label rows
  rownames(data.matx) = data.pivot %>% pull(1)

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
similarity.example <- cosine.similarity("compassion")
similarity.example %>%
  arrange(-cosine)
# Interesting results

# Lets try again
cosine.similarity("joy") %>%
  arrange(-cosine) %>%
  print(n = 40)
# We see that the words are not exactly closely related.
# I propose three reasons:
# 1. The words where not broken down into base format, i.e., stemmed
# 2. The dimensions used was too small
# 3. The number of tokens worked with was also too small.

# Lets solve this in the next iteration of finding word embeddings.
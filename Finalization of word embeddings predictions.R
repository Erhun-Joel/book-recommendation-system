# Loading libraries
library(tidyverse)
library(tidytext)

# Loading data fro github repo
emotion.data <-
read_delim(
  "https://raw.githubusercontent.com/Erhun-Joel/book-recommedation-system/refs/heads/main/Data/Kaggle%20Emotions%20Datasets/train.txt",
  col_names = c("text", "emotion")
)

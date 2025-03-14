# To get book library from Gutenberg API

# Loading required libraries
library(tidyverse)
library(httr)

# Creating function to automatally collect API data
get.book.data <- function(){

  # Declaring base url
  base.url = "https://gutendex.com/books/?page="

  # Declaring page number variable
  page = 1

  # Defining dataset variables
  title = c()
  description = c()
  language = c()
  genre = c()
  tags = c()
  authors = c()

  # Creating infinity loop only to break when no other page exist
  while(TRUE){

    # Getting API response
    response = GET(paste0(base.url, page))

    # Parsing out API response
    response.content = content(response)

    # Creating loop for number of books gotten
    for(i in 1:length(response.content$results)){

      # Appending out contents
      # Using if statements to handle NULL values

      # Appending title contents
      if(!is.null(response.content$results[[i]]$title)){
        title = c(title, response.content$results[[i]]$title)
      }else{
        title = c(title, NA)
      }

      # Appending description contents
      if(!is.null(response.content$results[[i]]$summaries) && length(response.content$results[[i]]$summaries) != 0){
        description = c(description, paste(as_vector(response.content$results[[i]]$summaries), collapse = "----------"))
      }else{
        description = c(description, NA)
      }

      # Appending language contents
      if(!is.null(response.content$results[[i]]$languages[[1]])){
        language = c(language, response.content$results[[i]]$languages[[1]])
      }else{
        language = c(language, NA)
      }
      
      # Dealing with slightly complicated contents

      # Formating tags and combining to tag vector
      if(!is.null(response.content$results[[i]]$subjects)){
        tag.specific = paste(as_vector(response.content$results[[i]]$subjects), collapse = ", ")
        tags = c(tags, tag.specific)
      }else{
        tags = c(tags, NA)
      }

      # Formating genre data and combining to genre vector
      if(!is.null(response.content$results[[i]]$bookshelves)){
        genre.specific = as_vector(response.content$results[[i]]$bookshelves) %>%
          str_remove("Browsing: ") %>%
          paste(collapse = ", ")
        genre = c(genre, genre.specific)
      }else{
        genre = c(genre, NA)
      }
      
      # Frormating author data and combining to author vector
      if(!is.null(response.content$results[[i]]$authors) && length(response.content$results[[i]]$authors) != 0){

        # Declare mini variable for combining multiple authors of same book
        authors.initial = c()

        # Initializing for-loop of length equal to number of authors
        for(l in 1:length(response.content$results[[i]]$authors)){
          # Inputing mini-variable with author names
          authors.initial = c(authors.initial, response.content$results[[i]]$authors[[l]]$name)
        }
        # Collapsing vector into one element
        authors.initial = paste(authors.initial, collapse = "|")
        # Pasting to author variable
        authors = c(authors, authors.initial)
      }else{
        authors = c(authors, NA)
      }

    }

    # Checking break condition
    if(is.null(response.content$`next`)){
      break
    }
    
    # Increasing page variable
    page = page + 1

  }

  # Create dataset
  book.data = tibble(
    title,
    description,
    language,
    genre,
    tags,
    authors
  )

  # Modifying the dataset
  book.data = book.data %>%
    mutate(
      title = str_remove(title, ";.*"),
      description = str_remove_all(description, "\\\"|\\(This is an automatically generated summary.\\)"),
      genre = str_replace_all(genre, " &|/", ","),
      authors = str_remove_all(authors, ",|\\(|\\)|.\\."),
      authors = str_replace_all(authors, "  ", " ")
    )

  # Return dataset
  return(book.data)

}
book.data = get.book.data()
book.data

# Save data into needed folder using the write.csv function below
# write.csv(book.data, file = "---book.library.csv")







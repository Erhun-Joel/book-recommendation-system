# |-------------------------- To get book library from Gutenberg API --------------------------|

# Loading required libraries
library(tidyverse)
library(httr)

# Creating function to automatally collect API data
get.book.data <- 
  function(){
    # Declare While relate functions
    next_address <- "Not Null"
    i <- 1
  
    # Declare response list
    response_list <- c()
  
    # Get all page responses
    while(!is.null(next_address)){
      gutenberg.api <- paste0("https://gutendex.com/books/?page=", i)
  
      gutendex.response <- GET(gutenberg.api)
  
      gutendex.content <- content(gutendex.response)
  
      response_list <- c(
        response_list,
        gutendex.content$results
      )
  
      i = i + 1
      
      if (i %% 200 == 0) message("We are now at page number ", i)
      next_address <- gutendex.content[["next"]]
  
    }
  
    # Get out response data
    output.data <-
    response_list %>%
      map_dfr(
        .f = function(x){
          tibble(
            id = if (!is.null(x$id)) x$id else NA_character_,
            title = if(!is.null(x$title)) x$title else NA_character_,
            description = if (!is.null(x$summaries)) paste0(unlist(x$summaries), collapse = " --|-- ") else NA_character_,
            language = if (!is.null(x$languages)) paste0(unname(unlist(x$languages)), collapse = " & ") else NA_character_,
            subjects = if (!is.null(x$subjects)) paste0(unique(trimws(str_remove(unlist(map(x$subjects, function(x) str_split(x, " -- |, "))), "\\(.*?\\)"))), collapse = " & ") else NA_character_,
            image_urls = if (!is.null(x$formats$`image/jpeg`)) x$formats$`image/jpeg` else NA_character_
          )
        }
      ) %>%
      mutate(description = str_remove_all(description, "\\(This is an automatically generated summary.\\)|\\\""))
  
    # Declare object to be outputed
    return(output.data)
  }
book.data = get.book.data()
book.data

# Save data into needed folder using the write.csv function below
# Due to size restrictions, they would be broken down into subsets
# write.csv(book.data[(book.data$id %in% c(1:40000)),], file = "---book_library_first.csv")
# write.csv(book.data[!(book.data$id %in% c(1:40000)),], file = "---book_library_second.csv")







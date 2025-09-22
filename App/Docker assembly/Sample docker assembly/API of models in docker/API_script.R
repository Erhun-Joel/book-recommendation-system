# ---------- API of various models ---------- #
# Loading necessary libraries
library(dplyr)
library(plumber)

#* @apiTitle Book Reccommendation Models
#* @apiDescription The API hosting the models used for book reccommendation

#* Information on models present
#* @get /info
function(){
  info <- list(
    test_greeting = c("Hello", "Hi", "Pleased to meet you!")
  )

  return(info)
}

#* Multimodal model
#* @post /multi_modal
function(req){

  model = 
}
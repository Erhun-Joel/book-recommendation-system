# |--------------------------Defining Shiny Server----------------------------|

server <- function(input, output, session) {

  # Loading model
  operating_model <- tryCatch(
    {
      readRDS(url("https://raw.githubusercontent.com/Erhun-Joel/book-recommendation-system/refs/heads/main/Models/Selected/selected.rds"))
    },
    error = function(e) return(NULL)
  )

  # Loading probabilities csv
  probabilities <- read.csv("https://raw.githubusercontent.com/Erhun-Joel/book-recommendation-system/refs/heads/main/Data/Models%20probabilities/Selected/probabilities.csv") %>%
    select(id, .pred_anger, .pred_fear, .pred_joy, .pred_love, .pred_sadness, .pred_surprise) %>%
    tidyr::pivot_longer(.pred_anger:.pred_surprise, names_to = "emotion")

  # Define reactive objects
  emotion_pred <- reactiveVal(NULL)
  similar_id <- reactiveVal(NULL)
  selected_id <- reactiveVal(NULL)
  book_results <- reactiveVal(NULL)
  navigation_index <- reactiveVal(1)

  # Define objects that change on the recommend button
  observeEvent(input$go, {
    if (is.null(operating_model) || input$text_input == "") {
      emotion_pred(NULL)
    } else {
      predictions = predict(
        operating_model,
        tibble(text = input$text_input),
        type = "prob"
      )

      output = as.vector(t(predictions))
      names(output) = colnames(predictions)
      output = sort(output)

      emotion_pred(output)
    }

    if (!is.null(emotion_pred())) {
      results <- names(emotion_pred())

      first.id <-
      probabilities %>%
        group_by(id) %>%
        top_n(n = 1, wt = value) %>%
        filter(emotion == results[6]) %>%
        pull(id)

      similar_id(sample(x = unique(probabilities[["id"]]), size = 10, replace = FALSE))
      
      print(paste0("first.id: ", first.id))

      second.id <-
      probabilities[probabilities$id %in% first.id,] %>%
        filter(emotion != results[6]) %>%
        group_by(id) %>%
        top_n(n = 1, wt = value) %>%
        filter(emotion == results[5]) %>%
        pull(id)
      print(paste0("second.id: ", second.id))

      if(length(second.id) >= 10) similar_id(second.id)

      third.id <-
      probabilities[probabilities$id %in% second.id,] %>%
        filter(emotion %in% results[5:6]) %>%
        group_by(id) %>%
        top_n(n = 1, wt = value) %>%
        filter(emotion == results[4]) %>%
        pull(id)
      print(paste0("third.id: ", third.id))

      if(length(third.id) >= 10) similar_id(third.id)
    } else {
      similar_id(NULL)
    }

    if (!is.null(similar_id())) {
      selection <- sample(similar_id(), size = 10, replace = FALSE)
      selected_id(selection)
    } else {
      selected_id(NULL)
    }

    print(paste0("similar_id: ", similar_id()))
  })

  # Getting the equivalent resources from gutenberg api
  observe({
    if(!is.null(selected_id())) {
      ids <- selected_id()

      response_list <- c()

      for(i in 1:length(ids)) {
        api.address <- paste0("https://gutendex.com/books/", ids[i], "/")

        gutendex.response <- GET(api.address)
  
        gutendex.content <- content(gutendex.response)

        response_list <- c(
          response_list,
          list(gutendex.content)
        )

      }

      output.data <-
        response_list %>%
          map_dfr(
            .f = function(x){
              tibble(
                id = if (!is.null(x$id)) x$id else NA_character_,
                title = if(!is.null(x$title)) x$title else NA_character_,
                description = if (!is.null(x$summaries)) paste0(unlist(x$summaries), collapse = " --|-- ") else NA_character_,
                image_urls = if (!is.null(x$formats$`image/jpeg`)) x$formats$`image/jpeg` else NA_character_,
                location = if (!is.null(x$formats$`text/html`)) x$formats$`text/html` else NA_character_,
                authors = if (!is.null(x$authors)) paste(unname((x$authors %>% unlist())[names(x$authors %>% unlist()) == "name"]), collapse = " & ") else NA_character_
              )
            }
          ) %>%
          mutate(description = str_remove_all(description, "\\(This is an automatically generated summary.\\)|\\\""))
      
      print(output.data)
      book_results(output.data)
          
    } else {
      book_results(NULL)
    }

  })

  # Modifying index when next button is clicked
  observeEvent(input$next_page, {
    current_page = navigation_index()

    if (current_page >= 10) {
      navigation_index(1)
    } else {
      navigation_index(current_page + 1)
    }

  })

  # Modifying index when previous button is clicked
  observeEvent(input$previous_page, {
    current_page = navigation_index()

    if (current_page <= 1) {
      navigation_index(10)
    } else {
      navigation_index(current_page - 1)
    }
    
  })

  # Exposing the navigation index to use in box coloration
  page_number <- reactive({
    navigation_index()
  })

  # Defining UI setup
  output$output_ui <- renderUI({
    if (input$go == 0) {
      return(
        tagList(
          img(
            src = "images/placeholder_image.jpg",
            alt = "Placeholder Image",
            width = "100%",
            height = "100%"
          )
        )
      )
    } else {
      print(book_results())

      return(
        tagList(
          div(
            img(
              # Place book image here
              src = book_results()[["image_urls"]][navigation_index()],
              alt = "Book Image",
              height = "100%",
              style = "padding: 5px; border-radius: 10px;"
            ),
            style = "height: 55%;"
          ),
          div(
            span(p(book_results()[["title"]][navigation_index()])),
            span(p(book_results()[["authors"]][navigation_index()])),
            style = "height: 5%",
            id = "meta_info"
          ),
          div(
            p(
              book_results()[["description"]][navigation_index()],
              style = "height: 100%; overflow: auto; color: rgb(52, 52, 52); border: 1px solid rgb(110, 110, 110); border-radius: 5px; font-size: 14px;"
            ),
            style = "height: 30%"
          ),
          div(
            tags$a(
              href = book_results()[["location"]][navigation_index()],
              target = "_blank",
              tags$i(
              " READ BOOK",
              class = "fa-solid fa-book",
              style = "padding-left: 5px; margin-top: 0px;"
            ), id = "book_link"),
            style = "height: 5%",
            id = "icon_div"
          ),
          div(
            div(
              actionButton(inputId = "next_page", label = "Next", class = "movement_btn"),
              actionButton(inputId = "previous_page", label = "Previous", class = "movement_btn"),
              style = "width: 70%"
            ),
            div(
              tags$i(class = "fa-solid fa-square", style = ifelse(navigation_index() == 1, "color: red;", "")),
              tags$i(class = "fa-solid fa-square", style = ifelse(navigation_index() == 2, "color: red;", "")),
              tags$i(class = "fa-solid fa-square", style = ifelse(navigation_index() == 3, "color: red;", "")),
              tags$i(class = "fa-solid fa-square", style = ifelse(navigation_index() == 4, "color: red;", "")),
              tags$i(class = "fa-solid fa-square", style = ifelse(navigation_index() == 5, "color: red;", "")),
              tags$i(class = "fa-solid fa-square", style = ifelse(navigation_index() == 6, "color: red;", "")),
              tags$i(class = "fa-solid fa-square", style = ifelse(navigation_index() == 7, "color: red;", "")),
              tags$i(class = "fa-solid fa-square", style = ifelse(navigation_index() == 8, "color: red;", "")),
              tags$i(class = "fa-solid fa-square", style = ifelse(navigation_index() == 9, "color: red;", "")),
              tags$i(class = "fa-solid fa-square", style = ifelse(navigation_index() == 10, "color: red;", "")),
              style = "width: 30%"
            ),
            class = "pheripherials"
          )
        )
      )
    }
  })
}

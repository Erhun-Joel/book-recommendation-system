# |--------------------------Defining Shiny Server----------------------------|

server <- function(input, output, session) {

  # Define reactive objects
  selected_id <- reactiveVal(NULL)
  book_results <- reactiveVal(NULL)
  navigation_index <- reactiveVal(1)

  # Define objects that change on the recommend button
  observeEvent(input$go, {

    selected_id(
      tryCatch(
        expr = {
          httr::POST(
            url = "http://project_api:5101/reccomend",
            body = list(shiny_input_text = input$text_input),
            encode = "json"
          ) %>%
            content %>%
            unlist()
        },
        error = function(e){
          print(paste0("error message: ", e))
          NULL
        }
      )
    )

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

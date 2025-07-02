# |--------------------------Defining Shiny Server----------------------------|

server <- function(input, output, session) {
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
              src = "images/placeholder_image.jpg",
              alt = "Placeholder Image",
              height = "100%",
              style = "padding: 5px; border-radius: 10px;"
            ),
            style = "height: 55%;"
          ),
          div(
            span(p("Pleased")),
            span(p("Happy")),
            span(p("Sad")),
            style = "height: 5%",
            id = "meta_info"
          ),
          div(
            p(
              HTML("
            Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim.
                      Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim.
                                Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim.
            Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim.
                      Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim.
                                Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa.
  
            "),
            style = "height: 100%; overflow: auto; color: rgb(52, 52, 52); border: 1px solid rgb(110, 110, 110); border-radius: 5px;"
            ),
            style = "height: 30%"
          ),
          div(
            tags$i(
              " READ BOOK",
              class = "fa-solid fa-book",
              style = "padding-left: 5px; margin-top: 0px;"
            ),
            style = "height: 5%",
            id = "icon_div"
          ),
          next_section
        )
      )
    }
  })
}

next_section <- div(
  div(
    actionButton(inputId = "next", label = "Next", class = "movement_btn"),
    actionButton(inputId = "previous", label = "Previous", class = "movement_btn"),
    style = "width: 70%"
  ),
  div(
    tags$i(class = "fa-solid fa-square"),
    tags$i(class = "fa-solid fa-square"),
    tags$i(class = "fa-solid fa-square"),
    tags$i(class = "fa-solid fa-square"),
    tags$i(class = "fa-solid fa-square"),
    tags$i(class = "fa-solid fa-square"),
    tags$i(class = "fa-solid fa-square"),
    tags$i(class = "fa-solid fa-square"),
    tags$i(class = "fa-solid fa-square"),
    tags$i(class = "fa-solid fa-square"),
    style = "width: 30%"
  ),
  class = "pheripherials"
)
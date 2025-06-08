# Defining user interface
ui <- fluidPage(
  # Setting out head html commands
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css")
  ),

  # Main container
  div(
    class = "main",
    titlePanel("Find Your Next Read!"),
    sidebarLayout(
      sidebarPanel(
        textAreaInput(inputId = "text", label = NULL, placeholder = "Kindly tell us about how you want to feel right now"),
        fluidRow(
          column(
            9,
            actionButton("recommend", label = "Recommend!", width = "100%"),
            class = "left__column"
          ),
          column(
            3,
            actionButton("clear", label = "Clear Text", width = "100%"),
            class = "right__column"
          )
        )
      ),
      mainPanel(h3("This is the main bar"))
    )
  )
)
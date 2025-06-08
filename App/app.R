# Loading needed libraries
library(shiny)

# Defining server
server <- function(input, output){}

# Defining style of ui using css
styles.css <-
"
body, .sidebar, .mainbar {
background-color: #d3d3d3;
border-color: #d3d3d3;
}

.main {
padding: 150px;
}

.left__column,
.right__column {
padding: 3px;
}
"

# Defining user interface
ui <- fluidPage(
  # Setting out head html commands
  tags$head(
    tags$style(
      HTML(styles.css)
    )
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

# Run Shiny App
shinyApp(ui, server)


# |--------------------------- Defining Shiny UI -----------------------------|

# ui template
ui <- htmlTemplate(
  filename = "www/index.html",
  output_ui = withSpinner(uiOutput("output_ui"), type = 8, color = "red")
)

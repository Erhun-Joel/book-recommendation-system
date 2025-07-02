# |--------------------------- Defining Shiny UI -----------------------------|

# ui template
ui <- htmlTemplate(
  filename = "www/index.html",
  output_ui = uiOutput("output_ui")
)
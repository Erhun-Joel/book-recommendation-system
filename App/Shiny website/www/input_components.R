# Creating input buttons

# Submit button
submit_button <- actionButton(
  inputId = "go",
  label = "Recommend!",
  class = "submit_btn"
)

# Reset button
reset_button <- actionButton(
  inputId = "reset",
  label = "Reset!",
  class = "reset_btn"
)

# Input text
emotion_text <-
textAreaInput(
  inputId = "text_input",
  label = NULL,
  placeholder = "Tell us how you feel. We will reccommend something for you to read!"
)
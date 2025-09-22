# ---------- Script containing run instructions for plumber API ---------- #
# Loading neccessary library
library(plumber)

plumber::plumb("API_script.R")$run(port = 8000)

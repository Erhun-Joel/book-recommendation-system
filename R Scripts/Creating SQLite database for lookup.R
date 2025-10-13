# ---------- Creating a lookup database for various developed model ---------- #
# Loading libraries
library(dplyr)
library(DBI)

# Bring out lookup table of random forest from github
# Note; only this model has been fully developed
look.up.id <- read.csv(
  "https://github.com/Erhun-Joel/book-recommendation-system/raw/refs/heads/main/Data/Models%20probabilities/rf_probabilities.csv"
) %>%
  as_tibble() %>%
  select(-any_of("X")) %>%
  janitor::clean_names()
look.up.id

# Create database
look.up.database <- dbConnect(
  SQLite(),
  "C:/Users/Erhun/Documents/Data Analysis/Projects/Reccomendation System/Data/Lookup database/lookup_database.db"
)
look.up.database

# Write lookup tables to database
look.up.database %>%
  dbWriteTable(name = "rf_v1", value = look.up.id)

# Confrim its vaiability
look.up.database %>%
  dbGetQuery(
    "
    SELECT *
    FROM rf_v1
    LIMIT 5;
    "
  )

# disconnect from database
dbDisconnect(look.up.database)

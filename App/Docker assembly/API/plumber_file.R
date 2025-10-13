# ---------- File containing api response functions ---------- #
# Loading libraries
library(dplyr)
library(DBI)
library(RSQLite)
library(jsonlite)
library(workflows)
library(textrecipes)
library(glue)
library(stringr)
library(ranger)

#* @apiTitle Reccommendation engine
#* @apiDescription This api helps select the books to be reccommended to the user

#* Actual selection function
#* @post /reccomend
function(req) {
    text = jsonlite::fromJSON(req$postBody)$shiny_input_text

    model = readRDS("/home/api/models/rf_v1.rds")

    predictions = predict(model, tibble(text), type = "prob") %>%
        unlist %>%
        sort(decreasing = TRUE) %>%
        names %>%
        str_remove(".")

    database.connection <- dbConnect(
        SQLite(),
        "/home/api/lookup_database.db"
    )

    selected.ids <- database.connection %>%
        dbGetQuery(
            glue::glue(
                "
                SELECT *
                FROM rf_v1
                WHERE {predictions[1]} > {predictions[2]} AND 
                {predictions[2]} > {predictions[3]} AND 
                {predictions[3]} > {predictions[4]}
                ORDER BY RANDOM()
                LIMIT 10;
                "

            )
        ) %>%
        pull(id)
    
    if(length(selected.ids) < 10) {
        selected.ids <- database.connection %>%
            dbGetQuery(
                glue::glue(
                    "
                    SELECT *
                    FROM rf_v1
                    WHERE {predictions[1]} > {predictions[2]} AND 
                    {predictions[2]} > {predictions[3]}
                    ORDER BY RANDOM()
                    LIMIT 10;
                    "
    
                )
            )
    }

    dbDisconnect(database.connection)
    
    return(selected.ids)
}
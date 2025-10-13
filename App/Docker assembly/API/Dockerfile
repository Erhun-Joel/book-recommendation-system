FROM rocker/r-ver:4.3.3

RUN mkdir /home/api

RUN R -e "install.packages(c('dplyr', 'plumber', 'DBI', 'RSQLite', 'jsonlite', 'workflows', 'glue', 'textrecipes', 'stringr', 'ranger'))"

COPY /lookup_database.db /home/api/lookup_database.db
COPY /plumber_file.R /home/api/api_functions.R
COPY /models /home/api/models

EXPOSE 5101

CMD ["R", "-e", "plumber::plumb('/home/api/api_functions.R')$run(port = 5101, host = '0.0.0.0')"]

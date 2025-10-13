FROM rocker/shiny:4.3.3

RUN mkdir /home/my_web_app

RUN R -e "install.packages(c('shiny', 'shinycssloaders', 'workflows', 'textrecipes', 'dplyr', 'purrr', 'stringr', 'httr', 'tidyr', 'ranger'))"

COPY /www /home/my_web_app/www
COPY /global.R /home/my_web_app/global.R
COPY /ui.R /home/my_web_app/ui.R
COPY /server.R /home/my_web_app/server.R

EXPOSE 5100

CMD ["R", "-e", "shiny::runApp('/home/my_web_app', port = 5100, host = '0.0.0.0')"]
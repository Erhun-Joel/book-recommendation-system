# Book Recommendation System
### Summary
This project is an attempt to perform an end to end analysis matching potential books to use cases. Its uniqueness lies in how that is done, by building models to match text inputs to emotions.

### Project Objective
To accurately relate word corpuses to emotions experienced by humans and as a side effect, recommend appropraite books to read.

### Data
The main data used is gotten from Kaggle website and contains various sentences with emotion pairs. Six emotions are identified in total: sadness, joy, surprise, fear, anger and love. The link to the data is found [here](https://www.kaggle.com/datasets/praveengovi/emotions-dataset-for-nlp).

Information on the books to be recommended where gotten from the the [gutenberg project](https://www.gutenberg.org/) api directly which is free to use. Other data sets in this project are manually setup.

### Models
Model development in this project is divided into two main parts:
- Variable preparation
- Model Training

Various methods where used to prepare the text data for prediction activities. Most of them started with the computation of term frequency-inverse document frequency. From this, the models where either trained directly or worked upon further. A notable case involves generating word embeddings by performing Singular Value Decomposition on a matrix representation of the data.

On the other hand, the model types computed where limited to mainly simple trees. Computation cost made it difficult to expand  from this significantly.
### Shiny App
A Shiny App was made as a interface with the model and user to showcase use cases. It performs selection by predicting the order of emotions being displayed by a given text and looking for matches in the gutenberg api. A sample of the app is deployed in [shinyapps.io](https://qcidhj-erhun-igbinnosa.shinyapps.io/book_recommendation_app/).

There may be difficulty experienced in running the online version of the web app, most likely from from limited RAM allocation. If so, this web app may be used locally using docker. Navigate to and download the docker assembly folder under the App folder or click [here](App/Docker%20assembly). Then using the computers terminal, run the following lines of code in sequence:

```
cd "<docker assembly directory>"
```

```
docker-compose up --build
```
If terminal is opened in project folder, you may skip the first line of code.
Note: This App differs from the online version and involves setting up an api powered by an SQLite lookup database to select book recommendations and the web app involved with user interaction.

### Shortcomings and Future Updates
After engagement with this project, it is obvious that model performance leaves a lot to be desired. The main reason behind this is data quality. The data sourced for this does not seem to emulate common speech in the English language. Also, there seems to not be enough samples to justify more advanced context-aware variable generating methods.
The gutenberg project used in this project as a library of books also tends to contain older books related to classic literature, avioding copyright infringments that comes with copying and uploading more modern books.

Future improvements coming up would includes:
- Adding google books api to the book search.
- Sourcing more broader word corpuses related to text and emotions.

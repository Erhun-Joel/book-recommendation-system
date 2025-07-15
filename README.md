# Book Recommendation System
### Summary
This project is an attempt to perform an end to end analysis matching potential books to use cases. Its uniqueness lies in how that is done, by building models to match text inputs to emotions.

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
A Shiny App was made as a interface with the model and user to showcase use cases. It performs selection by predicting the order of emotions being displayed by a given text and looking for matches in the gutenberg api. A sample of the app is deployed in [shinyapps.io](https://qcidhj-erhun-igbinnosa.shinyapps.io/book_recommendation_app/)
### Future Updates
Future improvements coming up includes:
- Adding google books api to the book search due to gutenberg lack of current book listings.
- Increase in model complexity to increase prediction performance.
- Re-attempting word embeddings with more computationally efficient methods.

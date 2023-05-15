from flask import Flask, jsonify, request
import numpy as np
import scipy



############################################
from keras.models import Sequential
# from sklearn.externals import joblib
import numpy
import pandas as pd
from keras.datasets import imdb
from sklearn import model_selection
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers.embeddings import Embedding
from keras.preprocessing import sequence
from sklearn.model_selection import train_test_split
import scipy
import pickle

# fix random seed for reproducibility
numpy.random.seed(7)




############################################
from tensorflow.keras.models import load_model
from keras.models import Sequential
# from sklearn.externals import joblib
import pandas as pd
import numpy as np
from sklearn import linear_model
# from sklearn.externals import joblib
# import joblib
from bs4 import BeautifulSoup
import re
from sklearn.feature_extraction.text import CountVectorizer
####################################################
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from keras.preprocessing import sequence
import pickle

import flask
app = Flask(__name__)

model = load_model('model.h5')
helper = pickle.load(open('helper.pkl', 'rb'))

@app.route('/')
def hello_world():
    return 'Hello World! ...   Please Enter Host Id'


@app.route('/index')
def index():
    return flask.render_template('IMDB.html')
    # return Flask(__name__, template_folder={'D:\KASHIF\Y_OneDrive\IMP_DOC_4_Interviews\Project-02\IMDB.html'})


@app.route('/predict', methods=['POST'])
# def load_data():
    # model = load_model('model.h5')
    # helper = pickle.load(open('helper.pkl', 'rb'))
    # return model, helper

def predict():
    # model = load_model('model.h5')
    # helper = pickle.load(open('helper.pkl', 'rb'))
    to_predict_list = request.form.to_dict()
    review_text = to_predict_list['review_text']
    max_review_length = 600 
    print(type(review_text))
    print(len(review_text))
    temp = []
    temp.append(review_text)
    sample = helper.transform(temp).nonzero()[1]
    print(type(sample))
    print(len(review_text))
    sample = sequence.pad_sequences([sample], maxlen=max_review_length)
    val = model.predict(sample)
    prediction=""
    if val[0][0] >= 0.5:
        prediction = "Positive :)"
    else:
        prediction = "Negative :("
    return jsonify({'Prediction': prediction})
  

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080)
    # app.run(port=8080, debug=True)


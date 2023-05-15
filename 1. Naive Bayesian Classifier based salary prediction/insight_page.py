import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


def show_explore_page():
    st.write(""" ### Raw Data""")
    st.write(df.head(50))
    st.write(""" ### Pair-Wise Correlation Coefficient""")
    corr = df.corr()
    tt = corr.style.background_gradient(cmap='coolwarm', axis=1)\
        .set_properties(**{'max-width': '50px', 'font-size': '10pt'})\
        .set_precision(3) 
    st.write(tt) 

@st.cache
def load_data():
    data = 'adult.csv'
    df = pd.read_csv(data, header=None, sep=',\s', engine = 'python')
    col_names = ['age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status', 'occupation', 'relationship',
                 'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income']

    df.columns = col_names
    return df

df = load_data()







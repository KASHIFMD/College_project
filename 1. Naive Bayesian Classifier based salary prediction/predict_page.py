import numpy as np
import pandas as pd
import streamlit as st
import pickle


def load_model():
    with open('saved_steps.pkl', 'rb') as file:
        data = pickle.load(file)
    return data


data = load_model()

gnb = data["model"]
encoder = data["encoder"]
scaler = data["scaler"]


def show_predict_page():
    st.title("Salary Prediction")
    st.write(""" ### Please insert information to display salary class """)

    workclass = ['State-gov', 'Self-emp-not-inc', 'Private', 'Federal-gov',
                 'Local-gov', 'Self-emp-inc', 'Without-pay',
                 'Never-worked', 'other']
    occupation = ['Adm-clerical', 'Exec-managerial', 'Handlers-cleaners',
                  'Prof-specialty', 'Other-service', 'Sales', 'Craft-repair',
                  'Transport-moving', 'Farming-fishing', 'Machine-op-inspct',
                  'Tech-support', 'Protective-serv', 'Armed-Forces',
                  'Priv-house-serv']

    native_country = ['United-States', 'Cuba', 'Jamaica', 'India', 'Mexico',
                      'South', 'Puerto-Rico', 'Honduras', 'England', 'Canada', 'Germany',
                      'Iran', 'Philippines', 'Italy', 'Poland', 'Columbia', 'Cambodia',
                      'Thailand', 'Ecuador', 'Laos', 'Taiwan', 'Haiti', 'Portugal',
                      'Dominican-Republic', 'El-Salvador', 'France', 'Guatemala',
                      'China', 'Japan', 'Yugoslavia', 'Peru',
                      'Outlying-US(Guam-USVI-etc)', 'Scotland', 'Trinadad&Tobago',
                      'Greece', 'Nicaragua', 'Vietnam', 'Hong', 'Ireland', 'Hungary',
                      'Holand-Netherlands']

    education = ['Bachelors', 'HS-grad', '11th', 'Masters', '9th', 'Some-college',
                 'Assoc-acdm', 'Assoc-voc', '7th-8th', 'Doctorate', 'Prof-school',
                 '5th-6th', '10th', '1st-4th', 'Preschool', '12th']

    relationship = ['Not-in-family', 'Husband', 'Wife', 'Own-child', 'Unmarried',
                    'Other-relative']

    marital_status = ['Never-married', 'Married-civ-spouse', 'Divorced',
                      'Married-spouse-absent', 'Separated', 'Married-AF-spouse',
                      'Widowed']

    race = ['White', 'Black', 'Asian-Pac-Islander', 'Amer-Indian-Eskimo',
            'Other']

    sex = ['Male', 'Female']

    tab01 = ''.join(st.selectbox("Country", native_country))
    tab02 = ''.join(st.selectbox("Workclass", workclass))
    tab03 = ''.join(st.selectbox("Occupation", occupation))
    tab04 = ''.join(st.selectbox("Education", education))
    tab05 = ''.join(st.selectbox("Relationship", relationship))
    tab06 = ''.join(st.selectbox("Maritul_status", marital_status))
    tab07 = ''.join(st.selectbox("Race", race))
    tab08 = ''.join(st.selectbox("Gender", sex))

    education_num = st.slider("Years of education", 10, 16, 3)
    age = st.slider("Age", 15, 60, 30)
    fnlwgt = st.slider("Final Weight", 10000, 1500000, 50000)

    capital_gain = st.slider("Capital_gain", 0, 5000, 2000)
    capital_loss = st.slider("Capital_loss", 0, 5000, 2000)
    hours_per_week = st.slider("Working_Hours/Week", 0, 100, 40)

    ok = st.button("""Predict ...     """)
    if(ok):
        X = {'age': [age], 'workclass': [tab02], 'fnlwgt': [fnlwgt], 'education': [tab04], 'education_num': [education_num],
             'marital_status': [tab06], 'occupation': [tab03], 'relationship': [tab05], 'race': [tab07], 'sex': [tab08],
             'capital_gain': [capital_gain], 'capital_loss': [capital_loss], 'hours_per_week': [hours_per_week], 'native_country': [tab01]}
        # st.write(X)
        X = pd.DataFrame.from_dict(X)
        X = encoder.transform(X)
        X = scaler.transform(X)
        cols = ['intercept', 'age', 'workclass_0', 'workclass_1', 'workclass_2',
                'workclass_3', 'workclass_4', 'workclass_5', 'workclass_6',
                'workclass_7', 'fnlwgt', 'education_0', 'education_1', 'education_2',
                'education_3', 'education_4', 'education_5', 'education_6',
                'education_7', 'education_8', 'education_9', 'education_10',
                'education_11', 'education_12', 'education_13', 'education_14',
                'education_num', 'marital_status_0', 'marital_status_1',
                'marital_status_2', 'marital_status_3', 'marital_status_4',
                'marital_status_5', 'occupation_0', 'occupation_1', 'occupation_2',
                'occupation_3', 'occupation_4', 'occupation_5', 'occupation_6',
                'occupation_7', 'occupation_8', 'occupation_9', 'occupation_10',
                'occupation_11', 'occupation_12', 'relationship_0', 'relationship_1',
                'relationship_2', 'relationship_3', 'relationship_4', 'race_0',
                'race_1', 'race_2', 'race_3', 'sex_0', 'capital_gain', 'capital_loss',
                'hours_per_week', 'native_country_0', 'native_country_1',
                'native_country_2', 'native_country_3', 'native_country_4',
                'native_country_5', 'native_country_6', 'native_country_7',
                'native_country_8', 'native_country_9', 'native_country_10',
                'native_country_11', 'native_country_12', 'native_country_13',
                'native_country_14', 'native_country_15', 'native_country_16',
                'native_country_17', 'native_country_18', 'native_country_19',
                'native_country_20', 'native_country_21', 'native_country_22',
                'native_country_23', 'native_country_24', 'native_country_25',
                'native_country_26', 'native_country_27', 'native_country_28',
                'native_country_29', 'native_country_30', 'native_country_31',
                'native_country_32', 'native_country_33', 'native_country_34',
                'native_country_35', 'native_country_36', 'native_country_37',
                'native_country_38', 'native_country_39']
        X = pd.DataFrame(X, columns=[cols]) 
        print(X.shape)
        salary = gnb.predict(X) 

        st.subheader(f"Salary class is {salary[0]}   $")

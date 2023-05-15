from PIL import Image
import streamlit as st
from insight_page import show_explore_page
from predict_page import show_predict_page

page = st.sidebar.selectbox(
    "Explore", ("Predict", "Data insight", "Developed by"))
if page == "Predict":
    show_predict_page()
elif page == "Data insight":
    show_explore_page()
else:
    st.title("Trained and Deployed -\n\n-Mohammed KASHIF")
    st.write("""
Mtech 2nd Year, CSDP\n
Indian Institute Technology Kharagpur\n
Midnapur, W.B., INDIA- 721302\n
Email: kasifmohammed04@kgpian.iitkgp.ac.in\n
Cell: (+91) 9156026054\n
""")


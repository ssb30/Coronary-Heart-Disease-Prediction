# Coronary-Heart-Disease-Prediction

This project aims to predict the Coronary Heart Disease (CHD) in people living in the town of Framingham, Massachusetts for the coming 10-years. Here the detailed history of patients is given This study uses Logistic regression, K nearest neighbors and Random Forest with and without SMOTE to predict the results. From the study we can conclude that Age, Glucose level, Diastolic BP, and total cholesterol level affects the risk factor to be affected by Coronary Heart Disease in the coming 10 years. 

This data set corresponds to the ongoing cardiovascular study of people living in the town of Framingham, Massachusetts. The objective is to determine if the patient has a 10-year risk of developing future Coronary Heart Disease (CHD). The dataset that is available has about 4000 records of 15 different parameters and contains patient information. Every characteristic listed here carries a potential risk.

This study focuses on to the predicting the 10-year risk of developing future coronary heart disease (CHD). This paper will explore the following research questions:
• What are the top 5 features that cause Cardio Vascular Diseases?
• How does age affect the risk factor?
• What role does Blood Pressure have in predicting the Heart Disease risk?

Dataset Description: The dataset has about 4000 records of 15 different parameters and contains patient information. The 15 attributes present in the dataset are divided into categorical and continuous variables. Class label - TenYearCHD has 3,593 negative records and 643 positive records. So, this is an imbalanced data. Negative records represented by 0’s and positive records represented by 1’s

# Repository overview

├── README.md         
├── Coronary_HeartDiseasePrediction_files  #Contains all the plots required for rendering in md file

├── Coronary_HeartDiseasePrediction.Rmd    # It is the rmd file with all the code

├── framingham.csv # It is the dataset used for the project  

└── Coronary_HeartDiseasePrediction.md # The output file with all the code ans outputs

# Conclusion

This project attempted to produce few insights on the top five important reasons for the cause of Coronary Heart Disease. It determines that Random forest with SMOTE is the best model to derive the important features with the best accuracy among all. Based on the findings, precautions might be taken against those who are anticipated to have a high risk of developing coronary heart disease.


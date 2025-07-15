import numpy as np
import pandas as pd
import statsmodels.api as sm
from sklearn.preprocessing import StandardScaler

# Load the data
df = pd.read_csv(r".\score_wellcome_duplicate.csv")

# Define independent variables (IVs) and dependent variable (DV)
x = df[["2023 Turnover", "MSP_sum", "workload","Satisfy_score1", "Manager_score", "Belonging_score",
        "avg_score_Q4","avg_score_Q25"]]

y = df[["productivity_sum"]]


combined_df = pd.concat([x, y], axis=1)
correlation_matrix = combined_df.corr()
print(correlation_matrix)

#scaler_x = StandardScaler()
#scaler_y = StandardScaler()

# Normalize the independent variables
#x = scaler_x.fit_transform(x)

# Normalize the dependent variable
#y = scaler_y.fit_transform(y)

# Add a constant to the model
#x = sm.add_constant(x)

# Fit the initial OLS model
#model = sm.OLS(y, x).fit()


#print(model.summary())
#print(model.rsquared)

# Get predictions
#y_pred_scaled = model.predict(x)

# Inverse transform the predictions to get them back to the original scale
#y_pred = scaler_y.inverse_transform(y_pred_scaled.reshape(-1, 1))

#print(y_pred)
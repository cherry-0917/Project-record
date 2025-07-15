import pandas as pd
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns


df = pd.read_csv(r".\score.csv")
data = df.iloc[1:]

# --------------------------------------------------------------------------------------
## Gallup --- Good manager model
# Role Clarity & Expectation-Setting: Question[22]
# Strengths-Based Coaching & Encouraging: Question[25]
# Recognition & Feedback: Question[16, 29]
# Resource Provision & Support(In work/ mental): Work support: Question[6, 11, 27,31]
# No Q31 in store
X = data[['avg_score_Q6', 'avg_score_Q11', 'avg_score_Q16', 
          'avg_score_Q22', 'avg_score_Q25', 'avg_score_Q27', 'avg_score_Q29']]
X = sm.add_constant(X) 

# Employee Satisfactory: average score of Q1 and Q3
# Manager score: Q17
# Employee belonging, trust, retention:Q[4,12,15,34,35]
# No Q34 and Q35 in store 
Y1 = data['Satisfy_score']
Y2 = data['Manager_score']
Y3 = data['Belonging_score']

# Check length -> True
len(X) == len(Y1) == len(Y2) == len(Y3)

# Fit the model for each dependent variable
model_1 = sm.OLS(Y1, X).fit()
model_2 = sm.OLS(Y2, X).fit()
model_3 = sm.OLS(Y3, X).fit()

# Print the summary for each model
print("Model for Satisfy_score")
print(model_1.summary())
print("Model for Manager_score")
print(model_2.summary())
print("Model for Belonging_score")
print(model_3.summary())

def plot_predictions(y_actual, model, title):
    plt.figure(figsize=(10, 6))
    plt.scatter(y_actual, model.fittedvalues, alpha=0.5)
    plt.plot([y_actual.min(), y_actual.max()], [y_actual.min(), y_actual.max()], 'r--')
    plt.xlabel('Actual Values')
    plt.ylabel('Predicted Values')
    plt.title(title)
    plt.show()

# Prediction plots
plot_predictions(Y1, model_1, 'Predictions for Satisfy Score')
plot_predictions(Y2, model_2, 'Predictions for Manager Score')
plot_predictions(Y3, model_3, 'Predictions for Belonging Score')


# Partial regression plots
fig = plt.figure(figsize=(12, 8))
fig = sm.graphics.plot_partregress_grid(model_1, fig=fig)
plt.show()

fig = plt.figure(figsize=(12, 8))
fig = sm.graphics.plot_partregress_grid(model_2, fig=fig)
plt.show()

fig = plt.figure(figsize=(12, 8))
fig = sm.graphics.plot_partregress_grid(model_3, fig=fig)
plt.show()

# Residual plots
def plot_residuals(model, title):
    residuals = model.resid
    plt.figure(figsize=(10, 6))
    sns.histplot(residuals, kde=True)
    plt.title(title)
    plt.xlabel('Residuals')
    plt.ylabel('Frequency')
    plt.show()

plot_residuals(model_1, 'Residuals of Satisfy Score Model')
plot_residuals(model_2, 'Residuals of Manager Score Model')
plot_residuals(model_3, 'Residuals of Belonging Score Model')

# ------------------------------------------------------------------------------------
## Glint model
# Define the independent variables: avg_score_Q7 to avg_score_Q30
# No response on Q13 and Q14 in store
X_columns = [f'avg_score_Q{i}' for i in range(7, 31)]
X = data[X_columns]
X = sm.add_constant(X) 
X = X.drop(['avg_score_Q13', 'avg_score_Q14'], axis=1)

# Define the dependent variables: avg_score_Q1 to avg_score_Q6
Y_columns = [f'avg_score_Q{i}' for i in range(1, 7)]
Y_variables = {col: data[col] for col in Y_columns}

# Check missing value
X.isnull().sum().to_frame().rename(columns={0:"Total No. of Missing Values"})
Y = data[Y_columns]
Y.isnull().sum().to_frame().rename(columns={0:"Total No. of Missing Values"})

## Fit the model for each dependent variable and print the summary
models = {}
for i, (name, Y) in enumerate(Y_variables.items(), start=1):
    # Align Y with X to ensure they have the same index after dropping NaNs
    Y = Y[X.index]
    if Y.isnull().any():
        Y.dropna(inplace=True)
        X = X.loc[Y.index]  # Update X to match Y's index after dropping

    if len(X) > 0 and len(Y) > 0:
        model = sm.OLS(Y, X).fit()
        models[name] = model
        print(f"Model for {name}")
        print(model.summary())
        print("\n")
    else:
        print(f"No data available for model: {name}")

# Define the titles for the prediction plots
plot_titles = {
    'avg_score_Q1': 'Predictions for Satisfaction(TM)',
    'avg_score_Q2': 'Predictions for Satisfaction(Work)',
    'avg_score_Q3': 'Predictions for Recommendation',
    'avg_score_Q4': 'Predictions for Future Success',
    'avg_score_Q5': 'Predictions for Belonging',
    'avg_score_Q6': 'Predictions for Retention'
}

# Prediction plots for each dependent variable
for name, model in models.items():
    title = plot_titles.get(name, f'Predictions for {name}')  # Default title if name not found
    plot_predictions(Y_variables[name].dropna(), model, title)



# Function to plot residuals
def plot_residuals(model, title):
    residuals = model.resid
    plt.figure(figsize=(10, 6))
    sns.histplot(residuals, kde=True)
    plt.title(title)
    plt.xlabel('Residuals')
    plt.ylabel('Frequency')
    plt.show()


plot_titles = {
'avg_score_Q1': 'Residual Plot for Satisfaction(TM)',
'avg_score_Q2': 'Residual Plot for Satisfaction(Work)',
'avg_score_Q3': 'Residual Plot for Recommendation',
'avg_score_Q4': 'Residual Plot for Future Success',
'avg_score_Q5': 'Residual Plot for Belonging',
'avg_score_Q6': 'Residual Plot for Retention'
}

# Fit the model for each dependent variable and print the summary
models = {}
for name, Y in Y_variables.items():
    # Align Y with X to ensure they have the same index after dropping NaNs
    Y = Y[X.index]
    if Y.isnull().any():
        Y.dropna(inplace=True)
        X = X.loc[Y.index]  # Update X to match Y's index after dropping

    if len(X) > 0 and len(Y) > 0:
        model = sm.OLS(Y, X).fit()
        models[name] = model

        # Use custom plot titles
        plot_title = plot_titles.get(name, f'Residuals for {name}')
        plot_residuals(model, plot_title)

    else:
        print(f"No data available for model: {name}")






# Function to plot all predictions in one overall plot
def plot_overall_predictions(Y_variables, models):
    plt.figure(figsize=(12, 8))
    for name, model in models.items():
        y_actual = Y_variables[name].dropna()
        if len(y_actual) > 0 and len(model.fittedvalues) > 0:
            plt.scatter(y_actual, model.fittedvalues, alpha=0.5, label=f'{name}')
    plt.plot([y_actual.min(), y_actual.max()], [y_actual.min(), y_actual.max()], 'r--', label='Ideal Fit')
    plt.xlabel('Actual Values')
    plt.ylabel('Predicted Values')
    plt.title('Overall Predictions for All Models')
    plt.legend()
    plt.show()
# Overall plot for all models
plot_overall_predictions(Y_variables, models)

import pandas as pd
import numpy as np


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------

# Read from Employee DataFrame
employee_df = pd.read_excel(r".\employee.xlsx", sheet_name=1)

# Select Wellcome in Hong Kong and Store
df = employee_df
df = df.sort_values(by=["Region", "Area", "Store"])
df = df[df["Area"]!="Multi-area"]
df['Address'] = df.loc[:, 'Region'].astype(str) + '-' + df.loc[:, 'Area'].astype(str) + '-' + df.loc[:, 'Store'].astype(str)

# Clean Level 4 to 7 Manager, Retrieve Name only 
df.loc[df["Manager Level 7"]=="9f3451bb-ee29-4ae5-bfc5-79c465faa608 (Deleted User - 9f3451bb-ee29-4ae5-bfc5-79c465faa608)"] = np.nan
df.loc[df["Manager Level 7"]=="1154c274-369d-4e97-bb46-b641e9f553e4 (Deleted User - 1154c274-369d-4e97-bb46-b641e9f553e4)"] = np.nan
df["Manager Level 4"].fillna("No Level 4 Manager", inplace=True)
df["Manager Level 5"].fillna("No Level 5 Manager", inplace=True)
df["Manager Level 6"].fillna("No Level 6 Manager", inplace=True)
df["Manager Level 7"].fillna("No Level 7 Manager", inplace=True)

man_list = ['Manager Level 4', 'Manager Level 5','Manager Level 6', 'Manager Level 7']
for man in man_list:
    df[man] = df[man].apply(lambda p: p.split("(")[1].split("-")[0] if "No Level" not in p else p)

# 4872 Overall
print(len(df['Address'].unique())) 


# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------

# Read from Glint DataFrame
glint_df = pd.read_excel(r".\glint.xlsx", skiprows=range(0, 5))
glint_df = glint_df.sort_values(by=["Store"])
overall = glint_df[glint_df['Store']=="(overall)"].dropna(subset=['Question Text'])

glint_df = glint_df.drop(overall.index)
glint_df = glint_df[glint_df["Questions"]!="2023 YVC 11 Qs"]
glint_df = glint_df[glint_df["Store"]!="(overall)"]
glint_df.replace('--', np.nan, inplace=True)

# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------

# Read from Survey Questions DataFrame Q1-Q38
survey_question_df = pd.read_excel(r".\survey_question_no.xlsx", sheet_name=0)
survey_question_df.columns = ["Question Number", "Question Text"]

# Merge Glint and Survey Questions DataFrame, Only keep those that share the same address
df_final = pd.merge(glint_df[glint_df["Store"].isin(df['Address'])], survey_question_df, on='Question Text', how='right')
df_final = df_final[['Store', 'Overall Invitees', 'Overall Respondents',
       'Overall Response Rate', 'Questions', 'Question Number', 'Question Text',
       'Question Respondents', 'Average Score', 'Percent Favorable']]

df_final = df_final.sort_values(by=["Store", "Question Number"])

# Remove Q31-Q38
#df_final = df_final[df_final["Question Number"].isin(range(1, 31))]
df_final.replace(0, np.nan, inplace=True)

overall = pd.merge(overall, survey_question_df, on='Question Text', how='right')
df_final = pd.concat([overall, df_final], axis=0)
df_final = df_final[['Store', 'Overall Invitees', 'Overall Respondents',
       'Overall Response Rate', 'Questions', 'Question Number', 'Question Text',
       'Question Respondents', 'Average Score', 'Percent Favorable']]

df_final.replace(0, np.nan, inplace=True)
df_final = df_final.sort_values(by=["Store", "Question Number"])

# Ensure 'Address' is correctly populated in both DataFrames
manager_metrics = df.groupby('Address')[['Manager Level 7','Manager Level 6', 'Manager Level 5', 'Manager Level 4']].first().reset_index()

# Merge to add manager levels to df_final
df_final = df_final.merge(manager_metrics, left_on='Store', right_on='Address', how='left')

# Drop the Address column from the merge
df_final.drop(columns=['Address'], inplace=True)
##df_final

df_final.to_csv(r".\train_overall.csv", index=False)

# ------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------

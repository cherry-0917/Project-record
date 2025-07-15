import pandas as pd
import numpy as np

df = pd.read_csv(r".\train_overall.csv")

df_prep = df.rename(columns={
    'Store': 'store_name',
    'Question Number': 'question_no',
    'Average Score': 'avg_score',
    'Percent Favorable': '%_favorable',
    'Overall Response Rate' : 'overall_response_rate(%)',
    'Overall Respondents': 'overall_respondents',
    'Manager Level 7': 'ML_7',
    'Manager Level 6': 'ML_6',
    'Manager Level 5': 'ML_5', 
    'Manager Level 4': 'ML_4'
})[['store_name', 'question_no', 'avg_score', '%_favorable', 'overall_response_rate(%)','overall_respondents', 'ML_7','ML_6', 'ML_5', 'ML_4']]


# Pivot the table to create multi-level columns
pivot_df = df_prep.pivot_table(
    index='store_name',
    columns='question_no',
    values=['avg_score', '%_favorable'],
    aggfunc='first'
).reset_index()




# Get store name from first column
store_col = pivot_df.columns[0][0] if isinstance(pivot_df.columns[0], tuple) else pivot_df.columns[0]
new_columns = [store_col]
for col in pivot_df.columns[1:]:
    if isinstance(col, tuple):
        new_columns.append(f"{col[0]}_{col[1]}")
    else:
        new_columns.append(str(col))

pivot_df.columns = new_columns

# Get response rates (store-level)
store_metrics = df_prep.groupby('store_name')[['overall_respondents','overall_response_rate(%)', 'ML_7','ML_6', 'ML_5', 'ML_4']].first().reset_index()


# Generate ordered columns
questions = sorted(df_prep['question_no'].unique(), key=int)
ordered_columns = [store_col, 'overall_respondents', 'overall_response_rate(%)', 'ML_7','ML_6', 'ML_5', 'ML_4']


# Add score
for q in questions:
    ordered_columns.extend([
        f"avg_score_{q}",
        f"%_favorable_{q}"
    ])

# Create final dataframe
final_df = pivot_df.merge(store_metrics, on=store_col, how='inner')[ordered_columns]

final_df.columns = [
    col.replace('_score_', '_score_Q').replace('_favorable_', '_favorable_Q')
    if any(x in col for x in ['score', 'favorable']) else col
    for col in final_df.columns
]


#Seperate Store and SSC for analysis
final_df['Group'] = None
for i in range(len(final_df['avg_score_Q34'])):
    if str(final_df['avg_score_Q34'][i]) == "nan":
        final_df['Group'][i] = 1
    else:
        final_df['Group'][i] = 2

#DV score setting
final_df['Satisfy_score1'] = (final_df['avg_score_Q1']+final_df['avg_score_Q2'])/2
final_df['Satisfy_score2'] = (final_df['avg_score_Q1']+final_df['avg_score_Q3'])/2
final_df['Manager_score'] = final_df['avg_score_Q7']
#No Q34 and Q35 in store
final_df['Belonging_score'] = np.where(
    final_df['Group'] == 1,
    (final_df['avg_score_Q5'] + final_df['avg_score_Q15'] + final_df['avg_score_Q16'] + final_df['avg_score_Q6']) / 4,
    (final_df['avg_score_Q5'] + final_df['avg_score_Q15'] + final_df['avg_score_Q16'] + final_df['avg_score_Q6'] + final_df['avg_score_Q34']) / 5
)

final_df = final_df.dropna(subset=["avg_score_Q2"])

final_df.to_csv(r".\score_overall.csv", index = False)

final_df = pd.read_csv(r".\score_overall.csv")
final_df = final_df.iloc[1:, ]


final_df['number of employee'] = (final_df['overall_respondents'] / final_df['overall_response_rate(%)'] * 100).round().astype(int)


columns_in_order = ['store_name',   'number of employee', #'FTE', 
                    'ML_7', 'ML_6', 'ML_5', 'ML_4', 
                    'avg_score_Q1', '%_favorable_Q1', 'avg_score_Q2', '%_favorable_Q2', 'avg_score_Q3', '%_favorable_Q3', 
                    'avg_score_Q4', '%_favorable_Q4', 'avg_score_Q5', '%_favorable_Q5', 'avg_score_Q6', '%_favorable_Q6', 
                    'avg_score_Q7', '%_favorable_Q7', 'avg_score_Q8', '%_favorable_Q8', 'avg_score_Q9', '%_favorable_Q9', 
                    'avg_score_Q10', '%_favorable_Q10', 'avg_score_Q11', '%_favorable_Q11', 'avg_score_Q12', '%_favorable_Q12', 
                    'avg_score_Q13', '%_favorable_Q13', 'avg_score_Q14', '%_favorable_Q14', 'avg_score_Q15', '%_favorable_Q15', 
                    'avg_score_Q16', '%_favorable_Q16', 'avg_score_Q17', '%_favorable_Q17', 'avg_score_Q18', '%_favorable_Q18', 
                    'avg_score_Q19', '%_favorable_Q19', 'avg_score_Q20', '%_favorable_Q20', 'avg_score_Q21', '%_favorable_Q21', 
                    'avg_score_Q22', '%_favorable_Q22', 'avg_score_Q23', '%_favorable_Q23', 'avg_score_Q24', '%_favorable_Q24', 
                    'avg_score_Q25', '%_favorable_Q25', 'avg_score_Q26', '%_favorable_Q26', 'avg_score_Q27', '%_favorable_Q27', 
                    'avg_score_Q28', '%_favorable_Q28', 'avg_score_Q29', '%_favorable_Q29', 'avg_score_Q30', '%_favorable_Q30', 
                    'avg_score_Q31', '%_favorable_Q31', 'avg_score_Q32', '%_favorable_Q32', 'avg_score_Q33', '%_favorable_Q33', 
                    'avg_score_Q34', '%_favorable_Q34', 'avg_score_Q35', '%_favorable_Q35', 'avg_score_Q36', '%_favorable_Q36', 
                    'avg_score_Q37', '%_favorable_Q37', 'avg_score_Q38', '%_favorable_Q38',
                    'Satisfy_score1', 'Satisfy_score2','Manager_score', 'Belonging_score']

final_df = final_df[columns_in_order]
final_df.to_csv(r".\score_overall_nonduplicate.csv", index=False)

final_df = final_df.loc[final_df.index.repeat(final_df['number of employee'])].reset_index(drop=True)
final_df.to_csv(r".\score_overall_duplicate.csv", index=False)

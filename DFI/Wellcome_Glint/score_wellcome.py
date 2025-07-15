import pandas as pd
import numpy as np

df = pd.read_csv(r".\train_wellcome.csv")
df['Store'] = df['Store'].replace('R01-A02-Luckifast?? Building', 'R01-A02-Luckifast Building')

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
    'Manager Level 4': 'ML_4', 
    'FTE' : 'FTE'
})[['store_name', 'question_no', 'avg_score', '%_favorable', 'overall_response_rate(%)','overall_respondents', 'ML_7','ML_6', 'ML_5', 'ML_4', 'FTE']]


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
store_metrics = df_prep.groupby('store_name')[['overall_respondents','overall_response_rate(%)', 'ML_7','ML_6', 'ML_5', 'ML_4', 'FTE']].first().reset_index()


# Generate ordered columns
questions = sorted(df_prep['question_no'].unique(), key=int)
ordered_columns = [store_col, 'overall_respondents', 'overall_response_rate(%)', 'ML_7','ML_6', 'ML_5', 'ML_4', 'FTE']


# Add score
# Add score
for q in questions:
    ordered_columns.extend([
        f"avg_score_{q}",
        f"%_favorable_{q}"
    ])

# Create final dataframe
final_df = pivot_df.merge(store_metrics, on=store_col, how='left')[ordered_columns]

final_df.columns = [
    col.replace('_score_', '_score_Q').replace('_favorable_', '_favorable_Q')
    if any(x in col for x in ['score', 'favorable']) else col
    for col in final_df.columns
]



#DV score setting
final_df['Satisfy_score1'] = (final_df['avg_score_Q1']+final_df['avg_score_Q2'])/2
final_df['Satisfy_score2'] = (final_df['avg_score_Q1']+final_df['avg_score_Q3'])/2
final_df['Manager_score'] = final_df['avg_score_Q7']
#No Q34 and Q35 in store
final_df['Belonging_score'] = (final_df['avg_score_Q5']+final_df['avg_score_Q15']+final_df['avg_score_Q16'] +final_df['avg_score_Q6']) / 4 

final_df = final_df.iloc[1:, ]
final_df["Store"] = final_df["store_name"].map(lambda p: str(p).split("-")[2].split(" - ")[0].strip())
final_df.to_csv(r".\score_wellcome.csv", index = False)





# Read from Sales DataFrame
store_info_df = pd.read_excel(r".\financial.xlsx", sheet_name=0)
sales_23_24_df = pd.read_excel(r".\financial.xlsx", sheet_name=1)
sales_25_df = pd.read_excel(r".\financial.xlsx", sheet_name=2)
customer_23_24_df = pd.read_excel(r".\financial.xlsx", sheet_name=3)
customer_25_df = pd.read_excel(r".\financial.xlsx", sheet_name=4)

merge_key = ["6-digit", "5-digit", "SAP Code", "Store", "Format"]
sales_df = sales_23_24_df.merge(sales_25_df, how="inner", on=merge_key)
sales_df.columns = [col if not col.startswith("202") else "sales_"+col for col in sales_df.columns]

customer_df = customer_23_24_df.merge(customer_25_df, how="inner", on=merge_key)
customer_df.columns = [col if not col.startswith("202") else "customer_"+col for col in customer_df.columns]

sales_customer_df = sales_df.merge(customer_df, how="inner", on=merge_key) 


sales_customer_df["sales_sum"] = sales_customer_df[['sales_2023 9', 'sales_2023 10', 'sales_2023 11', 'sales_2023 12', 'sales_2024 1',
                                                       'sales_2024 2', 'sales_2024 3', 'sales_2024 4', 'sales_2024 5', 'sales_2024 6', 
                                                       'sales_2024 7', 'sales_2024 8']].sum(axis=1)
sales_customer_df["customer_sum"] = sales_customer_df[['customer_2023 9', 'customer_2023 10', 'customer_2023 11', 'customer_2023 12',
                                                          'customer_2024 1', 'customer_2024 2', 'customer_2024 3', 'customer_2024 4', 
                                                          'customer_2024 5 ', 'customer_2024 6', 'customer_2024 7', 'customer_2024 8']].sum(axis=1)

financial_df = store_info_df.merge(sales_customer_df, how="left", on=["Store", "6-digit", "5-digit", "SAP Code"])
replace_dict = {
    "Allway Garden" : "Allway Gardens", 
    "Cheung Hang Estate" : "Cheung Hang", 
    "Chun Seen Mei Est" : "Chun Seen Mei", 
    "Chung Hwa" : "Chung Hwa Plaza", 
    "Fitford" : "Fitfort",
    "Hoi Fu Superstore" : "Hoi Fu Court", 
    "Hung Hom Superstore" : "Hung Hom", 
    "Hunghom Centre" : "Hung Hom Centre", 
    "Ka Wo Building" : "Ka Wo", 
    "Kam Wing Street" : "Kam Wing", 
    "Kwai Chung Shopping Center" : "Kwai Chung", 
    "Mei Foo Superstore" : "Mei Foo 2", 
    "Metropole Superstore" : "Metropole",
    "On Kay Court" : "On Kay", 
    "Panorama" : "The Panorama", 
    "PopWalk" : "Popwalk", 
    "Prince Edward Road" : "Prince Edward",
    "Richland Garden" : "Richland Gardens",
    "Sau Mau Ping Estate" : "Sau Mau Ping Plaza", 
    "Sheung Shui Superstore" : "Sheung Shui", 
    "Smithfield Road" : "Smithfield Terrace", 
    "Tin Chak Supstore" : "Tin Chak", 
    "Wanchai" : "Wan Chai", 
    "Winner Centre" : "Winner", 
    "Yaumati" : "Yaumatei", 
    "DDC Westwood" : "Westwood", 
    "Elements (ThreeSixty)" : "Elements", 
    "Nexxus Building MPJ" : "Nexxus Building", 
    "Olivers - Prince's Building" : "Prince Building", 
    "Langham Place MPJ" : "Langham Place", 
    "Surson Building Mini MPJ" : "Surson Building", 
    "Perkins Road MPJ" : "Perkins Road", 
    "Tin Hau Temple Road" : "Tin Hau", 
    "No 8 Garden" : "No.8 Garden", 
    "PopCorn" : "Popcorn", 
    "Telford Plaza MPJ" : "Telford Plaza", 
    "Park Signature" : "Park Signature MP (WEHK", 
    "Kam Wing Street" : "Kam Wing",
    "Sun Kwai Hing Garden" : "Sun Kwai Hing", 
}
financial_df["Store"] = financial_df["Store"].replace(replace_dict)
#Shrinkage
shrinkage_df = pd.read_excel(r".\Known and Unknown Shrinkage by store - E.xlsx")
shrinkage_df["Total Shrinkage Sum"] = shrinkage_df["Known Shrinkage Sum"] + shrinkage_df["Unknown Shrinkage Sum"]
shrinkage_df = shrinkage_df[["SAP Code", "Centre","District", "Region", "Store", "Format", "Total Shrinkage Sum"]]
shrinkage_df.rename(columns={"Centre":"5-digit"}, inplace=True)
replace_dict2 = {
    "Fullagar Industrial Building" : "Fullagar", 
    "The Westwood" : "Westwood",
    "Lei Yue Mun" : "Lei Yue Mun Plaza",
    "Sau Mau Ping" : "Sau Mau Ping Plaza", 
    "Kimberly Road" : "Kimberley Road", 
    "Yan On" : "Yan On Building", 
    "Fairway Garden" : "Fair Way Garden", 
    "NING YUEN STREET" : "Ning Yuen Street", 
    "Shun Ning" : "Shun Ning Road", 
    "Kwai Chung S'store" : "Kwai Chung", 
    "Mayfair Garden" : "Mayfair Gardens", 
    "Tivoli Garden" : "Tivoli Garden 2", 
    "Chung On" : "Chung On Estate", 
    "Tsui Lai" : "Tsui Lai Garden", 
    "Cheung Fat Building" : "Cheung Fat", 
    "Golden Plaza" : "Golden Plaza 1", 
    "Ping Wui" : "Ping Wui Centre", 
    "Lions Rise MPJ" : "Lions Rise", 
    "No. 8 Garden" : "No.8 Garden", 
    "Koway II" : "Ko Way 2",
    "Allway Garden" : "Allway Gardens", 
    "Elements (ThreeSixty)" : "Elements", 
    "Hunghom Centre" : "Hung Hom Centre",
    "K-11" : "K11",
    "Ka Wo Building" : "Ka Wo", 
    "Metro Pole" : "Metropole",
    "Olivers - Prince's Building" : "Prince Building", 
    "On Kay Court" : "On Kay", 
    "Park Signature" : "Park Signature MP (WEHK",
    "Park Vale " : "Park Vale",
    "PopCorn" : "Popcorn", 
    "PopWalk" : "Popwalk",  
    "Prince Edward Road" : "Prince Edward",
    "Richland Garden" : "Richland Gardens",
    "Sheung-Shui " : "Sheung Shui", 
    "Smithfield Road" : "Smithfield Terrace", 
    "Tak Man " : "Tak Man",
    "Wanchai" : "Wan Chai", 
    "Yaumati" : "Yaumatei",
    "The Edge" : "Popcorn"}
shrinkage_df["Store"] = shrinkage_df["Store"].replace(replace_dict2)
shrinkage_df = financial_df.merge(shrinkage_df, how="left", on=["SAP Code","5-digit", "Store"])
shrinkage_df= shrinkage_df[["Store", "Total Shrinkage Sum"]]

SOP_df = pd.read_excel(r".\SOP by store 2023-2025.xlsx", sheet_name=0)
SOP_df = SOP_df.rename(columns={
    'No': "5-digit",
    'EXISTING': 'Store',
    'SOP_sum':'SOP_sum'})[['5-digit', 'Store', 'SOP_sum']]
SOP_df["Store"] = SOP_df["Store"].replace(replace_dict)

replace_dict = {
    "Fullagar Industrial Building" : "Fullagar", 
    "The Westwood" : "Westwood",
    "Lei Yue Mun" : "Lei Yue Mun Plaza",

    "Sau Mau Ping" : "Sau Mau Ping Plaza", 
    "Kimberly Road" : "Kimberley Road", 
    "Yan On" : "Yan On Building", 
    "Fairway Garden" : "Fair Way Garden", 
    "NING YUEN STREET" : "Ning Yuen Street", 
    "Shun Ning" : "Shun Ning Road", 
    "Kwai Chung S'store" : "Kwai Chung", 
    "Mayfair Garden" : "Mayfair Gardens", 
    "Tivoli Garden" : "Tivoli Garden 2", 
    "Chung On" : "Chung On Estate", 
    "Sheung Shui S'store" : "Sheung Shui", 
    "Tsui Lai" : "Tsui Lai Garden", 
    "Cheung Fat Building" : "Cheung Fat", 
    "Golden Plaza" : "Golden Plaza 1", 
    "Ping Wui" : "Ping Wui Centre", 
    "Lions Rise MPJ" : "Lions Rise", 
    "No. 8 Garden" : "No.8 Garden", 
    "Koway II" : "Ko Way 2"
}
SOP_df["Store"] = SOP_df["Store"].replace(replace_dict)

financial_SOP_df = financial_df.merge(SOP_df, how="left", on=["Store"]).rename(columns={"5-digit_x":"5-digit"}).drop(labels=["5-digit_y"], axis=1)

turnover_df = pd.read_excel(r".\cleaned_yearly_turnover.xlsx", sheet_name=0)
turnover_df["2023 Turnover"].drop(labels=0, axis=0, inplace=True)
turnover_df.rename(columns={"Store":"store_name"}, inplace=True)

msp_2023_df = pd.read_excel(r".\(For internal use) 2023 Yearly WE_MSP_All Regions.xlsx", sheet_name=0)
msp_2024_df = pd.read_excel(r".\(For internal use) 2024 Yearly WE_MSP_All Regions.xlsx", sheet_name=0)
msp_2023_df = msp_2023_df[["Region", "Area", "Store Code", "Store Name", "2023 MSP_sum"]]
msp_2024_df = msp_2024_df[["Region", "Area", "Store Code", "Store Name", "Format", "2024 MSP_sum"]]
msp_2023_df.rename(columns={"Store Name":"Store"}, inplace=True)
msp_2024_df.rename(columns={"Store Name":"Store"}, inplace=True)
replace_dict = {
    "Wanchai " : "Wan Chai", 
    "Ka Wo Building" : "Ka Wo",
    "PopWalk" : "Popwalk",
    "Mayfair" : "Mayfair Gardens", 
    "Sheung-Shui " : "Sheung Shui", 
    "Tsui Lai" : "Tsui Lai Garden", 
    "Ping Wui" : "Ping Wui Centre", 
    "Oceanic Heights" : "Oceania Heights", 
    "360 Elements " : "Elements", 
    "Olivers - Prince's Building" : "Prince Building", 
    "No. 8 Garden" : "No.8 Garden", 
    "PopCorn " : "Popcorn",  
    "Park Signature " : "Park Signature MP (WEHK"
}
msp_2023_df["Store"] = msp_2023_df["Store"].replace(replace_dict).map(lambda p: str(p).strip())
msp_2024_df["Store"] = msp_2024_df["Store"].replace(replace_dict).map(lambda p: str(p).strip())

score_df = pd.read_csv(r".\score_wellcome.csv")
score_df = score_df.merge(financial_SOP_df, how="left", on="Store").drop(labels=["Region", "District"], axis=1)
score_df = score_df.merge(turnover_df, how="left", on="store_name")
score_df = score_df.merge(msp_2024_df, how="left", on="Store")
score_df = score_df.merge(msp_2023_df, how="left", on="Store")
score_df = score_df.merge(shrinkage_df, how="left", on="Store")


exclude_stores = ["R01-A03-DDC Westwood", "R02-A06-Hing Tin 2", "R02-A09-Tung Tau Estate"]
score_df = score_df[~score_df["store_name"].isin(exclude_stores)]
score_df["productivity_sum"] = (score_df["sales_sum"] /  score_df["Trading Area sq.ft"])
score_df["2023 Turnover"] = score_df["2023 Turnover"] *100
score_df['MSP_sum'] = score_df['2023 MSP_sum'] + score_df['2024 MSP_sum']
score_df['number of employee'] = (score_df['overall_respondents'] / score_df['overall_response_rate(%)'] * 100).round().astype(int)
score_df['workload'] = score_df['Trading Area sq.ft'] / score_df['FTE'] 
score_df['SOP / T Area'] = score_df['SOP_sum'] / score_df['Trading Area sq.ft']
score_df['SOP / Sales'] = score_df['SOP_sum'] / score_df["sales_sum"]
score_df['shrinkage `%` of Sales'] = (score_df['Total Shrinkage Sum']/score_df['sales_sum'])*100
score_df.drop(labels=['overall_respondents', 'overall_response_rate(%)', '2023 MSP_sum', '2024 MSP_sum'], axis=1, inplace=True)

columns_in_order = ['store_name', '6-digit', '5-digit', 'SAP Code', 'Format_x', 'Format_y', 'Store Cluster', 'Gross Area sq.ft', 'Trading Area sq.ft',  
                    'number of employee', 'FTE', 
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
                    'sales_sum', 'customer_sum', 
                    'SOP_sum', 'SOP / T Area', 'SOP / Sales', '2023 Turnover',
                    'MSP_sum', 'workload', 'shrinkage `%` of Sales',
                    'productivity_sum', 
                    'Satisfy_score1', 'Satisfy_score2','Manager_score', 'Belonging_score']

score_df = score_df[columns_in_order]
score_df.to_csv(r".\score_wellcome_nonduplicate.csv", index=False)

score_df = score_df.loc[score_df.index.repeat(score_df['number of employee'])].reset_index(drop=True)
score_df.to_csv(r".\score_wellcome_duplicate.csv", index=False)
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Dec  7 09:35:00 2024

@author: karolinematildekruse
"""

## DICTIONARY MODEL

import pandas as pd

################## BREDERE DICTIONARY #################
# Load the dataset
file_path = '/Users/karolinematildekruse/Desktop/small_dataset512.xlsx'
data = pd.read_excel(file_path)

# Define the dictionary of search terms categorized by topics
search_terms = {
    1: [
        # General identity politics
        "identitetspolitik", "kønsminoriteter", "seksuelle minoriteter",
        "etniske minoriteter", "personer med handicap", "kvinder",
        # Historical and structural injustice
        "slaveejere", "slavegjorte personer", "migrantbaggrund",
        "oprindelige befolkningsgrupper", "sprog og kultur", "race-baserede-ord",
        "koloniale symboler", "kulturel appropriation", "religiøse traditioner",
        "religiøse højtider", "politibrutalitet", "racisme", "systematisk racisme",
        "ulighed i muligheder", "religiøse beklædningsgenstande", "religiøse skoler",
        "kolonialisme", "anderledes historiefortælling",
        # LGBTQ+ and gender identity
        "LGBTQ+", "transkønnede", "nonbinære", "interkønnede", "homoseksuelle", 
        "biseksuelle", "panseksuelle", "queer-personer", "flydende kønsidentitet",
        "pronomener", "kønsneutralitet", "hadforbrydelser", "repræsentation",
        "kønsskifteoperationer", "kønsopdelte rum", "toiletter", "omklædningsrum",
        "sport", "normkritisk seksualundervisning", "opfattelse af køn", 
        "definitioner af køn", "flydende køn",
        # Women's rights
        "kønskvoter", "ligeløn", "øremærket barsel", "glasloftet", "MeToo",
        "kønsroller", "retten til abort", "seksuel chikane", "bekæmpelse af seksuel chikane",
        # Danish-specific identity politics events
        "Rigsfællesskabet", "Grønland", "Færøerne", "kolonitiden", "statuer", 
        "Vestindiske Øer", "normstormerne", "racismeparagraffen",
        "Pernille Vermund", "repatriering", "hadprædikanter", "ghettopakken",
        "asylstramninger", "burkaforbud", "imamlov", "håndtrykslov", "pligt til at give hånd",
        "smykkeloven", "Danmarks koloniale historie", "normkritik", "islamofobi",
        "anti-muslimske holdninger", "udlændingestramninger", "flygtningedebat",
        "identitetsdagsorden", "sexisme i Folketinget", "kvinders repræsentation",
        "greenwashing", "klimapolitik som social retfærdighed"
    ]
}


# Function to assign categories based on the presence of search terms
def categorize_text(text, terms_dict):
    if pd.isna(text):  # Skip NaN entries
        return None
    text = str(text).lower().strip()  # Normalize text: lowercase and strip spaces
    for category, terms in terms_dict.items():
        # Normalize search terms and check if any term is in the text
        if any(term.lower().strip() in text for term in terms):
            return category
    return None

# Apply the function to create a new column with the category
data['category'] = data['text'].apply(lambda x: categorize_text(x, search_terms))

# Save or display the updated dataset
print(data)

# Optionally save the updated dataset to a new Excel file
data.to_excel('/Users/karolinematildekruse/Desktop/dataset_with_categories.xlsx', index=False)


#### Matching rows: 
# Load the updated dataset
file_path = '/Users/karolinematildekruse/Desktop/dataset_with_categories.xlsx'
data = pd.read_excel(file_path)

# Filter the dataset to find rows where both '100' and 'category' have the value 1
matching_rows = data[(data[100] == 1) & (data['category'] == 1)]

# Count the number of matching rows
matching_count = matching_rows.shape[0]

# Print the count
print(f"Number of observations with 1 in both '100' and 'category': {matching_count}")

# Optionally display the matching rows
print(matching_rows)

## MISMATCH

# Filter the dataset to find rows where '100' is NA and 'category' is 1
mismatch_rows = data[data[100].isna() & (data['category'] == 1)]

# Count the number of such rows
mismatch_count = mismatch_rows.shape[0]

# Print the count
print(f"Number of observations where '100' is NA and 'category' is 1: {mismatch_count}")

# Optionally display the mismatched rows
print(mismatch_rows)

from sklearn.metrics import precision_recall_fscore_support, accuracy_score
from tabulate import tabulate

# Load the updated dataset
file_path = '/Users/karolinematildekruse/Desktop/updated_dataset_with_categories.xlsx'
data = pd.read_excel(file_path)

# Prepare true labels and predictions
# Assuming column '100' is the true label and 'category' is the predicted label
true_labels = data[100].fillna(0).astype(int)  # Replace NaN with 0 and convert to integer
predictions = data['category'].fillna(0).astype(int)  # Replace NaN with 0 and convert to integer

# Calculate precision, recall, F1-score, and support for each class
precision, recall, f1, support = precision_recall_fscore_support(true_labels, predictions, labels=[1, 0])

# Calculate overall accuracy
accuracy = accuracy_score(true_labels, predictions)

##### Exampels where the dictionary is wrong: 
# Filter the dataset to find rows where '100' is 1 but 'category' is NaN
missing_category_rows = data[(data[100] == 1) & (data['category'].isna())]

# Save or display the filtered dataset
missing_category_rows.to_excel('/Users/karolinematildekruse/Desktop/missing_category_rows.xlsx', index=False)
print(f"Number of observations where '100' is 1 and 'category' is NA: {missing_category_rows.shape[0]}")

# Optionally display the first few rows of the filtered data
print(missing_category_rows.head())


# Create a detailed table
results = {
    "Evalueringsmål": ["Precision", "Recall", "F1-score", "Support"],
    "Identitetspolitik (Ordbogsmodel)": [precision[0], recall[0], f1[0], support[0]],
    "Ikke identitetspolitik (Ordbogsmodel)": [precision[1], recall[1], f1[1], support[1]],
}

# Add overall accuracy as a separate row
accuracy_row = {
    "Evalueringsmål": "Accuracy",
    "Identitetspolitik (Ordbogsmodel)": accuracy,
    "Ikke identitetspolitik (Ordbogsmodel)": "-",
}
results_df = pd.DataFrame(results)
results_df = pd.concat([results_df, pd.DataFrame(accuracy_row, index=[0])], ignore_index=True)

# Display the table
print(results_df)

# Save the table as LaTeX
latex_table = tabulate(
    results_df,
    headers="keys",
    tablefmt="latex",
    showindex=False,
    floatfmt=".2f"
)
print(latex_table)



  

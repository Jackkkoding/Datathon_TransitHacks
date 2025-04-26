import pandas as pd
import numpy as np

# Load the demographic data
demographic_data = pd.read_csv("C:/Users/zhwja/Desktop/chicago_demographics_summary.csv")

# Define two example advertisement profiles
ads_profiles = {
    'Luxury Watch': {
        'target_income': 120000,  # High income
        'preferred_race': ['White', 'Asian'],
        'max_household_size': 2.0,
        'target_gender_ratio': 1.0,  # Balanced
        'target_age_category': 'Middle-aged Adults (40-59)',
        'weights': {'income': 0.4, 'race': 0.2, 'household': 0.2, 'gender': 0.1, 'age': 0.1}
    },
    'Budget Grocery': {
        'target_income': 50000,  # Lower-middle income
        'preferred_race': ['Hispanic', 'Black'],
        'min_household_size': 2.5,
        'target_gender_ratio': 1.0,  # Balanced
        'target_age_category': 'Young Adults (20-39)',
        'weights': {'income': 0.3, 'race': 0.2, 'household': 0.3, 'gender': 0.1, 'age': 0.1}
    }
}

# Function to calculate quality scores
def calculate_quality_scores(df, ad_profile):
    scores = []
    for idx, row in df.iterrows():
        # Income score
        if ad_profile.get('target_income', 0) > 0:
            income_score = min(1, row['median_income'] / ad_profile['target_income'])
        else:
            income_score = 1

        # Race score
        race_score = 1 if row['dominant_race'] in ad_profile['preferred_race'] else 0.5

        # Household size score
        if 'max_household_size' in ad_profile:
            household_score = min(1, ad_profile['max_household_size'] / row['avg_household_size'])
        elif 'min_household_size' in ad_profile:
            household_score = min(1, row['avg_household_size'] / ad_profile['min_household_size'])
        else:
            household_score = 1

        # Gender ratio score (closer to 1 is better)
        gender_score = 1 - abs(row['gender_ratio'] - ad_profile['target_gender_ratio'])

        # Age category score
        age_score = 1 if row['dominant_age_category'] == ad_profile['target_age_category'] else 0.5

        # Weighted sum
        final_score = (
            ad_profile['weights']['income'] * income_score +
            ad_profile['weights']['race'] * race_score +
            ad_profile['weights']['household'] * household_score +
            ad_profile['weights']['gender'] * gender_score +
            ad_profile['weights']['age'] * age_score
        )
        
        scores.append(final_score)
    return scores

# Calculate quality scores for each ad
demographic_data['Luxury_Watch_Score'] = calculate_quality_scores(demographic_data, ads_profiles['Luxury Watch'])
demographic_data['Budget_Grocery_Score'] = calculate_quality_scores(demographic_data, ads_profiles['Budget Grocery'])

# Output the scores for inspection
demographic_data[['zip_code', 'Luxury_Watch_Score', 'Budget_Grocery_Score']].sort_values(by='Luxury_Watch_Score', ascending=False)

# Select only the columns you want to export
score_list = demographic_data[['zip_code', 'Luxury_Watch_Score', 'Budget_Grocery_Score']]

# Export to a new CSV file
score_list.to_csv('C:/Users/zhwja/Desktop/chicago_advertisement_scores.csv', index=False)

print("Scores successfully exported!")

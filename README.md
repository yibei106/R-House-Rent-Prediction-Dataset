# Housing Rental Analysis

## Overview

This project analyzes a housing rental dataset containing over 4,700 rows and 12 columns to determine how various factors, including family background, neighborhood, and lifestyle, influence renters' decisions. The goal is to provide insights for decision-making on house rentals based on these factors.

## Dataset

The dataset consists of:

- **Rows**: 4,700+
- **Columns**: 12

## Objectives

1. **Data Import and Preprocessing**:
   - Import the dataset into R Studio.
   - Identify and analyze outliers (without removing them to avoid significant data loss).
   - Transform data for better readability, simplifying entries such as tenant types, points of contact, and area types.
   - Replace abnormalities in the area locality with the mode of the area.
   - Add two new columns to indicate the number of floors in a house and the specific floor being rented.

2. **Analysis Focus**:
   - Investigate factors affecting rental decisions for bachelors vs. families.
   - Analyze seasonal trends influencing rental choices.
   - Examine the importance of BHK configuration and its impact on rental pricing.
   - Conduct city-wise comparisons to identify the best options based on specific rental conditions.
   - Analyze the correlation between rental house sizes and other characteristics.
   - Study the influence of furnishing status on rental features.
   - Explore reasons for preferring remote areas over lively neighborhoods.
   - Investigate factors leading to the choice of renting basements.
  
## Key Findings

- **Preferences by Tenant Type**:
  - **Bachelors**: Prefer configurations like 1BHK and 2BHK, super area types, and cities with more semi-furnished houses.
  - **Families**: Tend to prefer 2BHK and 3BHK, carpet area types, and semi-furnished houses.

- **Best Months for Renting**:
  - **June and July**: These months have higher availability of houses, making them the best months for renting.
  - **April**: Rental options are more limited during this month.

- **Rent Trends**:
  - Rent increases with larger BHK configurations, with 2BHK being the most popular choice among renters.

- **City-Specific Insights**:
  - **Mumbai**: Has the highest rent range and the most rental options available.
  - **Kolkata**: Known for being the most affordable city for rentals.

- **Other Key Factors**:
  - **Number of Bathrooms**: Influences tenant preferences.
  - **Furnishing Status**: Semi-furnished houses are a common choice.
  - **Point of Contact**: Tenants tend to prefer dealing directly with owners or agents.

- **Area Type Preferences**:
  - **Super Area Houses**: Better for larger spaces, commonly preferred by families.
  - **Carpet Area Houses**: More suitable for smaller preferences, often chosen by bachelors.

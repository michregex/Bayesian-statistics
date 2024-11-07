# Malignant Melanoma Mortality Analysis in Europe

This repository contains the analysis and modeling of malignant melanoma mortality data across various European nations, using hierarchical regression modeling. The project explores the relationship between UVB exposure and melanoma mortality rates, identifying potential areas for intervention and policy development.

## Table of Contents
1. [Introduction](#introduction)
2. [Data Overview](#data-overview)
3. [Analysis Approach](#analysis-approach)
4. [Modeling Methodology](#modeling-methodology)
5. [Results and Conclusions](#results-and-conclusions)
6. [Dependencies](#dependencies)
7. [Usage](#usage)
8. [References](#references)

## Introduction
This analysis focuses on malignant melanoma, a severe form of skin cancer, examining its mortality rate across European regions from 1971 to 1980. By analyzing geographical variations and possible correlations with UVB exposure, this study aims to inform potential interventions to reduce mortality rates.

## Data Overview
Data used in this project includes:
- **Malignant Melanoma Mortality Rates**: Collected from the Atlas of Cancer Mortality within the European Economic Community, covering the years 1971-1980.
- **UVB Radiation Levels**: Measured UVB exposure data specific to each county within the analyzed European nations.

### Dataset Structure
The dataset has 354 rows and 6 columns, with information on:
- **Nation**: Country identifier (e.g., Belgium, W. Germany, etc.).
- **Region and County IDs**: Regional identifiers for each record.
- **Number of Male Deaths**: Recorded melanoma deaths among males.
- **Expected Deaths**: Estimated expected deaths.
- **UVB Dose**: Measured UVB radiation levels.

## Analysis Approach
An exploratory data analysis was performed to identify patterns and correlations. Key findings include:
- A strong positive correlation (0.87) between recorded and expected deaths.
- A mild negative correlation (-0.25) between melanoma deaths and UVB levels, indicating complex interactions beyond UV exposure.

## Modeling Methodology
This project uses a hierarchical regression model to account for the multilevel structure of the data, grouping counties within countries. The model assumes:
- **Poisson Distribution**: For the count of deaths.
- **Hierarchical Parameters**: Including UVB exposure as a predictive variable for mortality rates.

The model was implemented in Stan, with prior distributions informed by historical data.

### Stan Model Code
The main modeling script is provided in `stan_model.stan`, specifying likelihood functions and parameter distributions.

## Results and Conclusions
The findings reveal unexpected trends, where increased UVB exposure correlates with lower mortality in several regions. This could be due to additional factors, such as genetic resilience and preventive behaviors in sunnier countries. Key points:
- **Positive UVB-Mortality Correlation**: Observed in France, Ireland, and the Netherlands, suggesting potential risk with higher UVB.
- **Negative UVB-Mortality Correlation**: Found in other countries, possibly due to unaccounted variables like skin type and sun protection habits.

### Recommended Interventions
Based on high mortality rates, especially in Denmark, the study recommends focused awareness campaigns and preventive measures.

## Dependencies
- **Stan**: For hierarchical Bayesian modeling.
- **R** or **Python**: For running the statistical analysis and plotting results.
- **Libraries**: `rstan`, `ggplot2`, and other statistical libraries.

## Usage

1. **Run the Stan Model**:
   Load the `.stan` file in an environment that supports Stan, and execute the script following the instructions in the code comments.

2. **Analyze Results**:
   Use the provided R or Python scripts to visualize and interpret the model’s output.


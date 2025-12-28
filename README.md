# A STUDY ON REGIONAL AND SECTORAL DYNAMICS OF THE UK ENTERPRISE DEMOGRAPHY (2019-2024)
## This project examines trends in football tournaments, focusing on the geographical distribution of matches, match outcomes, scoring patterns, and regional influences. 
It leverages datasets from Kaggle to analyze key variables and patterns across top international tournaments.

### Research Questions
1.	How do enterprise births, deaths, total net change, and churn rates compare for different regions in the UK from 2019 to 2024, and to what extent can a regression model accurately forecast enterprise births in 2024 and the total net enterprise change in 2025?
2.	How different are the enterprise survival rates by sector (2019 cohort, 1-to-5-year survival rate) and geographic area (2019–2023 cohorts, 1-year survival rate) in the UK?

### Analytical Techniques
1. Exploratory Data Analysis (EDA)
- Trend analysis of births, deaths, and net change in enterprises (2019-2024) by UK regions.
- Multi-line time-series graphs 
- Bar chart/rankings for regions of selected years
- Data summary tables showing the regions with the highest/lowest values

2. Churn Analysis
- Calculation of churn-type indicators with data about births and deaths
- Regional bar charts
- Choropleth mapping 

3. Regression Forecasting (baseline)
- Train: 2019-2023
- Test:2024 (Assessed by Absolute Percentage Error, APE)
- Forecast: 2025 (net enterprise change)
- It is used as a transparent baseline model rather than a best possible method for prediction.

4. Survival Analysis
- Sector survival: Survival rates of the 2019 cohort from 1-5 years (Table 4.2).
- Geographic survival: 1-year survival for cohorts 2019-2023 by region (Table 4.1).  
- Graphic comparisons and summaries of rankings to show variability and extremeness.

### Setup Instructions
1. Prerequisites
Ensure the following are installed:
- R: Version 4.0+
- RStudio
- Git (optional, for version control)
2. Install R and R Studio
  - Download https://posit.co/download/rstudio-desktop/ (Windows/macOS).
  - Use your package manager (Linux).
3. Clone the Repository
- Open the `IJC437-Assignment` file in your RStudio working directory.
- To clone the repository from GitHub: git clone https://github.com/tsridhara1/IJC437-Assignment.git
4. Run Analysis Scripts
- RQ1.R
- RQ2.R

### Key Insights
RQ1:
- Data for enterprise births and deaths reveal geographical variation in the regions of 2019-2024 with higher absolute values in regions like London and South East.
- Churn rates point out areas that have high levels of “business dynamism,” which entails higher levels of births and deaths than the baseline, while other regions have lower churn rates compared to others.
- Regression baseline model trained on the period 2019 to 2023 is likely to offer reasonable predictive power for 2024 birth rates for different regions but certainly is no substitute for a full-blown predictive model.
- The 2025 net change forecasts are particularly model-sensitive to the short training period and structural changes that have been observed.

RQ2:
- Overall survival rates show a large gap between sectors (2019 cohort, 1-5 year survival). This is reflected in a significant variation in overall survival rates of businesses across sectors.
- These regional survival rates over a year (cohorts between 2019 and 2023) are less dispersed than the survival rates in a sector, though they also include

### Limitations
1. The data is taken from officially published tables. The variables are limited by what is published in the ONS format.
2. Simple regression forecasting functions as a benchmark and might not be able to handle policy shocks or nonlinear events.
3. Cross regional comparisons may be impacted by population and business number differences, while rate comparisons could be necessary for certain interpretations.

### Future Work
1. Compare the baseline regression to time series models (such as ETS/ARIMA) and assess whether enhancements are worth added complexity.
2. Add rate-based indicators such as number of births per active enterprises to facilitate comparisons.
3. Enhance sector profilers for survival to incorporate patterns of births and deaths to survival results for more refined “enterprise dynamics” segmentation.
4. Provide functionality to automate the ingestion of datasets and validation of tables.









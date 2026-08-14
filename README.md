# UK Brexit Voting Analysis

A statistical modelling project analysing ward-level voting patterns in the 2016 UK EU Referendum using demographic and socioeconomic data from 803 UK wards.

## Project Overview

The objective was to identify key factors associated with Leave voting and develop a statistical model to predict Leave support in wards with missing referendum outcomes.

The analysis combines exploratory data analysis, feature selection, Generalised Linear Models, hierarchical clustering, interaction analysis and model diagnostics.

## Tools & Methods

- R
- Generalised Linear Models (GLM)
- Quasibinomial regression
- Hierarchical clustering
- Interaction terms
- ANOVA F-tests
- Exploratory Data Analysis
- Model diagnostics
- Statistical visualisation

## Methodology

The project began with exploratory analysis of demographic, socioeconomic and regional variables, including education, age, ethnicity, housing, unemployment, deprivation and social grade.

Highly correlated variables were removed to reduce multicollinearity and improve interpretability.

A binomial GLM framework was initially considered, but substantial overdispersion led to the use of a quasibinomial model. Logit, probit and complementary log-log link functions were compared, with the logit specification selected.

The original nine UK regions were grouped into five clusters using hierarchical clustering based on estimated regional coefficients. Region-specific interaction effects were then tested using ANOVA F-tests.

## Key Results

- Final model explained approximately **92.97% of the null deviance**.
- Education, ethnicity, unemployment, population density and social grade showed strong associations with Leave voting.
- Several effects varied significantly across regional groups, motivating the inclusion of interaction terms.
- The final model was used to predict Leave vote proportions for **267 wards** with missing outcomes.
- Residual and influence diagnostics were conducted to assess model validity and identify influential observations.

## Repository Contents

- `brexit_voting_analysis.R` — complete R analysis and modelling pipeline
- `brexit_voting_analysis_report.pdf` — full project report

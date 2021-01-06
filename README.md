# covid19bxl
This repository contains the data analysis scripts (in R) used for the manuscript "Reducing contacts to stop SARS-CoV-2 transmission during
the second pandemic wave in Brussels, Belgium", available as preprint: https://www.medrxiv.org/content/10.1101/2020.12.23.20248795v1 

3 datasets from the Brussels region were used:
- contacts reported by reported cases through contact tracing: date and index case ID
- daily reported cases numbers 
- primary-secondary case pairs (suspected transmission events): index cases and their reported contacts which are reported as a case within 3 weeks after the date of last reported contact

7 scripts are used for data cleaning and analysis: 
1. the reported case numbers and age group fractions (Fig 1) and percentage of the population which was SARS-CoV-2 confirmed by age group (Supp Fig 1): bxl_case numbers_epicurve_20201218.R
2. the daily/weekly mean number of contacts reported per SARS-CoV-2 case with linear regression model (Fig 2, Supp Fig 2): lin_rgr_contacts_20201218.R
3. estimated instantaneous reproduction number (Rt) based on daily reported cases (Fig 3): rt_estim_20201218.R
4. transmission matrix between index and secondary cases of all identified transmission events (Fig 4): descriptives_20201218.R
5. testing rate by age group (Supp Fig 3): age_specific_test rates_20201218.R
6. linear regression between number of contacts and Rt: lin_rgr_rt_20201218.R
7. Poisson regression of the fraction 10-19 years olds among  SARS-CoV-2 cases in Brussels before and after school opening: poisson_rgr_20201218.R

# NFHS-5 Maharashtra Child Anaemia Analysis

## Overview

This project examines anaemia among children aged 6–59 months in the National Family Health Survey-5 (NFHS-5) Maharashtra Kids Recode (KR) dataset. The R workflow estimates survey-weighted prevalence and 95% confidence intervals across selected subgroups and fits a survey-weighted multivariable logistic regression model.

## Research question

What is the prevalence of anaemia among children aged 6–59 months in the NFHS-5 Maharashtra dataset, and how does anaemia vary by child age and selected maternal, household, and demographic characteristics?

## Objectives

- Estimate survey-weighted anaemia prevalence overall and across subgroups.
- Report 95% confidence intervals for prevalence estimates.
- Examine adjusted associations between anaemia and selected characteristics using survey-weighted logistic regression.
- Maintain a reproducible R workflow suitable for an epidemiology portfolio.

## Data and study population

- **Source:** NFHS-5 Maharashtra Kids Recode (KR) dataset.
- **Population:** Children aged 6–59 months with a valid anaemia classification.
- **Outcome:** `anemic = 1` for mild, moderate, or severe anaemia (`hw57` values 1–3); `anemic = 0` for not anaemic (`hw57` value 4). Other or missing classifications are treated as missing.
- **Survey design:** Individual sample weights (`v005 / 1,000,000`), primary sampling units (`v021`), and strata (`v022`) are included in the survey design.

The analytic sample size reported during the project workflow was **7,751 children**. Re-running the cleaned script will print the sample size calculated from the source data so this can be checked against the saved analysis.

## Variables

| Concept | NFHS variable | Use |
|---|---|---|
| Child age in months | `hw1` | Eligibility and age groups |
| Anaemia classification | `hw57` | Outcome derivation |
| Child sex | `b4` | Model covariate |
| Residence | `v025` | Model covariate |
| Maternal education | `v106` | Model covariate |
| Wealth quintile | `v190` | Model covariate |
| Mother's age | `v012` | Maternal age groups |
| Birth order | `bord` | Birth-order groups |
| Sample weight | `v005` | Survey weight |
| PSU | `v021` | Survey clustering |
| Stratum | `v022` | Survey stratification |

## Methods

1. Import the KR dataset with `haven`.
2. Restrict the analytic population to children aged 6–59 months with valid anaemia status.
3. Define child age groups (6–23, 24–35, 36–47, and 48–59 months), maternal age groups, and birth-order groups.
4. Estimate weighted prevalence and 95% confidence intervals using the `survey` package.
5. Fit a survey-weighted logistic regression model with child age group, sex, maternal age, maternal education, wealth quintile, residence, and birth order as covariates.
6. Export tables and a prevalence plot to the `results/` directory.

Reference categories for the multivariable model are: age 6–23 months, male, mother's age under 20 years, no maternal education, poorest wealth quintile, urban residence, and first-born child.

## Key findings from the completed analysis

- Overall survey-weighted anaemia prevalence was **69.9% (95% CI 68.3%–71.4%)**.
- Prevalence decreased across child age groups: **82.5%** at 6–23 months, **73.4%** at 24–35 months, **62.8%** at 36–47 months, and **56.5%** at 48–59 months.
- In the adjusted model, older child age groups had lower odds of anaemia than children aged 6–23 months. The previously reported adjusted odds ratios were 0.61 for ages 24–35 months, 0.37 for ages 36–47 months, and 0.29 for ages 48–59 months.
- The previously reported model also showed lower adjusted odds for the poorer, middle, richer, and richest wealth quintiles compared with the poorest quintile. Interpret all estimates in light of the observational design and the specified model.

These figures are transcribed from the project's completed analysis. Re-run the cleaned script against the same authorized dataset before treating the outputs as a final, independently reproduced result. The script uses the survey design and should be treated as the source for regenerated output tables.

## Repository structure

```text
NFHS_Anemia_Project/
├── NFHS_Anemia_Project.Rproj
├── NFHS_Anemia_Analysis_Clean.R
├── README.md
├── .gitignore
└── results/
    ├── tables/
    └── figures/
```

## Reproducibility

1. Open `NFHS_Anemia_Project.Rproj` in RStudio.
2. Place the authorized source file named `NFHS5-Maharashtra KR (2).dta` in the project root, or update `data_file` in the script to the local filename.
3. Install the required R packages if needed: `haven`, `dplyr`, `ggplot2`, `survey`, and `tibble`.
4. Run `NFHS_Anemia_Analysis_Clean.R` from top to bottom.
5. Review the generated CSV files and PNG figure under `results/`.

The source dataset is intentionally not included in the public repository. Do not upload NFHS microdata to GitHub unless the applicable data-use terms explicitly permit redistribution. Use aggregate outputs only where sharing is permitted.

## Limitations

- This is a cross-sectional analysis; associations should not be interpreted as causal effects.
- Results depend on the outcome definition, complete-case availability, variable coding, and model specification.
- Anaemia estimates are based on the NFHS-5 classification available in `hw57`; verify the survey documentation and measurement notes when preparing a formal report or publication.
- Survey-weighted regression estimates are adjusted associations, not proof of causality.

## Tools

R, RStudio, `haven`, `dplyr`, `ggplot2`, `survey`, and `tibble`.

## Author

Saswati  
Public Health | Epidemiology | Clinical Research | Medical Writing

# NFHS-5 Maharashtra Child Anaemia Analysis

## Overview

This project uses NFHS-5 Maharashtra Kids Recode (KR) data to examine
the prevalence of anaemia among children aged 6–59 months and assess
the association between child age group and anaemia status.

The analysis was conducted in R with a focus on reproducible
epidemiological analysis.

## Research Question

What is the prevalence of anaemia among children aged 6–59 months
in the NFHS-5 Maharashtra dataset, and how does anaemia vary across
child age groups?

## Objectives

1. Estimate anaemia prevalence across age groups.
2. Calculate 95% confidence intervals for prevalence estimates.
3. Assess the association between age group and anaemia.
4. Estimate odds ratios using logistic regression.
5. Develop a reproducible epidemiological analysis workflow in R.

## Data

Data source: National Family Health Survey-5 (NFHS-5), Maharashtra,
Kids Recode (KR) dataset.

The analysis uses children aged 6–59 months with valid anaemia
measurements.

## Methods

### Outcome

Anaemia was defined using the NFHS-5 anaemia classification:

- Severe anaemia
- Moderate anaemia
- Mild anaemia
- Not anaemic

For analysis, severe, moderate and mild anaemia were classified as
"anaemic", while children classified as "not anaemic" were classified
as "not anaemic".

### Age groups

Children were categorized into:

- 6–23 months
- 24–35 months
- 36–47 months
- 48–59 months

### Statistical analysis

The analysis included:

- Descriptive statistics
- Anaemia prevalence estimation
- 95% confidence intervals
- Chi-square test
- Odds ratios
- Logistic regression

The current results are unweighted analytical estimates intended
for methodological learning and demonstration.

## Key Findings

In the unweighted analysis, anaemia prevalence decreased across
increasing age groups:

- 6–23 months: 84.7%
- 24–35 months: 74.7%
- 36–47 months: 65.2%
- 48–59 months: 57.3%

Logistic regression using children aged 6–23 months as the reference
group showed progressively lower odds of anaemia in the older age
groups.

## Reproducibility

The analysis was conducted using R.

The repository contains:

- R analysis script
- Analysis result tables
- Visualizations
- Project documentation

The analysis script can be used to reproduce the reported results
when the appropriate source dataset is available.

## Important Limitation

NFHS is a complex survey incorporating sampling weights, clustering
and stratification. The estimates presented in this initial analysis
are unweighted and should therefore not be interpreted as official
population-representative estimates.

A subsequent stage of this project will incorporate the appropriate
NFHS survey design and sampling weights.

## Tools

- R
- RStudio
- dplyr
- haven
- ggplot2
- epitools

## Author

Saswati

Public Health | Epidemiology | Clinical Research | Medical Writing
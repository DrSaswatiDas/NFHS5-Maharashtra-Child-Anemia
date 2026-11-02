# NFHS-5 Maharashtra: Child anaemia analysis
# Reproducible survey-weighted analysis script
# Note: Keep the source microdata out of public GitHub repositories unless its
# terms of use explicitly permit redistribution.

# 1. Packages -----------------------------------------------------------------
required_packages <- c("haven", "dplyr", "ggplot2", "survey", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                               quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_packages) > 0) {
  stop("Install the following packages before running this script: ",
       paste(missing_packages, collapse = ", "))
}

library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(tibble)

# 2. Paths and output folders --------------------------------------------------
data_file <- "NFHS5-Maharashtra KR (2).dta"
if (!file.exists(data_file)) {
  stop("Cannot find ", data_file,
       ". Place the authorized NFHS-5 Maharashtra KR file in the project root.")
}

dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

# 3. Import and prepare data ---------------------------------------------------
nfhs <- read_dta(data_file)

required_vars <- c("hw1", "hw57", "b4", "v025", "v106", "v190",
                   "v012", "bord", "v005", "v021", "v022")
missing_vars <- setdiff(required_vars, names(nfhs))
if (length(missing_vars) > 0) {
  stop("Required variable(s) not found in the dataset: ",
       paste(missing_vars, collapse = ", "))
}

# NFHS-5 KR variables: hw1 = child age in months; hw57 = anaemia category;
# v005 = sample weight; v021 = PSU; v022 = sampling stratum.
nfhs <- nfhs %>%
  mutate(
    anemic = case_when(
      is.na(hw57) ~ NA_real_,
      hw57 %in% c(1, 2, 3) ~ 1,
      hw57 == 4 ~ 0,
      TRUE ~ NA_real_
    ),
    age_group = cut(
      hw1,
      breaks = c(6, 24, 36, 48, 60),
      right = FALSE,
      labels = c("6-23 months", "24-35 months", "36-47 months", "48-59 months")
    ),
    mother_age_group = cut(
      v012,
      breaks = c(-Inf, 20, 25, 30, 35, Inf),
      right = FALSE,
      labels = c("<20 years", "20-24 years", "25-29 years", "30-34 years", "35+ years")
    ),
    birth_order_group = case_when(
      is.na(bord) ~ NA_character_,
      bord == 1 ~ "1st child",
      bord == 2 ~ "2nd child",
      bord >= 3 ~ "3rd+ child",
      TRUE ~ NA_character_
    ),
    # NFHS individual weights are scaled by 1,000,000.
    weight = as.numeric(v005) / 1000000
  )

# Analysis population: children aged 6-59 months with valid anaemia status.
nfhs_analysis <- nfhs %>%
  filter(hw1 >= 6, hw1 <= 59, !is.na(anemic)) %>%
  mutate(
    age_group = droplevels(age_group),
    mother_age_group = droplevels(mother_age_group),
    birth_order_group = factor(birth_order_group,
                               levels = c("1st child", "2nd child", "3rd+ child"))
  )

message("Unweighted analysis sample size: ", nrow(nfhs_analysis))

# 4. Survey design -------------------------------------------------------------
# nest = TRUE allows PSU identifiers to be reused across strata if needed.
options(survey.lonely.psu = "adjust")
nfhs_design <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~weight,
  data = nfhs_analysis,
  nest = TRUE
)

# 5. Helper: survey-weighted prevalence and 95% confidence interval ------------
weighted_prevalence <- function(design, variable, display_name) {
  x <- design$variables[[variable]]
  group_levels <- if (is.factor(x)) levels(droplevels(x)) else unique(na.omit(x))
  out <- lapply(group_levels, function(level) {
    if (is.factor(x)) {
      sub_design <- design[which(!is.na(x) & x == level), ]
      label <- as.character(level)
    } else {
      sub_design <- design[which(!is.na(x) & x == level), ]
      label <- as.character(level)
    }
    estimate <- svymean(~anemic, sub_design, na.rm = TRUE)
    ci <- confint(estimate)
    data.frame(
      Variable = paste0(display_name, ": ", label),
      Prevalence = as.numeric(coef(estimate)[1]) * 100,
      Lower_95CI = as.numeric(ci[1, 1]) * 100,
      Upper_95CI = as.numeric(ci[1, 2]) * 100,
      stringsAsFactors = FALSE
    )
  })
  bind_rows(out)
}

# Overall prevalence
overall_estimate <- svymean(~anemic, nfhs_design, na.rm = TRUE)
overall_ci <- confint(overall_estimate)
overall_row <- data.frame(
  Variable = "Overall",
  Prevalence = as.numeric(coef(overall_estimate)[1]) * 100,
  Lower_95CI = as.numeric(overall_ci[1, 1]) * 100,
  Upper_95CI = as.numeric(overall_ci[1, 2]) * 100
)

# Subgroup prevalence estimates
prevalence_table <- bind_rows(
  overall_row,
  weighted_prevalence(nfhs_design, "age_group", "Age"),
  weighted_prevalence(nfhs_design, "b4", "Sex code"),
  weighted_prevalence(nfhs_design, "mother_age_group", "Mother's age"),
  weighted_prevalence(nfhs_design, "v106", "Maternal education code"),
  weighted_prevalence(nfhs_design, "v190", "Wealth quintile code"),
  weighted_prevalence(nfhs_design, "v025", "Residence code"),
  weighted_prevalence(nfhs_design, "birth_order_group", "Birth order")
) %>%
  mutate(`Prevalence (95% CI)` = sprintf(
    "%.1f%% (%.1f-%.1f%%)", Prevalence, Lower_95CI, Upper_95CI
  ))

write.csv(prevalence_table, "results/tables/table1_weighted_anemia_prevalence.csv",
          row.names = FALSE, na = "")

# 6. Figure: weighted prevalence by child age group ----------------------------
age_plot_data <- weighted_prevalence(nfhs_design, "age_group", "Age") %>%
  mutate(Age = sub("^Age: ", "", Variable))

age_plot <- ggplot(age_plot_data,
                   aes(x = Age, y = Prevalence)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower_95CI, ymax = Upper_95CI), width = 0.15) +
  labs(
    title = "Survey-weighted anaemia prevalence by child age group",
    x = "Child age group",
    y = "Anaemia prevalence (%)"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal()

ggsave("results/figures/figure_weighted_anemia_prevalence_age.png",
       plot = age_plot, width = 8, height = 5, dpi = 300)

# 7. Multivariable survey-weighted logistic regression --------------------------
# Reference categories: 6-23 months; male; mother <20 years; no education;
# poorest wealth quintile; urban residence; first-born child.
nfhs_design_model <- update(
  nfhs_design,
  sex_factor = factor(b4, levels = c(1, 2)),
  education_factor = factor(v106, levels = c(0, 1, 2, 3)),
  wealth_factor = factor(v190, levels = c(1, 2, 3, 4, 5)),
  residence_factor = factor(v025, levels = c(1, 2))
)

weighted_model <- svyglm(
  anemic ~ age_group + sex_factor + mother_age_group + education_factor +
    wealth_factor + residence_factor + birth_order_group,
  design = nfhs_design_model,
  family = quasibinomial()
)

model_summary <- summary(weighted_model)$coefficients
model_ci <- confint(weighted_model)
model_table <- data.frame(
  term = rownames(model_summary),
  estimate = model_summary[, "Estimate"],
  p_value = model_summary[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  mutate(
    OR = exp(estimate),
    Lower_95CI = exp(model_ci[term, 1]),
    Upper_95CI = exp(model_ci[term, 2])
  )

# Friendly labels for the model terms; unmatched terms retain their term name.
term_labels <- c(
  "age_group24-35 months" = "Age: 24-35 vs 6-23 months",
  "age_group36-47 months" = "Age: 36-47 vs 6-23 months",
  "age_group48-59 months" = "Age: 48-59 vs 6-23 months",
  "sex_factor2" = "Sex: Female vs Male",
  "mother_age_group20-24 years" = "Mother's age: 20-24 vs <20 years",
  "mother_age_group25-29 years" = "Mother's age: 25-29 vs <20 years",
  "mother_age_group30-34 years" = "Mother's age: 30-34 vs <20 years",
  "mother_age_group35+ years" = "Mother's age: 35+ vs <20 years",
  "education_factor1" = "Education: Primary vs none",
  "education_factor2" = "Education: Secondary vs none",
  "education_factor3" = "Education: Higher vs none",
  "wealth_factor2" = "Wealth: Poorer vs poorest",
  "wealth_factor3" = "Wealth: Middle vs poorest",
  "wealth_factor4" = "Wealth: Richer vs poorest",
  "wealth_factor5" = "Wealth: Richest vs poorest",
  "residence_factor2" = "Residence: Rural vs urban",
  "birth_order_group2nd child" = "Birth order: 2nd vs 1st",
  "birth_order_group3rd+ child" = "Birth order: 3rd+ vs 1st"
)

final_or_table <- model_table %>%
  filter(term != "(Intercept)") %>%
  mutate(
    `Predictor comparison` = ifelse(term %in% names(term_labels),
                                    unname(term_labels[term]), term),
    `Adjusted OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", OR, Lower_95CI, Upper_95CI),
    `p-value` = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  select(`Predictor comparison`, `Adjusted OR (95% CI)`, `p-value`)

write.csv(final_or_table, "results/tables/table2_adjusted_OR_final.csv",
          row.names = FALSE, na = "")

# Also save the full numeric model output for transparent downstream use.
write.csv(model_table, "results/tables/table_multivariable_logistic_regression.csv",
          row.names = FALSE, na = "")

# 8. Run summary ---------------------------------------------------------------
message("Analysis complete.")
message("Analysis sample: ", nrow(nfhs_analysis), " children aged 6-59 months with valid anaemia status.")
message("Survey design: ", length(unique(nfhs_analysis$v021)), " unique PSU codes in the analysis data.")
message("Outputs saved under results/tables/ and results/figures/.")
print(overall_row)
print(summary(weighted_model))

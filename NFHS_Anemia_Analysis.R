# Load package
library(haven)

# Import NFHS dataset
nfhs <- read_dta("NFHS5-Maharashtra KR (2).dta")

# Extract variable labels
var_labels <- sapply(nfhs, function(x) attr(x, "label"))

# Create anaemia variable
nfhs$anemic <- ifelse(
  is.na(nfhs$hw57),
  NA,
  ifelse(nfhs$hw57 %in% c(1, 2, 3), 1, 0)
)

# Create analysis dataset
nfhs_anemia_age <- nfhs[
  nfhs$hw1 >= 6 & !is.na(nfhs$hw57),
]

# Create age groups
nfhs_anemia_age$age_group <- cut(
  nfhs_anemia_age$hw1,
  breaks = c(6, 24, 36, 48, 60),
  right = FALSE,
  labels = c(
    "6-23 months",
    "24-35 months",
    "36-47 months",
    "48-59 months"
  )
)

# Age group x anaemia
age_anemia <- table(
  nfhs_anemia_age$age_group,
  nfhs_anemia_age$anemic
)

age_anemia

# Anaemia prevalence by age group
prop.table(age_anemia, margin = 1) * 100

# Chi-square test
chisq.test(age_anemia)
prop_ci <- function(x, n) {
  result <- prop.test(x, n)
  
  c(
    prevalence = x / n * 100,
    lower = result$conf.int[1] * 100,
    upper = result$conf.int[2] * 100
  )
}
results_age <- nfhs_anemia_age %>%
  group_by(age_group) %>%
  summarise(
    total = n(),
    anaemic = sum(anemic)
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(prop_ci(anaemic, total))
  ) %>%
  tidyr::unnest_wider(ci)
ggplot(results_age,
       aes(x=age_group, y=prevalence))+
  geom_point(size=3)+
  geom_errorbar(
    aes(ymin=lower, ymax=upper),
    width=0.15
  )+
  labs(
    title="Anaemia prevalence by age group",
    x= "Age group",
    y= "Anaemia prevalence(%)"
    )+
  theme_minimal()
age_anemia<- table(
  nfhs_anemia_age$age_group,
  nfhs_anemia_age$anemic
)
age_anemia
model1 <- glm(
  anemic ~age_group,
  data= nfhs_anemia_age,
  family= binomial()
)
summary(model1)
exp(coef(model1))
exp(cbind(
  OR= coef(model1),
  confint(model1)
))

regression_table <- data.frame(
  Age_group = c(
    "6-23 months",
    "24-35 months",
    "36-47 months",
    "48-59 months"
  ),
  
  OR = c(
    1,
    exp(coef(model1))[2],
    exp(coef(model1))[3],
    exp(coef(model1))[4]
  ),
  
  Lower_95CI = c(
    NA,
    exp(confint(model1))[2, 1],
    exp(confint(model1))[3, 1],
    exp(confint(model1))[4, 1]
  ),
  
  Upper_95CI = c(
    NA,
    exp(confint(model1))[2, 2],
    exp(confint(model1))[3, 2],
    exp(confint(model1))[4, 2]
  )
)

regression_table
regression_table$p_value <- c(
  NA,
  summary(model1)$coefficients[2, 4],
  summary(model1)$coefficients[3, 4],
  summary(model1)$coefficients[4, 4]
)

regression_table
p_values <- summary(model1)$coeeficients [, 4]
p_values
regression_table$p_values <- c(
  NA,
  p_values[2],
  p_values[3],
  p_values[4]
)
regression_table
summary(model1)$coefficients
p_values <- summary(model1)$coefficients[, "Pr(>|z|)"]
p_values
regression_table$p_values <- c(
  NA,
  p_values[2],
  p_values[3],
  p_values[4]
)
regression_table
regression_table$OR_CI <- ifelse(
  is.na(regression_table$OR),
  "Reference",
  paste0(
    sprintf("%.2f",regression_table$OR),
    " (",
    sprintf("%.2f", regression_table$Lower_95CI),
    "-",
    sprintf("%.2f", regression_table$Upper_95CI),
    ")"
  )
)
regression_table

final_regression_table <- regression_table %>%
  select(
    Age_group,
    OR_CI,
    p_values
  )
final_regression_table

dir.create("results", showWarnings = FALSE)
dir.create("results/tables", showWarnings = FALSE)
dir.create("results/figures", showWarnings = FALSE)

write.csv(
  final_regression_table,
  "results/tables/table_age_logistic_regression.csv",
  row.names = FALSE
)
file.exists(
  "results/tables/table_age_logistic_regression.csv"
)

write.csv(
  results_age,
  "results/tables/table_age_prevalence.csv",
  row.names = FALSE
)
file.exists(
  "results/tables/table_age_prevalence.csv"
)

ggsave(
  "results/figures/figure_anemia_prevalence_by_age.png",
  width= 8,
  height = 6,
  dpi = 300
)
file.exists(
  "results/figures/figure_anemia_prevalence_by_age.png"
)
names(nfhs)[grepl(
  "v005|v021|v022|weight|cluster|strata",
  names(nfhs),
  ignore.case = TRUE
)]

nfhs <- read_dta("NFHS5-Maharashtra KR (2).dta")
dim(nfhs)
c("v005", "v021", "v022") %in% names(nfhs)
attr(nfhs$v005, "label")
attr(nfhs$v021, "label")
attr(nfhs$v022, "label")
summary(nfhs$v005)
weight = v005 / 100000

install.packages("survey")
library(survey)
colSums(
  is.na(
    nfhs[,c("v005", "v021", "v022")]
  )
)
nfhs$weight <- nfhs$v005 / 100000
summary(nfhs$weight)
nfhs_design <- svydesign(
  ids= ~v021,
  strata= ~v022,
  weights = ~weight,
  data = nfhs,
  nest = TRUE
)
nfhs_design
nfhs_design_anemia <- subset(
  nfhs_design,
  hw1 >=6 & !is.na (hw57)
)
nrow(nfhs_design_anemia$variables)
nfhs$age_group <- cut(
  nfhs$hw1,
  breaks = c(6, 24, 36, 48, 60),
  right = FALSE,
  labels = c(
    "6-23 months",
    "24-35 months",
    "36-47 months",
    "48-59 months"
  )
)
nfhs_design <- update(
  nfhs_design,
  age_group = nfhs$age_group
)
nfhs_design_anemia <- subset(
  nfhs_design,
  hw1 >= 6 & !is.na(hw57)
)
table(
  nfhs_design_anemia$variables$age_group,
  useNA = "ifany"
)
nfhs_design <- update(
  nfhs_design,
  anemic = ifelse(
    is.na(hw57),
    NA,
    ifelse(hw57 %in% c(1, 2, 3), 1, 0)
  )
)
nfhs_design_anemia <- subset(
  nfhs_design,
  hw1 >= 6 & !is.na(hw57)
)
names(nfhs_design_anemia$variables)
weighted_anemia <- svyby(
  ~anemic,
  ~age_group,
  nfhs_design_anemia,
  svymean,
  na.rm = TRUE
)

weighted_anemia
confint(weighted_anemia)
weighted_table <- data_frame(
  age_group= weighted_anemia$age_group,
  prevalence= weighted_anemia$anemic *100,
  lower_95CI = confint(weighted_anemia)[,1]*100,
  upper_95CI= confint(weighted_anemia)[,2]*100
)
weighted_table
write.csv(
  weighted_table,
  "results/tables/table_weighted_anemia_prevalence.csv",
  row.names = FALSE
)
file.exists(
  "results/tables/table_weighted_anemia_prevalence.csv"
)
weighted_plot<- ggplot(
  weighted_table,
  aes(x= age_group, y= prevalence)
) +
  geom_point(size=3) +
  geom_errorbar(
    aes(
      ymin= lower_95CI,
      ymax= upper_95CI
    ),
    width = 0.15
  ) +
  labs(
    title = "Survey-weighted anemia prevalence by age group",
    x= "Age group",
    y= "Anemia Prevalence(%)"
  ) + 
  theme_minimal()
weighted_plot
ggsave(
  "results/figures/figure_weighted_anemia_prevalence_age.png",
  plot= weighted_plot,
  width = 8,
  height = 6,
  dpi = 300
)
file.exists(
  "results/figures/figure_weighted_anemia_prevalence_age.png"
)
var_labels[
  grepl(
    "education|wealth|residence|birth order|birth interval|water|toilet|sanitation|breast|food|diet|illness|diarr|fever|mother|age",
    var_labels,
    ignore.case= TRUE
  )
]
table(nfhs_anemia_age$b4, useNA= "ifany")
table(nfhs_anemia_age$v025, useNA = "ifany")
attr(nfhs$v025, "labels")
table(nfhs_anemia_age$v106, useNA = "ifany")
attr(nfhs$v106, "labels")
table(nfhs_anemia_age$v190, useNA = "ifany")
attr(nfhs$v190, "labels")
summary(nfhs_anemia_age$v012)
table(nfhs_anemia_age$v012, useNA = "ifany")
nfhs_anemia_age$mother_age_group <- cut(
  nfhs_anemia_age$v012,
  breaks= c(0,20,25,30,35,Inf),
  right = FALSE,
  labels = c(
    "<20 years",
    "20-24 years",
    "25-29 years",
    "30-34 years",
    "35+ years"
  )
)
table(
  nfhs_anemia_age$mother_age_group,
  useNA = "ifany"
)
table(nfhs_anemia_age$bord, useNA = "ifany")
attr(nfhs$bord, "label")
nfhs_anemia_age$birth_order_group <- ifelse(
  nfhs_anemia_age$bord==1,
  "1st child",
  ifelse(
    nfhs_anemia_age$bord==2,
    "2nd child",
    "3rd+ child"
  )
)
table(
  nfhs_anemia_age$birth_order_group,
  useNA = "ifany"
)
summary(nfhs_anemia_age$b11)
table(nfhs_anemia_age$b11, useNA = "ifany")
nfhs$mother_age_group<- nfhs_anemia_age$mother_age_group
nfhs$birth_order_group<- nfhs_anemia_age$birth_order_group
nfhs_design<- update(
  nfhs_design,
  mother_age_group= nfhs$mother_age_group,
  birth_order_group= nfhs$birth_order_group
)
nfhs_design_anemia<- subset(
  nfhs_design,
  hw1 >=6 & !is.na(hw57)
)
nfhs$mother_age_group <- cut(
  nfhs$v012,
  breaks = c(0, 20, 25, 30, 35, Inf),
  right = FALSE,
  labels = c(
    "<20 years",
    "20-24 years",
    "25-29 years",
    "30-34 years",
    "35+ years"
  )
)
nfhs$birth_order_group <- ifelse(
  nfhs$bord == 1,
  "1st child",
  ifelse(
    nfhs$bord == 2,
    "2nd child",
    ifelse(
      nfhs$bord >= 3,
      "3rd+ child",
      NA
    )
  )
)
table(nfhs$mother_age_group, useNA = "ifany")
table(nfhs$birth_order_group, useNA = "ifany")
nfhs$weight <- nfhs$v005 / 100000
nfhs_design <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~weight,
  data = nfhs,
  nest = TRUE
)
nfhs_design_anemia <- subset(
  nfhs_design,
  hw1 >= 6 & !is.na(hw57)
)
nfhs_design_anemia
table(nfhs_design_anemia$variables$mother_age_group, useNA = "ifany")
table(nfhs_design_anemia$variables$birth_order_group, useNA = "ifany")
weighted_model2 <- svyglm(
  anemic ~ 
    age_group+
    factor(b4)+
    mother_age_group+
    factor(v106)+
    factor(v190)+
    factor(v025)+
    birth_order_group,
  design= nfhs_design_anemia,
  family = quasibinomial()
)
nfhs$anemic <- ifelse(
  is.na(nfhs$hw57),
  NA,
  ifelse(nfhs$hw57 %in% c(1,2,3),1,0)
)
nfhs$age_group <- cut(
  nfhs$hw1,
  breaks = c(6, 24, 36, 48, 60),
  right = FALSE,
  labels = c(
    "6-23 months",
    "24-35 months",
    "36-47 months",
    "48-59 months"
  )
)
nfhs$mother_age_group
nfhs$birth_order_group
names(nfhs)[
  names(nfhs) %in% c(
    "anemic",
    "age_group",
    "mother_age_group",
    "birth_order_group"
  )
]
nfhs$weight <- nfhs$v005 / 100000

nfhs_design <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~weight,
  data = nfhs,
  nest = TRUE
)

nfhs_design_anemia <- subset(
  nfhs_design,
  hw1 >= 6 & !is.na(hw57)
)
nfhs_design_anemia
nfhs$weight <- nfhs$v005 / 100000
nfhs_design <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~weight,
  data = nfhs,
  nest = TRUE
)
weighted_model2 <- syvglm(
  anemic~
    age_group+
    factor(b4, levels = c(1,2))+
    mother_age_group+
    factor(v106, levels= c(0,1,2,3))+
    factor(v190, levels = c(1,2,3,4,5))+
    factor(v025, levels = c(1,2))+
    birth_order_group,
  design= nfhs_design_anemia,
  family= quasibinomial()
)
library(survey)
weighted_model2 <- svyglm(
  anemic ~
    age_group +
    factor(b4, levels = c(1, 2)) +
    mother_age_group +
    factor(v106, levels = c(0, 1, 2, 3)) +
    factor(v190, levels = c(1, 2, 3, 4, 5)) +
    factor(v025, levels = c(1, 2)) +
    birth_order_group,
  design = nfhs_design_anemia,
  family = quasibinomial()
)
names(nfhs)[
  names(nfhs) %in% c(
    "anemic",
    "age_group",
    "mother_age_group",
    "birth_order_group"
  )
]
nfhs_design <- update(
  nfhs_design,
  anemic = nfhs$anemic,
  age_group = nfhs$age_group,
  mother_age_group = nfhs$mother_age_group,
  birth_order_group = nfhs$birth_order_group
)
nfhs$anemic <- ifelse(
  is.na(nfhs$hw57),
  NA,
  ifelse(nfhs$hw57 %in% c(1, 2, 3), 1, 0)
)
nfhs$age_group <- cut(
  nfhs$hw1,
  breaks = c(6, 24, 36, 48, 60),
  right = FALSE,
  labels = c(
    "6-23 months",
    "24-35 months",
    "36-47 months",
    "48-59 months"
  )
)
nfhs$mother_age_group <- cut(
  nfhs$v012,
  breaks = c(-Inf, 20, 25, 30, 35, Inf),
  right = FALSE,
  labels = c(
    "<20 years",
    "20-24 years",
    "25-29 years",
    "30-34 years",
    "35+ years"
  )
)
nfhs$birth_order_group <- ifelse(
  nfhs$bord == 1,
  "1st child",
  ifelse(
    nfhs$bord == 2,
    "2nd child",
    "3rd+ child"
  )
)
names(nfhs)[
  names(nfhs) %in% c(
    "anemic",
    "age_group",
    "mother_age_group",
    "birth_order_group"
  )
]
nfhs_design <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~weight,
  data = nfhs,
  nest = TRUE
)
nfhs_design_anemia <- subset(
  nfhs_design,
  hw1 >= 6 & !is.na(hw57)
)
setdiff(
  c(
    "anemic",
    "age_group",
    "b4",
    "mother_age_group",
    "v106",
    "v190",
    "v025",
    "birth_order_group"
  ),
  names(nfhs_design_anemia$variables)
)
weighted_model2 <- svyglm(
  anemic ~
    age_group +
    factor(b4, levels = c(1, 2)) +
    mother_age_group +
    factor(v106, levels = c(0, 1, 2, 3)) +
    factor(v190, levels = c(1, 2, 3, 4, 5)) +
    factor(v025, levels = c(1, 2)) +
    birth_order_group,
  design = nfhs_design_anemia,
  family = quasibinomial()
)
summary(weighted_model2)
OR_table<- data.frame(
  Variable= names(coef(weighted_model2)),
  aOR= exp(coef(weighted_model2)),
  Lower_95CI = exp(coef(weighted_model2)-
                     1.96*sqrt(diag(vcov(weighted_model2)))),
  Upper_95CI = exp(coef(weighted_model2)+
                     1.96*sqrt(diag(vcov(weighted_model2)))),
  p_value= summary(weighted_model2)$coefficients[,4]
)
OR_table
confint(weighted_model2)
final_OR_table <- data.frame(
  Variable = names(coef(weighted_model2)),
  aOR = exp(coef(weighted_model2)),
  Lower_95CI = exp(confint(weighted_model2)[, 1]),
  Upper_95CI = exp(confint(weighted_model2)[, 2]),
  p_value = summary(weighted_model2)$coefficients[, 4]
)

final_OR_table
final_OR_table$OR_CI <- sprintf(
  "%.2f (%.2f–%.2f)",
  final_OR_table$aOR,
  final_OR_table$Lower_95CI,
  final_OR_table$Upper_95CI
)

final_OR_table
write.csv(
  final_OR_table,
  "results/tables/table_multivariable_logistic_regression.csv",
  row.names= FALSE
)
file.exists(
  "results/tables/table_multivariable_logistic_regression.csv"
)
clean_OR_table <- final_OR_table[
  final_OR_table$Variable != "(Intercept)",
]

clean_OR_table$Variable <- c(
  "Age: 24-35 months vs 6-23 months",
  "Age: 36-47 months vs 6-23 months",
  "Age: 48-59 months vs 6-23 months",
  "Sex: Female vs Male",
  "Mother's age: 20-24 vs <20 years",
  "Mother's age: 25-29 vs <20 years",
  "Mother's age: 30-34 vs <20 years",
  "Mother's age: 35+ vs <20 years",
  "Maternal education: Primary vs none",
  "Maternal education: Secondary vs none",
  "Maternal education: Higher vs none",
  "Wealth: Poorer vs poorest",
  "Wealth: Middle vs poorest",
  "Wealth: Richer vs poorest",
  "Wealth: Richest vs poorest",
  "Residence: Rural vs urban",
  "Birth order: 2nd vs 1st",
  "Birth order: 3rd+ vs 1st"
)
clean_OR_table
clean_OR_table <- clean_OR_table[
  ,
  c("Variable", "OR_CI", "p_value")
]
clean_OR_table
write.csv(
  clean_OR_table,
  "results/tables/table_final_adjusted_OR.csv",
  row.names= FALSE
)
file.exists(
  "results/tables/table_final_adjusted_OR.csv"
)
overall_weighted <- svymean(
  ~anemic,
  nfhs_design_anemia,
  na.rm = TRUE
)
overall_weighted
confint(overall_weighted)
sex_weighted <- svyby(
  ~anemic,
  ~factor(b4),
  nfhs_anemia_age,
  svymean,
  na.rm= TRUE
)
sex_weighted
confint(sex_weighted)
nfhs_design_anemia <- update(
  nfhs_design_anemia,
  sex = factor(
    b4,
    levels = c(1, 2),
    labels = c("Male", "Female")
  )
)
table(
  nfhs_design_anemia$variables$sex,
  useNA = "ifany"
)
sex_weighted <- svyby(
  ~anemic,
  ~sex,
  nfhs_design_anemia,
  svymean,
  na.rm = TRUE
)

sex_weighted
confint(sex_weighted)
mother_age_weighted <- svyby(
  ~anemic,
  ~mother_age_group,
  nfhs_design_anemia,
  svymean,
  na.rm = TRUE
)

mother_age_weighted
confint(mother_age_weighted)
education_weighted <- svyby(
  ~anemic,
  ~factor(v106),
  nfhs_design_anemia,
  svymean,
  na.rm = TRUE
)

education_weighted
confint(education_weighted)
wealth_weighted <- svyby(
  ~anemic,
  ~factor(v190),
  nfhs_design_anemia,
  svymean,
  na.rm = TRUE
)

wealth_weighted
confint(wealth_weighted)
residence_weighted <- svyby(
  ~anemic,
  ~factor(v025),
  nfhs_design_anemia,
  svymean,
  na.rm = TRUE
)

residence_weighted
confint(residence_weighted)
birth_order_weighted <- svyby(
  ~anemic,
  ~birth_order_group,
  nfhs_design_anemia,
  svymean,
  na.rm = TRUE
)

birth_order_weighted
confint(birth_order_weighted)
table1 <- rbind(
  
  data.frame(
    Variable = "Age: 6-23 months",
    Prevalence = weighted_table$prevalence[1],
    Lower_95CI = weighted_table$lower_95CI[1],
    Upper_95CI = weighted_table$upper_95CI[1]
  ),
  
  data.frame(
    Variable = "Age: 24-35 months",
    Prevalence = weighted_table$prevalence[2],
    Lower_95CI = weighted_table$lower_95CI[2],
    Upper_95CI = weighted_table$upper_95CI[2]
  ),
  
  data.frame(
    Variable = "Age: 36-47 months",
    Prevalence = weighted_table$prevalence[3],
    Lower_95CI = weighted_table$lower_95CI[3],
    Upper_95CI = weighted_table$upper_95CI[3]
  ),
  
  data.frame(
    Variable = "Age: 48-59 months",
    Prevalence = weighted_table$prevalence[4],
    Lower_95CI = weighted_table$lower_95CI[4],
    Upper_95CI = weighted_table$upper_95CI[4]
  ),
  
  data.frame(
    Variable = "Sex: Male",
    Prevalence = sex_weighted$anemic[1] * 100,
    Lower_95CI = confint(sex_weighted)[1,1] * 100,
    Upper_95CI = confint(sex_weighted)[1,2] * 100
  ),
  
  data.frame(
    Variable = "Sex: Female",
    Prevalence = sex_weighted$anemic[2] * 100,
    Lower_95CI = confint(sex_weighted)[2,1] * 100,
    Upper_95CI = confint(sex_weighted)[2,2] * 100
  )
)

table1
table1 <- rbind(
  table1,
  
  data.frame(
    Variable = "Mother's age: <20 years",
    Prevalence = mother_age_weighted$anemic[1] * 100,
    Lower_95CI = confint(mother_age_weighted)[1,1] * 100,
    Upper_95CI = confint(mother_age_weighted)[1,2] * 100
  ),
  
  data.frame(
    Variable = "Mother's age: 20-24 years",
    Prevalence = mother_age_weighted$anemic[2] * 100,
    Lower_95CI = confint(mother_age_weighted)[2,1] * 100,
    Upper_95CI = confint(mother_age_weighted)[2,2] * 100
  ),
  
  data.frame(
    Variable = "Mother's age: 25-29 years",
    Prevalence = mother_age_weighted$anemic[3] * 100,
    Lower_95CI = confint(mother_age_weighted)[3,1] * 100,
    Upper_95CI = confint(mother_age_weighted)[3,2] * 100
  ),
  
  data.frame(
    Variable = "Mother's age: 30-34 years",
    Prevalence = mother_age_weighted$anemic[4] * 100,
    Lower_95CI = confint(mother_age_weighted)[4,1] * 100,
    Upper_95CI = confint(mother_age_weighted)[4,2] * 100
  ),
  
  data.frame(
    Variable = "Mother's age: 35+ years",
    Prevalence = mother_age_weighted$anemic[5] * 100,
    Lower_95CI = confint(mother_age_weighted)[5,1] * 100,
    Upper_95CI = confint(mother_age_weighted)[5,2] * 100
  )
)

table1
table1 <- rbind(
  table1,
  
  data.frame(
    Variable = "Maternal education: No education",
    Prevalence = education_weighted$anemic[1] * 100,
    Lower_95CI = confint(education_weighted)[1,1] * 100,
    Upper_95CI = confint(education_weighted)[1,2] * 100
  ),
  
  data.frame(
    Variable = "Maternal education: Primary",
    Prevalence = education_weighted$anemic[2] * 100,
    Lower_95CI = confint(education_weighted)[2,1] * 100,
    Upper_95CI = confint(education_weighted)[2,2] * 100
  ),
  
  data.frame(
    Variable = "Maternal education: Secondary",
    Prevalence = education_weighted$anemic[3] * 100,
    Lower_95CI = confint(education_weighted)[3,1] * 100,
    Upper_95CI = confint(education_weighted)[3,2] * 100
  ),
  
  data.frame(
    Variable = "Maternal education: Higher",
    Prevalence = education_weighted$anemic[4] * 100,
    Lower_95CI = confint(education_weighted)[4,1] * 100,
    Upper_95CI = confint(education_weighted)[4,2] * 100
  )
)

table1
table1 <- rbind(
  table1,
  
  data.frame(
    Variable = "Maternal education: No education",
    Prevalence = education_weighted$anemic[1] * 100,
    Lower_95CI = confint(education_weighted)[1,1] * 100,
    Upper_95CI = confint(education_weighted)[1,2] * 100
  ),
  
  data.frame(
    Variable = "Maternal education: Primary",
    Prevalence = education_weighted$anemic[2] * 100,
    Lower_95CI = confint(education_weighted)[2,1] * 100,
    Upper_95CI = confint(education_weighted)[2,2] * 100
  ),
  
  data.frame(
    Variable = "Maternal education: Secondary",
    Prevalence = education_weighted$anemic[3] * 100,
    Lower_95CI = confint(education_weighted)[3,1] * 100,
    Upper_95CI = confint(education_weighted)[3,2] * 100
  ),
  
  data.frame(
    Variable = "Maternal education: Higher",
    Prevalence = education_weighted$anemic[4] * 100,
    Lower_95CI = confint(education_weighted)[4,1] * 100,
    Upper_95CI = confint(education_weighted)[4,2] * 100
  )
)

table1
table1 <- table1[!duplicated(table1$Variable), ]

table1
table1 <- rbind(
  table1,
  
  data.frame(
    Variable = "Wealth: Poorest",
    Prevalence = wealth_weighted$anemic[1] * 100,
    Lower_95CI = confint(wealth_weighted)[1,1] * 100,
    Upper_95CI = confint(wealth_weighted)[1,2] * 100
  ),
  
  data.frame(
    Variable = "Wealth: Poorer",
    Prevalence = wealth_weighted$anemic[2] * 100,
    Lower_95CI = confint(wealth_weighted)[2,1] * 100,
    Upper_95CI = confint(wealth_weighted)[2,2] * 100
  ),
  
  data.frame(
    Variable = "Wealth: Middle",
    Prevalence = wealth_weighted$anemic[3] * 100,
    Lower_95CI = confint(wealth_weighted)[3,1] * 100,
    Upper_95CI = confint(wealth_weighted)[3,2] * 100
  ),
  
  data.frame(
    Variable = "Wealth: Richer",
    Prevalence = wealth_weighted$anemic[4] * 100,
    Lower_95CI = confint(wealth_weighted)[4,1] * 100,
    Upper_95CI = confint(wealth_weighted)[4,2] * 100
  ),
  
  data.frame(
    Variable = "Wealth: Richest",
    Prevalence = wealth_weighted$anemic[5] * 100,
    Lower_95CI = confint(wealth_weighted)[5,1] * 100,
    Upper_95CI = confint(wealth_weighted)[5,2] * 100
  )
)

table1
table1 <- rbind(
  table1,
  
  data.frame(
    Variable = "Residence: Urban",
    Prevalence = residence_weighted$anemic[1] * 100,
    Lower_95CI = confint(residence_weighted)[1,1] * 100,
    Upper_95CI = confint(residence_weighted)[1,2] * 100
  ),
  
  data.frame(
    Variable = "Residence: Rural",
    Prevalence = residence_weighted$anemic[2] * 100,
    Lower_95CI = confint(residence_weighted)[2,1] * 100,
    Upper_95CI = confint(residence_weighted)[2,2] * 100
  )
)

table1
table1 <- rbind(
  table1,
  
  data.frame(
    Variable = "Birth order: 1st child",
    Prevalence = birth_order_weighted$anemic[1] * 100,
    Lower_95CI = confint(birth_order_weighted)[1,1] * 100,
    Upper_95CI = confint(birth_order_weighted)[1,2] * 100
  ),
  
  data.frame(
    Variable = "Birth order: 2nd child",
    Prevalence = birth_order_weighted$anemic[2] * 100,
    Lower_95CI = confint(birth_order_weighted)[2,1] * 100,
    Upper_95CI = confint(birth_order_weighted)[2,2] * 100
  ),
  
  data.frame(
    Variable = "Birth order: 3rd+ child",
    Prevalence = birth_order_weighted$anemic[3] * 100,
    Lower_95CI = confint(birth_order_weighted)[3,1] * 100,
    Upper_95CI = confint(birth_order_weighted)[3,2] * 100
  )
)

table1
table1_final <- table1
table1_final[["Prevalence (95% CI)"]] <- sprintf(
  "%.1f%%(%.1f-%.1f%%)",
  table1_final$Prevalence,
  table1_final$Lower_95CI,
  table1_final$Upper_95CI
)
table1_final <- table1_final[
  ,
  c("Variable", "Prevalence (95% CI)")
]

rownames(table1_final) <- NULL

table1_final
write.csv(
  table1_final,
  "results/tables/table1_weighted_anemia_prevalence.csv",
  row.names = FALSE
)
file.exists(
  "results/tables/table1_weighted_anemia_prevalence.csv"
)
clean_OR_table
file.exists(
  "results/tables/table1_weighted_anemia_prevalence.csv"
)
write.csv(
  clean_OR_table,
  "results/tables/table2_adjusted_OR.csv",
  row.names = FALSE
)
file.exists(
  "results/tables/table2_adjusted_OR.csv"
)
clean_OR_table
summary(weighted_model2)$coefficients
coef_table<- summary(weighted_model2)$coefficients
OR_table_corrected <- data.frame(
  Variable= rownames(coef_table),
  OR =exp(coef_table[,"Estimate"]),
  Lower_95CI= exp(
    coef_table[,"Estimate"]-
      1.96*coef_table[,"Std. Error"]
  ),
  Upper_95CI=exp(
    coef_table[,"Estimate"]+
      1.96*coef_table[,"Std. Error"]
  ),
  p_value= coef_table[,"Pr(>|t|)"],
  row.names= NULL
)
OR_table_corrected
OR_table_corrected$ 'OR(95% CI)'<- sprintf(
  "%.2f(%.2f-%.2f)",
  OR_table_corrected$OR,
  OR_table_corrected$Lower_95CI,
  OR_table_corrected$Upper_95CI
)
OR_table_corrected
summary(weighted_model2)
degf(nfhs_design_anemia)
confint(weighted_model2)
levels(factor(nfhs_anemia_age$age_group))
levels(factor(nfhs_anemia_age$mother_age_group))
levels(factor(nfhs_anemia_age$birth_order_group))
levels(factor(nfhs_design_anemia$variables$age_group))

levels(factor(nfhs_design_anemia$variables$mother_age_group))

levels(factor(nfhs_design_anemia$variables$birth_order_group))
attr(nfhs_design_anemia$variables$v106, "labels")
attr(nfhs_design_anemia$variables$v190, "labels")
attr(nfhs_design_anemia$variables$v025, "labels")
final_OR_table <- OR_table_corrected

final_OR_table$Variable <- clean_OR_table$Variable

final_OR_table <- final_OR_table[
  ,
  c("Variable", "OR", "Lower_95CI", "Upper_95CI", "p_value")
]

final_OR_table$`Adjusted OR (95% CI)` <- sprintf(
  "%.2f (%.2f–%.2f)",
  final_OR_table$OR,
  final_OR_table$Lower_95CI,
  final_OR_table$Upper_95CI
)

final_OR_table <- final_OR_table[
  ,
  c("Variable", "Adjusted OR (95% CI)", "p_value")
]

final_OR_table <- final_OR_table[-1, ]

rownames(final_OR_table) <- NULL

final_OR_table
final_OR_table$Variable <- c(
  "Age: 24–35 vs 6–23 months",
  "Age: 36–47 vs 6–23 months",
  "Age: 48–59 vs 6–23 months",
  "Sex: Female vs Male",
  "Mother's age: 20–24 vs <20 years",
  "Mother's age: 25–29 vs <20 years",
  "Mother's age: 30–34 vs <20 years",
  "Mother's age: 35+ vs <20 years",
  "Education: Primary vs none",
  "Education: Secondary vs none",
  "Education: Higher vs none",
  "Wealth: Poorer vs poorest",
  "Wealth: Middle vs poorest",
  "Wealth: Richer vs poorest",
  "Wealth: Richest vs poorest",
  "Residence: Rural vs urban",
  "Birth order: 2nd vs 1st",
  "Birth order: 3rd+ vs 1st"
)

final_OR_table
final_OR_table$OR_CI <- sprintf(
  "%.2f (%.2f–%.2f)",
  final_OR_table$aOR,
  final_OR_table$Lower_95CI,
  final_OR_table$Upper_95CI
)
final_OR_table$p_value <- ifelse(
  final_OR_table$p_value < 0.001,
  "<0.001",
  sprintf("%.3f", final_OR_table$p_value)
)
final_OR_table <- final_OR_table[
  ,
  c("Variable", "OR_CI", "p_value")
]
names(final_OR_table) <- c(
  "Predictor comparison",
  "Adjusted OR (95% CI)",
  "p-value"
)
rownames(final_OR_table) <- NULL

final_OR_table
write.csv(
  final_OR_table,
  "results/tables/table2_adjusted_OR_final.csv",
  row.names = FALSE
)

file.exists(
  "results/tables/table2_adjusted_OR_final.csv"
)
summary(nfhs_design)
names(nfhs)[
  names(nfhs) %in% c(
    "V005","V021","V022",
    "hw1","hw57","b4",
    "v106","v190","v025"
  )
]
summary(nfhs$v005)
summary(nfhs$weight)
nfhs$weight<- nfhs$v005/1000000
summary(nfhs$weight)
length(unique(nfhs$v021))
length(unique(nfhs$v022))
summary(nfhs_design)
nfhs$weight <- nfhs$v005 / 1000000

nfhs_design <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~weight,
  data = nfhs,
  nest = TRUE
)

nfhs_design_anemia <- subset(
  nfhs_design,
  hw1 >= 6 & !is.na(hw57)
)

summary(nfhs_design_anemia)
nrow(nfhs_design_anemia$variables)

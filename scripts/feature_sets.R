
weighted_features <- c(
  "weighted_education", "weighted_fam_size", "weighted_income", "weighted_fam_income",
  "weighted_wage_sal", "weighted_inc_welfare", "weighted_ownership", "weighted_poverty"
)

# Binary (0/1) Features - Convert to Numeric for Modeling
binary_features <- c(
  "complication_binary", "readmitted", "hospital_expire_flag", "mortality_30_day",
  "ICU_flag","cms_flag",
  
  # Procedures-Based Flags
  "major_surgery_flag","cardiac_surgery_flag",
  "orthopedic_surgery_flag","neurosurgery_flag","sepsis_infection_flag",
  "dialysis_flag","blood_transfusion_flag","emergency_surgery_flag",
  "stroke_prevention_flag","bowel_resection_flag","colostomy_ileostomy_flag",
  "gastrostomy_tube_flag","central_line_flag","wound_debridement_flag",
  
  # Procedures-Based Flags - Late Stage
  "respiratory_support_flag","ecmo_flag","prolonged_ventilation_flag",
  "tracheostomy_flag","shock_management_flag","cpr_flag",
  
  # Medications-Based Flags
  "pressor_med_flag","broad_spectrum_antibiotic_med_flag",
  "antifungal_med_flag","inotrope_med_flag","antiarrhythmic_med_flag",
  "nephrotoxic_med_flag","diuretic_med_flag","corticosteroid_med_flag",
  "sedative_med_flag",
  
  # Demographic Flag
  "is_white"
)

# Convert Target Variables to Factors with Proper Labels
target_vars <- c("mortality_30_day", "readmitted", "hospital_expire_flag")


# Continuous Numeric Features - Add Weighted Features and Replace NA with 0
continuous_features <- c(
  "charlson_comorbidity_index", "complication_index", "length_of_stay",
  "total_icu_stays", "total_icu_hours", "num_procedures", "num_medications",
  "prior_hospital_visits_1yr", "prior_er_visits_1yr", "ed_acuity",
  
  # ICU Severity Scores
  "max_sapsii", "max_sirs", "max_oasis", "max_lods", "max_apsiii", "los_icu",
  
  # Newly Added Weighted Features
  weighted_features
)

# Categorical Variables - Replace NA with "Unknown" and Convert to Factor
categorical_vars <- c("marital_status_category", "insurance_category", 
                      "age_category", "admission_location_category", "discharge_location_category", "race_group", "gender")

icu_date_features <- c("first_icu_admit", "last_icu_discharge")

ed_features <- c("ed_chief_complaint", "ed_admit_time", "ed_discharge_time")

icu_features <- c("los_icu", "total_icu_hours", "total_icu_stays", "max_oasis", "max_sapsii", "max_lods", "max_apsiii", "max_sirs")

# Hospital Features
hosp_features <- c(
  "age_category", "age", "gender", "admission_location_numeric",
  
  ### Clinical Features
  "major_surgery_flag","cardiac_surgery_flag",
  "orthopedic_surgery_flag","neurosurgery_flag","sepsis_infection_flag",
  "dialysis_flag","blood_transfusion_flag","emergency_surgery_flag",
  "stroke_prevention_flag","bowel_resection_flag","colostomy_ileostomy_flag",
  "gastrostomy_tube_flag","central_line_flag","wound_debridement_flag",
  
  # Procedures-Based Flags - Late Stage
  "respiratory_support_flag","ecmo_flag","prolonged_ventilation_flag",
  "tracheostomy_flag","shock_management_flag","cpr_flag",
  
  # Medications-Based Flags
  "pressor_med_flag","broad_spectrum_antibiotic_med_flag",
  "antifungal_med_flag","inotrope_med_flag","antiarrhythmic_med_flag",
  "nephrotoxic_med_flag","diuretic_med_flag","corticosteroid_med_flag",
  "sedative_med_flag",
  
  ### Comorbidities, Risk, & Complication
  "charlson_comorbidity_index",
  # "complication_index", "complication_binary",
  
  ### Other Clinical Measures
  "prior_hospital_visits_1yr", "prior_er_visits_1yr",
  "num_medications", "ed_acuity"
  
  # Targets
  #, "length_of_stay" #, readmitted, hospital_expire_flag, mortality_30_day
)


# SDOH Features
sdoh_features <- c(
  "insurance_numeric", "marital_status_numeric", "race_group", "is_white",
  
  # Weighted SDOH Features
  "weighted_education", "weighted_fam_size", "weighted_income", 
  "weighted_fam_income", "weighted_wage_sal", "weighted_inc_welfare", 
  "weighted_ownership", "weighted_poverty"
)

# Base Features (Excluding SDOH)
base_features <- c(
  "age_category", "age", "gender", "admission_location_numeric",
  
  # ICU Severity Scores
  "max_sapsii", "max_sirs", "max_oasis", "max_lods", "max_apsiii", "los_icu",
  
  # Clinical Interventions & ICU Features
  "total_icu_stays", "total_icu_hours",
  
  "major_surgery_flag","cardiac_surgery_flag",
  "orthopedic_surgery_flag","neurosurgery_flag","sepsis_infection_flag",
  "dialysis_flag","blood_transfusion_flag","emergency_surgery_flag",
  "stroke_prevention_flag","bowel_resection_flag","colostomy_ileostomy_flag",
  "gastrostomy_tube_flag","central_line_flag","wound_debridement_flag",
  
  
  # Procedures-Based Flags - Late Stage
  "respiratory_support_flag","ecmo_flag","prolonged_ventilation_flag",
  "tracheostomy_flag","shock_management_flag","cpr_flag",
  
  # Medications-Based Flags
  "pressor_med_flag","broad_spectrum_antibiotic_med_flag",
  "antifungal_med_flag","inotrope_med_flag","antiarrhythmic_med_flag",
  "nephrotoxic_med_flag","diuretic_med_flag","corticosteroid_med_flag",
  "sedative_med_flag",
  
  # Comorbidities, Risk, & Complication
  "charlson_comorbidity_index", "sepsis_infection_flag",
   "complication_index", "complication_binary",
  
  # Other Clinical Measures
  "prior_hospital_visits_1yr", "prior_er_visits_1yr",
  "num_medications", "num_procedures","ed_acuity"
  
  # Targets
  #, "length_of_stay" #, readmitted, hospital_expire_flag, mortality_30_day
)

late_stage_features <- c(
  # Procedures-Based Flags - Late Stage
  "ecmo_flag","prolonged_ventilation_flag",
  "tracheostomy_flag","shock_management_flag", "cpr_flag",
  "pressor_med_flag"
)


features_sdoh <- c(base_features, sdoh_features)

features_no_sdoh <- base_features

features_hosp_sdoh <- c(hosp_features, sdoh_features)

features_hosp_icu <- c(hosp_features, icu_features)

features_hosp_icu_sdoh <- c(icu_features, hosp_features, sdoh_features)

features_non_late <- setdiff(base_features, late_stage_features)

features_non_late_sdoh <- c(features_non_late, sdoh_features)
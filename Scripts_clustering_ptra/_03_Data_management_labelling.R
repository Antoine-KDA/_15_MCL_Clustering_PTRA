#' Author: Antoine KDA
#' Date of creation: 04 September 2025
#' Last modification date:

# Objectives: Data management - create new variables --------------------------#
#' 
#' Limit to 18 years old patients
#' Create variables for tables and models
#' 
#' 
#' ----------------------------------------------------------------------------#

names(cvd_index)

#' List of important variables
#' case :==> case
#' Age at MCL diagnosis :==> age_entry_dt
#' Sex :==> female
#' Year at MCL diagnosis :==> entry_dt
#' Marital status :==> partner
#' Family histry of lymphoma :==> fam_hist_lymph, fam_hist_hl, fam_hist_nhl
#' Educ max :==>  educ_max
#' Obesity  :==> obesity_and_overweight_status
#' Stage of MCL
#' Performance Status Ecog
#' 

# 1- Recode the different variables -------------------------------------
# cross_tabulation(cvd_index, "case")
# cross_tabulation(cvd_index, "female")
# cross_tabulation(cvd_index, "partner")
# cross_tabulation(cvd_index, "fam_hist_lymph")
# cross_tabulation(cvd_index, "fam_hist_hl")
# cross_tabulation(cvd_index, "fam_hist_nhl")
# cross_tabulation(cvd_index, "educ_max")
# cross_tabulation(cvd_index, "obesity_and_overweight_status")
# cross_tabulation(cvd_index, "rheumatoid_arthritis_status")

# summary(cvd_index$age_entry_dt)
# summary(lubridate::year(cvd_index$entry_dt))

# Recode the different variables
cvd_index %<>% 
  select(-c(starts_with(c("icd", "cci_ch_", "region", "health_region")))) %>%
  filter(age_entry_dt >= 18) %>% # Limit to >=18 years old patients
  mutate(case_cat = ifelse(case == 1, "Patients", "Comparators"), 
         age_entry_dt_cat = cut(age_entry_dt, breaks = c(0, 60, 70, 80, Inf), 
                                labels = c("<=60", "61-70", "71-80", "81+"), 
                                right = TRUE), 
         Sex = ifelse(female == 1, "Female", "Male"), 
         Year_entry_dt_cat = lubridate::year(entry_dt)) %>% 
  mutate(Year_entry_dt_cat = cut(Year_entry_dt_cat, breaks = c(-Inf, 2005, 2010, 2015, Inf), 
                            labels = c("2000-2005", "2006-2010", "2011-2015", "2016-2019"), 
                            right = TRUE)) %>% 
  mutate(partner_cat = factor(case_when(
    partner == 1 ~ "AMarried",
    partner == 0 ~ "BNot married",
    is.na(partner) ~ NA
  ))) %>% 
  mutate(educ_max = factor(case_when(
    educ_max == "<= 9" ~ "A<= 9",
    educ_max == "10-12" ~ "B10-12",
    educ_max == ">= 13" ~ "C>= 13",
    educ_max == "missing" ~ NA
  ), ordered = TRUE)) %>%
  mutate(fam_hist_lymph_cat = ifelse(fam_hist_lymph == 1, "Yes", "No"), 
         fam_hist_hl_cat = ifelse(fam_hist_hl == 1, "Yes", "No"), 
         fam_hist_nhl_cat = ifelse(fam_hist_nhl == 1, "Yes", "No"))
  


# # Check the new variables 
# table(cvd_index$case, cvd_index$case_cat, exclude = NULL)
# table(cvd_index$female, cvd_index$Sex, exclude = NULL)
# table(cvd_index$partner, cvd_index$partner_cat, exclude = NULL)
# table(cvd_index$fam_hist_lymph, cvd_index$fam_hist_lymph_cat, exclude = NULL)
# table(cvd_index$fam_hist_hl, cvd_index$fam_hist_hl_cat, exclude = NULL)
# table(cvd_index$fam_hist_nhl, cvd_index$fam_hist_nhl_cat, exclude = NULL)
# table(cvd_index$educ_max, exclude = NULL)
# table(cvd_index$obesity_and_overweight_status, exclude = NULL)
# table(cvd_index$rheumatoid_arthritis_status, exclude = NULL)
# table(cvd_index$age_entry_dt_cat, exclude = NULL)
# addmargins(table(cvd_index$age_entry_dt_cat, exclude = NULL))
# addmargins(table(cvd_index$Year_entry_dt_cat, exclude = NULL))



# Remove duplicates
table(cvd_index$all_cvd_ever, exclude = NULL)
table(cvd_index$first_all_cvd_year, exclude = NULL)

cvd_index %<>% 
  mutate(CVD_Status_cat = ifelse(all_cvd_ever == 1, "Yes", "No"),
         CVD_Status = all_cvd_ever) %>% 
  mutate(Sex = as.factor(Sex), 
         Year_entry_dt_cat = as.factor(Year_entry_dt_cat), 
         Max_Enum_CVD_cat = cut(Max_Enum_CVD, breaks = c(-Inf, 0, 2, Inf), 
                                labels = c("A0", "B1-2", "C>=3"), 
                                right = TRUE)) %>%
  # Recode all first_cvd-codes_year to set "Never diagnosed" for NA
  mutate(across(starts_with("first_") & ends_with("_year"), ~ ifelse(is.na(.), "Never diagnosed", as.character(.)))) %>%

  # Add atherosclerotic CVD (coronary artery disease, stroke, peripheral and carotid artery disease)
  # This means that if a patient has any of these 3 CVDs, they will be classified as having atherosclerotic CVD  
  # coronary_artery_disease_ever = coronary artery disease
  # cerebrovascular_disease_ever = stroke
  # peripheral_vascular_carotid_ever = peripheral vascular carotid disease
  mutate(atherosclerotic_cvd_ever = ifelse(coronary_artery_disease_ever == 1 | cerebrovascular_disease_ever == 1 | peripheral_vascular_carotid_ever == 1, 1, 0),
         atherosclerotic_cvd_ever = ifelse(is.na(atherosclerotic_cvd_ever), 0, atherosclerotic_cvd_ever))
  
  

# Create age binary variable
cvd_index$age_entry_dt_bin <- cut(cvd_index$age_entry_dt, 
                                    breaks = c(-Inf, 64, Inf), 
                                    right = TRUE, 
                                    labels = c("<=64", "65+"))

# # Check
# table(cvd_index$CVD_Status, exclude = NULL)
# table(cvd_index$CVD_Status_cat, exclude = NULL)

# table(cvd_index$CVD_label, exclude = NULL)
# table(cvd_index$`Anti hypertensive drug`, exclude = NULL)
# table(cvd_index$`Reumatoid Artrit`, exclude = NULL)
# table(cvd_index$`Myocardial infarction`, exclude = NULL)
# table(cvd_index$`Cholesterol lowering drug`, exclude = NULL)
# table(cvd_index$`Cholesterol lowering drug`, cvd_index$CVD_Status, exclude = NULL)

# 2- Create labels for the newly created variables -------------------

# Create manual labels
manual_labels <- list(
  lopnr = "Patient ID",
  entry_dt = "Date of MCL diagnosis (matching date for comparators)",
  case = "Patient or Comparator (0=Comparator, 1=Patient)",
  female = "Sex (0=Male, 1=Female)",
  age_entry_dt = "Age at MCL diagnosis/matching",
  educ_max = "Highest education level",
  educ_entry_dt = "Highest education level at entry date",
  partner = "Marital status (0=Not married, 1=Married)",
  fam_hist_lymph = "Family history of lymphoma",
  fam_hist_hl = "Family history of Hodgkin lymphoma",
  fam_hist_nhl = "Family history of non-Hodgkin lymphoma",
  cci_w = "Weighted Charlson Comorbidity Index",
  cci_unw = "Unweighted Charlson Comorbidity Index",
  cci_w_5yrs = "Weighted CCI (5 years)",
  cci_unw_5yrs = "Unweighted CCI (5 years)",
  sct = "Stem cell transplant",
  sct_type = "Type of stem cell transplant",
  sct_hist = "History of stem cell transplant",
  first_all_cvd_year = "Years from first CVD diagnosis to MCL diagnosis/matching",
  all_cvd_ever = "Any CVD ever (0=No, 1=Yes)",
  first_arterial_hypertension_year = "Years from first arterial hypertension diagnosis to MCL diagnosis/matching",
  arterial_hypertension_ever = "Arterial hypertension ever (0=No, 1=Yes)",
  first_cardiac_arrest_year = "Years from first cardiac arrest diagnosis to MCL diagnosis/matching",
  cardiac_arrest_ever = "Cardiac arrest ever (0=No, 1=Yes)",
  first_cerebrovascular_disease_year = "Years from first cerebrovascular disease diagnosis to MCL diagnosis/matching",
  cerebrovascular_disease_ever = "Cerebrovascular disease ever (0=No, 1=Yes)",
  first_hyperlipidaemia_year = "Years from first hyperlipidaemia diagnosis to MCL diagnosis/matching",
  hyperlipidaemia_ever = "Hyperlipidaemia diagnosis (0=No, 1=Yes)",
  first_coronary_artery_disease_year = "Years from first coronary artery disease diagnosis to MCL diagnosis/matching",
  coronary_artery_disease_ever = "Coronary artery disease ever (0=No, 1=Yes)",
  first_atrial_fibrillation_year = "Years from first atrial fibrillation diagnosis to MCL diagnosis/matching",
  atrial_fibrillation_ever = "Atrial fibrillation ever (0=No, 1=Yes)",
  first_heart_failure_year = "Years from first heart failure diagnosis to MCL diagnosis/matching",
  heart_failure_ever = "Heart failure ever (0=No, 1=Yes)",
  first_peripheral_vascular_carotid_year = "Years from first peripheral vascular/carotid disease diagnosis to MCL diagnosis/matching",
  peripheral_vascular_carotid_ever = "Peripheral vascular/carotid disease ever (0=No, 1=Yes)",
  first_overweight_obesity_year = "Years from first overweight/obesity diagnosis to MCL diagnosis/matching",
  overweight_obesity_ever = "Overweight/obesity diagnosis (0=No, 1=Yes)",
  first_rheumatoid_arthritis_year = "Years from first rheumatoid arthritis diagnosis to MCL diagnosis/matching",
  rheumatoid_arthritis_ever = "Rheumatoid arthritis diagnosis (0=No, 1=Yes)",
  Max_Enum_CVD = "Number of distinct CVDs per patient",
  case_cat = "Patient or Comparator",
  age_entry_dt_cat = "Age at MCL diagnosis/matching", 
  Sex = "Sex", 
  Year_entry_dt_cat = "Year at MCL diagnosis/matching",
  partner_cat = "Marital status",
  fam_hist_lymph_cat = "Family history of lymphoma",
  fam_hist_hl_cat = "Family history of Hodgkin lymphoma",
  fam_hist_nhl_cat = "Family history of non-Hodgkin lymphoma",
  age_entry_dt_bin = "Age at MCL diagnosis/matching (binary)", 
  CVD_Status = "Any CVD ever (0=No, 1=Yes)",
  CVD_Status_cat = "Any CVD ever (No/Yes)",
  Max_Enum_CVD_cat = "Maximum number of distinct CVDs per patient (categorical)",
  atherosclerotic_cvd_ever = "Atherosclerotic CVD ever (0=No, 1=Yes)"

)

# Add manual labels to the data
var_label(cvd_index) <- manual_labels

# Save the dataset --------------------------------------------------------------

cvd_index_labelled <- cvd_index
save(cvd_index_labelled, manual_labels, file = paste0(output_file, "/Studydata_", toupper(cohort), ".Rdata"))

# Export to excel
openxlsx::write.xlsx(cvd_index_labelled, file = paste0(output_file, "/Studydata_", toupper(cohort), ".xlsx"), overwrite = TRUE)







#' Author: Antoine KDA
#' Date of creation: 04 October 2025
#' Last modification date:

# Objectives: Data management - Prepare data for PTRA --------------------------#
#' 
#' Format the data to follow the exact structure required for the PTRA package.
#' As per the example in the ptra_test.
#' 
#' 
#' ----------------------------------------------------------------------------#



#' Steps to follow:
#' 1- Create a patients data (in csv)
#' 2- Create a diagnosis data (in csv)
#' 3- Create a treatments data (in csv)

load(paste0(output_file, "/Studydata_", toupper(cohort), ".Rdata"))

str(cvd_index_labelled)

# Step 1: Create patients data ---------------------------------------------
patients <- cvd_index_labelled %>%
  select(lopnr, birth_dt, Sex) %>%
  distinct() %>%
  rename(patient_id = lopnr) %>%
  mutate(birth_year = lubridate::year(birth_dt),
         sex = ifelse(Sex == "Male", "M", "F"),
         col1 = paste0("\\000"), col2 = paste0("\\000"), 
         col3 = paste0("\\000"), col4 = paste0("\\000"), col5 = paste0("\\000"),
         col6 = paste0("\\000"), col7 = paste0("\\000"), col8 = paste0("\\000"),
         col9 = paste0("\\000")) %>%
  select(patient_id, sex, col1, col2, birth_year, col3, col4, col5, col6, col7, col8, col9)

dim(patients) # [1] 22561    12

# Step 2: Create diagnosis data ---------------------------------------------
diagnosis <- cvd_index_labelled %>%
  select(lopnr, dx_dt, dx_code) %>%
  distinct() %>%
  rename(patient_id = lopnr,
         date = dx_dt,
         code = dx_code) %>%
  mutate(code_type = "ICD10",
         code_version = "10",
         col1 = paste0("\\000"), col2 = paste0("\\000"), 
         col3 = paste0("\\000"), col4 = paste0("\\000"), col5 = paste0("\\000"),
         col6 = paste0("\\000"), col7 = paste0("\\000"), col8 = paste0("\\000"),
         col9 = paste0("\\000")) %>%
  select(patient_id, date, code, code_type, code_version, col1, col2, col3, col4, col5, col6, col7, col8, col9)

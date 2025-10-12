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
# 1- Load the inpatient and outpatient data

# source("Scripts/_01_Create_reduced_index_data.R", echo = TRUE)

# 1- Import the index data ------------------------------------------------------------

filpath <- "../00_Original_Data_Antoine/raw_data"

# load the index data 
index <- rio::import(file.path(filpath, "index_pop.csv")); dim(index)
# Load the inpatient data
inpatient <- rio::import(file.path(filpath, "spr_fk.csv"))
# Load the outpatient data
outpatient <- rio::import(file.path(filpath, "opr_fk.csv"))


# 1.1- Identify and remove duplicated patients in the index data (point 1)

index %>% 
  group_by(lopnr) %>% 
  filter(n() > 1) %>% 
  ungroup() -> dup_patients
# dim(dup_patients)

# 1.2- Remove duplicated: if case == 1, remove entire riskset of the corresponding case, if case == 0, remove only the lopnr

if (nrow(dup_patients) > 0) {
  index %>% 
    anti_join(dup_patients %>% filter(case == 1), by = "riskset") -> index
  index %>% 
    anti_join(dup_patients %>% filter(case == 0), by = "lopnr") -> index
}
# dim(index)
# [1] 22561    54

# 2.1- Get the necessary columns from the index data ------------------
patients <- index %>%
  select(lopnr, birth_dt, female, death_dt) %>%
  distinct() %>%
  rename(patient_id = lopnr) %>%
  mutate(birth_year = lubridate::year(birth_dt),
         birth_dt = as.Date(birth_dt),
         death_dt = as.Date(death_dt),
         year_month_death = ifelse(is.na(death_dt), "202312", format(death_dt, "%Y%m")),
         age_at_death = as.integer((death_dt - birth_dt)/365.25),
         sex = ifelse(female == 1, "F", "M"),
         col1 = paste0("\\\\000"), col2 = paste0("\\\\000"), 
         col3 = paste0("\\\\000"), col4 = paste0("\\\\000"), col5 = paste0("\\\\000"),
         col6 = paste0("\\\\000"), col7 = paste0("\\\\000"), col8 = paste0("\\\\000"),
         col9 = paste0("\\\\000")) %>%
  select(patient_id, sex, col1, col2, birth_year, age_at_death, col4, col5, col6, col7, year_month_death, col9)

# patients %>% head(100) %>% View()

# 2.2- Get the necessary columns from the index_inpat and index_outpat data ------------------

# The impatient data
red_index_inpat <- inpatient %>%
  select(lopnr, riskset, entry_dt, contains("datuma"), ar, alder,
         contains("dia")) %>% 
  select(-dia_ant) %>%
  mutate(dx_dt = as.Date(indatuma)) %>% 
  pivot_longer(cols = contains("dia"), 
                    names_to = "position", 
                    values_to = "dx") %>%
  mutate(dx = as.character(trimws(dx, which = "both")), 
         patient_id = lopnr, 
         icd_code_type = "ICD-10-WHO") %>%
#   filter(position == "hdia") %>% # Keep only the main diagnosis
  filter(!is.na(dx) & dx != "") %>% 
  filter(!grepl("-", dx)) %>% # Remove codes with hyphens (ranges)
  # Remove codes not starting with a letter (e.g., .A15)
  filter(grepl("^[A-Z]", dx, ignore.case = TRUE)) %>%

  # take only the necessary columns for PTRA - in the correct order

  mutate(col2 = paste0("\\\\000"),  col5 = paste0("\\\\000"), 
         col6 = paste0("\\\\000"),  col7 = paste0("\\\\000"), 
         col9 = paste0("\\\\000"),  col10 = paste0("\\\\000"), 
         # if dx contains a letter in the end (e.g., A41.5B, I10A) - remove this ending letter
         dx = ifelse(grepl("[A-Z]$", dx, ignore.case = TRUE),
                substr(dx, 1, nchar(dx)-1), dx),
         # if dx is more than 3 characters place a dot after the third character (e.g., I101 -> I10.1)
         dx = ifelse(nchar(dx) > 3, paste0(substr(dx, 1, 3), ".", substr(dx, 4, nchar(dx))), dx)) %>%
  select(patient_id, col2, icd_code_type, dx,
         col5, col6, col7, dx_dt, col9, col10) %>%
  arrange(patient_id, dx_dt) %>%
  distinct() # Remove duplicates


# View(red_index_inpat %>% head(100))
red_index_inpat$dx[1:100]

# head(red_index_inpat, 100) %>% View()

# The outpatient data
red_index_outpat <- outpatient %>%  
  select(lopnr, riskset, entry_dt, contains("datuma"), ar, alder,
         contains("dia")) %>% 
    select(-dia_ant) %>%
  mutate(dx_dt = as.Date(indatuma)) %>% 
  pivot_longer(cols = contains("dia"), 
                    names_to = "diag", 
                    values_to = "dx") %>%
  mutate(dx = as.character(trimws(dx, which = "both")),
         patient_id = lopnr, 
         icd_code_type = "ICD-10-WHO") %>%
#   filter(diag == "hdia") %>% # Keep only the main diagnosis
  filter(!is.na(dx) & dx != "") %>% 
  filter(!grepl("-", dx)) %>% # Remove codes with hyphens (ranges)
  # Remove codes not starting with a letter (e.g., .A15)
  filter(grepl("^[A-Z]", dx, ignore.case = TRUE))  %>%

  # take only the necessary columns for PTRA - in the correct order

  mutate(col2 = paste0("\\\\000"),  col5 = paste0("\\\\000"), 
         col6 = paste0("\\\\000"),  col7 = paste0("\\\\000"), 
         col9 = paste0("\\\\000"),  col10 = paste0("\\\\000"), 
         # if dx contains a letter in the end (e.g., A41.5B, I10A) - remove this ending letter
         dx = ifelse(grepl("[A-Z]$", dx, ignore.case = TRUE),
                substr(dx, 1, nchar(dx)-1), dx),
         # if dx is more than 3 characters place a dot after the third character (e.g., I101 -> I10.1)
         dx = ifelse(nchar(dx) > 3, paste0(substr(dx, 1, 3), ".", substr(dx, 4, nchar(dx))), dx)) %>%
  select(patient_id, col2, icd_code_type, dx,
         col5, col6, col7, dx_dt, col9, col10) %>%
  arrange(patient_id, dx_dt) %>%
  distinct() # Remove duplicates

# View(red_index_outpat %>% head(100))
# red_index_outpat$dx[1:100]


# Combine inpatient, outpatient, and drug data into one long format
ptra_diagnosis_data <- bind_rows(red_index_inpat, red_index_outpat) %>%
 # Reformat the date to be in the correct format of DD/MM/YYYY instead of YYYY-MM-DD
#   mutate(dx_dt = format(dx_dt, "%d/%m/%Y")) %>%
  arrange(patient_id, dx_dt) %>%
  distinct() # Remove duplicates


# View(head(ptra_diagnosis_data, 100))
# ptra_diagnosis_data %>% filter(!is.na(dx)) %>%
# head(10000) %>%
# View()

dim(ptra_diagnosis_data)
# [1] 182677     10



# 2.3- Get the necessary columns from the index_drug data ------------------

# ptra_drug_data <- index_drug %>%
#   select(lopnr, edatum, atc) %>%
#   rename(patient_id = lopnr,
#          date = edatum,
#          code = atc) %>%
#   mutate(code_type = "ATC",
#          code_version = "2024",
#          col1 = paste0("\\000"), col2 = paste0("\\000"), 
#          col3 = paste0("\\000"), col4 = paste0("\\000"), col5 = paste0("\\000"),
#          col6 = paste0("\\000"), col7 = paste0("\\000"), col8 = paste0("\\000"),
#          col9 = paste0("\\000")) %>%
#   select(patient_id, date, code, code_type, code_version, col1, col2, col3, col4, col5, col6, col7, col8, col9)

# str(index_drug)

# 5- Save the data for further analyses ------------------------------------------------------------
save(ptra_diagnosis_data, patients, file = paste0(output_file, "/ptra_diagnosis_patients_data_", toupper(cohort), ".Rdata"))

# Export to csv without row names and headers 
rio::export(ptra_diagnosis_data, file = paste0(output_file, "/ptra_diagnosis_data_", toupper(cohort), ".csv"), 
          row.names = FALSE, quote = FALSE, col.names = FALSE)
rio::export(patients, file = paste0(output_file, "/ptra_patients_data_", toupper(cohort), ".csv"), 
          row.names = FALSE, quote = FALSE, col.names = FALSE)


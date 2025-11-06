
#' Author: Antoine KDA
#' Date of creation: 06 November 2025
#' Last modification date:

# Objectives: Data management - Prepare data for PTRA --------------------------#
#' 
#' Format the data to follow the exact structure required for the PTRA-ICD10-WHO package.
#' As per the example in the ptra_test.
#' 
#' 
#' ----------------------------------------------------------------------------#



#' Steps to follow:
#' 1- Create a patients data (in csv)
#' 2- Create a diagnosis data (in csv)
#' 3- Create a treatments data (in csv)
#' 4- Create a merged diagnosis + treatments data (in csv)
#' 5- Save and export the data for PTRA

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
#' The header of the patients data should be as follows:
#' patient_id, sex, year_of_birth, patient_regional_location, month_year_death, control

# Create the whole cohort patient data
ptra_all_patients <- index %>%
  select(lopnr, female, birth_dt, health_region, death_dt, case) %>%
  distinct() %>%
  rename(patient_id = lopnr, sex = female, patient_regional_location = health_region, control = case) %>%
  mutate(year_of_birth = lubridate::year(birth_dt),
         birth_dt = as.Date(birth_dt),
         death_dt = as.Date(death_dt),
         month_year_death = paste0("\\\\000"), #ifelse(is.na(death_dt), "202312", format(death_dt, "%Y%m")),
        #  age_at_death = paste0("\\\\000"), #as.integer((death_dt - birth_dt)/365.25),
         sex = ifelse(sex == 1, "F", "M"),
         control = ifelse(control == 0, "yes", "no")) %>%
  select(patient_id, sex, year_of_birth, patient_regional_location, month_year_death, control) 

nrow(ptra_all_patients)
# [1] 22561
# remove all rows that contain NA in any of the columns
ptra_all_patients <- ptra_all_patients[complete.cases(ptra_all_patients), ]
nrow(ptra_all_patients)
# [1] 22561
table(ptra_all_patients$control)
#    no   yes
#  2056 20505

# Create a patient-only data
ptra_lymph_patients <- ptra_all_patients %>%
  filter(control == "no")
nrow(ptra_lymph_patients)

# ptra_all_patients %>% head(100) %>% View()

# 2.2- Get the necessary columns from the index_inpat and index_outpat data ------------------

#' The header of the diagnosis data should be as follows:
#' patient_id,code_system,code,date
#' 
#' Also I need to make sure that all lymphoma patients have at least one diagnosis record of C83.1 
#' in the diagnosis data. I will artificially add it with the date of entry data as diag date. 

# The impatient data
red_index_inpat <- inpatient %>%
  select(lopnr, 
         contains("datuma"), ar, alder,
         contains("dia")) %>% 
  select(-dia_ant) %>%
  mutate(date = as.Date(indatuma)) %>% 
  pivot_longer(cols = contains("dia"), 
                    names_to = "position", 
                    values_to = "code") %>%
# add the lymphoma patients with C83.1 diagnosis 
bind_rows(index %>% 
    filter(case == 1) %>%
    mutate(date = as.Date(entry_dt),
           position = "hdia",
           code = "C83.1") %>%    
    select(lopnr, date, position, code)) %>%

# data management/cleaning on the full data

  mutate(code = as.character(trimws(code, which = "both")), 
         patient_id = lopnr, 
         code_system = "ICD-10-WHO") %>%
#   filter(position == "hdia") %>% # Keep only the main diagnosis
  filter(!is.na(code) & code != "") %>% 
  mutate(code = ifelse(grepl("-|\\.", code), substr(code, 1, regexpr("-|\\.", code)-1), code),
         code = ifelse(grepl(" ", code), substr(code, 1, regexpr(" ", code)-1), code)) %>%
  # Remove codes not starting with a letter (e.g., .A15)
  filter(grepl("^[A-Z]", code, ignore.case = TRUE)) %>%

  # take only the necessary columns for PTRA - in the correct order

  mutate(# Truncate code at the first occurrence of a letter after the starting letter
        code = ifelse(nchar(code) > 1 & grepl("^[A-Z].*[A-Z]", code, ignore.case = TRUE),
                     substr(code, 1, regexpr("[A-Z]", substr(code, 2, nchar(code)), ignore.case = TRUE)),
                     code),
         # if code is more than 3 characters place a dot after the third character (e.g., I101 -> I10.1)
         code = ifelse(nchar(code) > 3, paste0(substr(code, 1, 3), ".", substr(code, 4, nchar(code))), code), 
         code = substr(code, 1, 6)) %>%
         # Truncate everything to be between 2 and 6 characters
  filter(nchar(code) >=2) %>%
  filter(!is.na(code)) %>%
  filter(!is.na(date)) %>%
  select(patient_id, code_system, code, date) %>%
  arrange(patient_id, date) %>%
  distinct() # Remove duplicates

# which(is.na(red_index_inpat$date))
# # integer(0)
# which(is.na(inpatient$indatuma))
nrow(red_index_inpat)
# [1] 396499

# View missing date patients
# red_index_inpat %>% filter(is.na(date)) %>% head(1000) %>% View()
# red_index_inpat %>% filter(is.na(date)) %>% nrow()

# View(red_index_inpat %>% head(1000))
table(nchar(red_index_inpat$code))
red_index_inpat$code[which(nchar(red_index_inpat$code)>6)][1:400]
length(red_index_inpat$code[which(nchar(red_index_inpat$code)>6)])
# head(red_index_inpat, 100) %>% View()

# The outpatient data
red_index_outpat <- outpatient %>%  
  select(lopnr, # riskset, entry_dt, 
         contains("datuma"), ar, alder,
         contains("dia")) %>% 
    select(-dia_ant) %>%
  mutate(date = as.Date(indatuma)) %>% 
  pivot_longer(cols = contains("dia"), 
                    names_to = "diag", 
                    values_to = "code") %>%
  mutate(code = as.character(trimws(code, which = "both")),
         patient_id = lopnr, 
         code_system = "ICD-10-WHO") %>%
#   filter(position == "hdia") %>% # Keep only the main diagnosis
  filter(!is.na(code) & code != "") %>% 
  mutate(code = ifelse(grepl("-|\\.", code), substr(code, 1, regexpr("-|\\.", code)-1), code),
         code = ifelse(grepl(" ", code), substr(code, 1, regexpr(" ", code)-1), code)) %>%
  # Remove codes not starting with a letter (e.g., .A15)
  filter(grepl("^[A-Z]", code, ignore.case = TRUE)) %>%

  # take only the necessary columns for PTRA - in the correct order

  mutate(# Truncate code at the first occurrence of a letter after the starting letter
        code = ifelse(nchar(code) > 1 & grepl("^[A-Z].*[A-Z]", code, ignore.case = TRUE),
                     substr(code, 1, regexpr("[A-Z]", substr(code, 2, nchar(code)), ignore.case = TRUE)),
                     code),
         # if code is more than 3 characters place a dot after the third character (e.g., I101 -> I10.1)
         code = ifelse(nchar(code) > 3, paste0(substr(code, 1, 3), ".", substr(code, 4, nchar(code))), code), 
         code = substr(code, 1, 6)) %>%
         # Truncate everything to be between 2 and 6 characters
  filter(nchar(code) >=2) %>%
  filter(!is.na(code)) %>%
  filter(!is.na(date)) %>%
  select(patient_id, code_system, code, date) %>%
  arrange(patient_id, date) %>%
  distinct() # Remove duplicates


# View(red_index_outpat %>% head(100))
# red_index_outpat$code[1:100]


# Combine inpatient, outpatient, and drug data into one long format
ptra_all_diagnosis <- bind_rows(red_index_inpat, red_index_outpat) %>%
  arrange(patient_id, date) %>%
  distinct() # Remove duplicates
nrow(ptra_all_diagnosis)
# [1] 1198146

# For patient_id with multiple C83 diagnoses, keep only the first occurrence, if the 
# id different from C83, then keep all occurrences

ptra_all_diagnosis <- ptra_all_diagnosis %>%
  group_by(patient_id, code) %>%
  arrange(date) %>%
  filter(ifelse(code == "C83" | grepl("^C83\\.", code), row_number() == 1, TRUE)) %>%
  ungroup()

nrow(ptra_all_diagnosis)
# [1] 1150724

# check if there is missingness in any of the columns
sum(is.na(ptra_all_diagnosis)) # 0

#' Note: Sweden is using the ICD-10-SE which is a local modification of the ICD-10 WHO version
#' Therefore, some codes may not exactly match the WHO version.
#' I will remove that do not have a match in the WHO version later on in the analysis.

# Import the ICD-10-WHO codes to check against
# Read ICD data from  text file 
icd_data <- read.table("C:\\Users\\kwhr625\\Box\\Personal\\Old_Studies\\CVD_in_DLBCL\\Clustering_CVD\\Data\\Codelists_ICD_10_9_ATC\\icd102019enMeta\\icd102019syst_codes.txt", 
                       sep = ";", header = FALSE, stringsAsFactors = FALSE, fill = TRUE, quote = "", comment.char = "") 

# View(icd_data)
names(icd_data)

(set_diff <- setdiff(ptra_all_diagnosis$code, icd_data$V7))
export1 <- data.frame(missing_icd_codes_full = set_diff)

# If code is in the set_diff, truncate the code to 3 characters and check again
ptra_all_diagnosis$code <- ifelse(ptra_all_diagnosis$code %in% set_diff, 
                                        substr(ptra_all_diagnosis$code, 1, 3), 
                                        ptra_all_diagnosis$code)

# Check again for missing codes
(set_diff2 <- setdiff(ptra_all_diagnosis$code, icd_data$V7))
#  [1] "U00" "I84" "B59" "A11" "V07" "V08" "J07"
export2 <- data.frame(missing_icd_codes_lvl3 = set_diff2)

# Check how many records have these codes
length(which(ptra_all_diagnosis$code %in% set_diff2))
# [1] 3851

(set_diff3 <- setdiff(gsub("\\.", "", ptra_all_diagnosis$code), icd_data$V8))

# If code is in the set_diff2, exclude these records and create the final dataset
ptra_all_diagnosis <- ptra_all_diagnosis %>%
  filter(!code %in% set_diff2)
nrow(ptra_all_diagnosis)
# [1] 1146873


# Create a patient-only data 
ptra_lymph_diagnosis <- ptra_all_diagnosis %>%
  right_join(ptra_lymph_patients %>% select(patient_id) , by = "patient_id") %>% 
  arrange(patient_id, date) %>%
  distinct() # Remove duplicates
nrow(ptra_lymph_diagnosis)
# [1] 141635

# View(head(ptra_diagnosis_data, 100))
# ptra_diagnosis_data %>% filter(!is.na(code)) %>%
# head(10000) %>%
# View()

dim(ptra_all_diagnosis)
# [1] 1146873       4
dim(ptra_lymph_diagnosis)
# [1] 141635      4

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
# save(ptra_diagnosis_data, patients, file = paste0(output_file, "/ptra_diagnosis_patients_data_", toupper(cohort), ".Rdata"))

# Export to csv without row names and headers 
rio::export(ptra_all_diagnosis, file = paste0(output_file, "/ptra_all_diagnosis_", toupper(cohort), ".csv"), 
          row.names = FALSE, quote = FALSE, col.names = TRUE)
rio::export(ptra_all_patients, file = paste0(output_file, "/ptra_all_patients_", toupper(cohort), ".csv"), 
          row.names = FALSE, quote = FALSE, col.names = TRUE)

rio::export(ptra_lymph_diagnosis, file = paste0(output_file, "/ptra_lymph_diagnosis_", toupper(cohort), ".csv"), 
          row.names = FALSE, quote = FALSE, col.names = TRUE)
rio::export(ptra_lymph_patients, file = paste0(output_file, "/ptra_lymph_patients_", toupper(cohort), ".csv"), 
          row.names = FALSE, quote = FALSE, col.names = TRUE)
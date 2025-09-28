#' Author: Antoine KDA
#' Date of creation: 28 July 2025
#' Last modification date: 
#' Last modification date: 
 


# Objectives: Disease and prescription identification ---------------------#
#' 
#' Main task: Search for diseases and prescriptions in the inpatient, outpatient, and prescribed drug data
#' 
#' Specific sub-tasks: 
#' 1- Import the icd10 and ATC codes list and create all necessary regroupings
#' 2- Search for the disease and prescription codes in the inpatient, outpatient, and prescribed drug data
#' 3- Create a summary table of the identified diseases and prescriptions in long format with years 1-10
#'    
#'    
#' Changes: 
#' No changes yet
#' 
#' -----------------------------------------------------------------------------#
#' 


# 1- Import the icd10 and ATC codes list and create all necessary regroupings ------------------

# Load ICD-10 codes
icd10 <- read.csv("./Data/Codelists_ICD_10_9_ATC/icd102019enMeta/icd102019syst_groups.txt", sep = ";", header = FALSE, 
                  col.names = c("code_start", "code_end", "disease_group_code", "icd10_group_description")) %>%
                  select(code_start, code_end, disease_group_code, icd10_group_description) %>%
                  mutate(disease_group_code = paste0("Group", as.character(disease_group_code))) %>%
                  left_join(
         read.csv("./Data/Codelists_ICD_10_9_ATC/icd102019enMeta/icd102019syst_chapters.txt", sep = ";", header = FALSE, 
                  col.names = c("disease_group_code", "disease_group_name")) %>%
                  select(disease_group_code, disease_group_name) %>%
                  mutate(disease_group_code = paste0("Group", as.character(disease_group_code)))
                  ) %>%
         filter(grepl("^I|^E|^M|^G", code_start)) # Keep only codes starting with I, E, M, or G (Cardiovascular, Endocrine, Musculoskeletal, and Nervous system diseases)

# View(icd10)

# Load ATC codes
atc_who_3ch <- read.csv("./Data/atcd-master/WHO ATC-DDD 2024-07-31.csv", sep = ",", header = TRUE) %>%
  select(atc_code, atc_name) %>%
  filter(nchar(atc_code) == 3) %>% # Keep only 3-character ATC codes
  mutate(atc_name = tolower(atc_name)) %>%
  mutate(atc_name = gsub("^(.)", "\\U\\1", atc_name, perl = TRUE)) %>% # Capitalize first letter of ATC name
  filter(grepl("^C02|^C03|^C07|^C08|^C09", atc_code)) # Keep only cardiovascular drugs

# View(atc_who_3ch)

# 1.1- Function to expand ICD-10 code ranges ---------------
# This function expands a range of ICD-10 codes from a start code to an end code
# It handles cases where the codes have the same letter prefix or different prefixes.

expand_icd10_range <- function(code_start, code_end) {
  # Extract letter prefix and numeric parts
  start_letter <- substr(code_start, 1, 1)
  end_letter <- substr(code_end, 1, 1)
  start_num <- as.numeric(substr(code_start, 2, nchar(code_start)))
  end_num <- as.numeric(substr(code_end, 2, nchar(code_end)))
  
  # Generate sequence of codes
  if (start_letter == end_letter) {
    # Same letter prefix
    nums <- start_num:end_num
    codes <- paste0(start_letter, sprintf("%02d", nums))
    codes <- paste0(codes, collapse = "|")
    codes <- paste0("^(", codes, ")") 
  } else {
    # Different letter prefixes (rare case)
    codes <- c(paste0(start_letter, sprintf("%02d", start_num:99)),
               paste0(end_letter, sprintf("%02d", 0:end_num)))
    codes <- paste0(codes, collapse = "|")
    codes <- paste0("^(", codes, ")") 
  }
  
  return(codes)
}

# Example
expand_icd10_range("A50", "A59")

# 1.1- Apply the function to expand ICD-10 code ranges ---------------
# Expand all code ranges change this to create a new column codes in the icd10 data frame
icd10$codes <- NULL
icd10$code_group <- NULL
for (i in 1:nrow(icd10)) {
    icd10[i, "codes"] <- expand_icd10_range(icd10$code_start[i], icd10$code_end[i])
    icd10[i, "code_group"] <- paste0(icd10$code_start[i], "-", icd10$code_end[i])
}

View(head(icd10))


# 2- Search for the disease codes in the inpatient, and outpatient data ------------------
#' Steps
#' 1- Get the necessary columns from the index_inpat and index_outpat data
#' 2- Pivot longer the index_inpat and index_outpat data to have a long format with diag1-30
#' 3- Search for the disease codes in the long format data
#' 4- Create a variable year 1-10 for each disease code found relative to the entry date


# 2.1- Get the necessary columns from the index_inpat and index_outpat data ------------------

# The impatient data
red_index_inpat <- index_inpat %>%
  select(lopnr, riskset, entry_dt, contains("datuma"), ar, alder,
         contains("dia")) %>%
  select(-dia_ant) %>%
  mutate(
    indatuma = as.Date(indatuma),
    utdatuma = as.Date(as.character(utdatuma), format = "%Y%m%d"),
    los = utdatuma - indatuma) %>% # Calculate length of stay
  pivot_longer(cols = contains("dia"), 
                    names_to = "diag", 
                    values_to = "code") %>%
  mutate(code = as.character(gsub(" ", "", code))) %>%
  filter(!is.na(code)) %>%
  mutate(year = year(entry_dt) - year(utdatuma))



# The outpatient data
red_index_outpat <- index_outpat %>%  
  select(lopnr, riskset, entry_dt, contains("datuma"), ar, alder,
         contains("dia")) %>%
    select(-dia_ant) %>%
  mutate(indatuma = as.Date(indatuma)) %>% 
  pivot_longer(cols = contains("dia"), 
                    names_to = "diag", 
                    values_to = "code") %>%
  mutate(code = as.character(gsub(" ", "", code))) %>%
  filter(!is.na(code)) %>%
  mutate(year = year(entry_dt) - year(indatuma))

# 2.2- Search for the disease codes in the long format data ------------------

# Search for the disease codes in the inpatient data
# Initialize columns as NA
red_index_inpat$code_group <- NA
red_index_inpat$code_group_description <- NA
for (i in 1:nrow(icd10)) {
  print(paste0(i, "/", nrow(icd10), " Searching for ", icd10$icd10_group_description[i], " codes...: (", icd10$code_group[i], ")"))
  match_idx <- grepl(icd10$codes[i], red_index_inpat$code, ignore.case = TRUE)
  red_index_inpat$code_group[match_idx] <- icd10$code_group[i]
  red_index_inpat$code_group_description[match_idx] <- icd10$icd10_group_description[i]
}

# Check the results
View(head(red_index_inpat, 100))

# Search for the disease codes in the outpatient data
# Initialize columns as NA
red_index_outpat$code_group <- NA
red_index_outpat$code_group_description <- NA
for (i in 1:nrow(icd10)) {
  print(paste0(i, "/", nrow(icd10), " Searching for ", icd10$icd10_group_description[i], " codes...: (", icd10$code_group[i], ")"))
  match_idx <- grepl(icd10$codes[i], red_index_outpat$code, ignore.case = TRUE)
  red_index_outpat$code_group[match_idx] <- icd10$code_group[i]
  red_index_outpat$code_group_description[match_idx] <- icd10$icd10_group_description[i]
}

# Check the results
View(head(red_index_outpat, 100))

# 3- Search for the prescription codes in the prescribed drug data ------------------

# Atc codes
red_index_drug_atc <- index_drug %>%
  select(lopnr, riskset, entry_dt, edatum, ar, alder, atc) %>%
  mutate(edatum = as.Date(edatum), 
         atc_group_code = substr(atc, 1, 1), 
         atc = substr(atc, 1, 3)) %>% 
  filter(!is.na(atc)) %>%
  mutate(year = year(entry_dt) - year(edatum)) %>%
  inner_join(atc_who_3ch, by = c("atc" = "atc_code"))

# Check the results
View(head(red_index_drug_atc, 100))

# 4- Combine all three data frames into one long format ------------------

if (cohort == "severeonly") {
  # Not include drugs
  # Combine inpatient and outpatient data into one long format
combined_index_data <- bind_rows(
  red_index_inpat %>% mutate(exam_dt = indatuma, 
                             type = "inpatient") %>%  
                      select(lopnr, riskset, entry_dt, exam_dt, year, type, code, 
                      code_group, code_group_description) ,

  red_index_outpat %>% mutate(exam_dt = indatuma, type = "outpatient") %>%  # outpatients have a length of stay of 1 day
                      select(lopnr, riskset, entry_dt, exam_dt, year, type, code, 
                             code_group, code_group_description) 
) %>%
arrange(lopnr, entry_dt, exam_dt, year) 
# %>%
# filter(!is.na(disease_group_code))
} else {
  # Include drugs
  # Combine inpatient, outpatient, and drug data into one long format
combined_index_data <- bind_rows(
  red_index_inpat %>% mutate(exam_dt = indatuma, 
                             type = "inpatient") %>%  
                      select(lopnr, riskset, entry_dt, exam_dt, year, type, code, 
                      code_group, code_group_description) ,

  red_index_outpat %>% mutate(exam_dt = indatuma, type = "outpatient") %>%  # outpatients have a length of stay of 1 day
                      select(lopnr, riskset, entry_dt, exam_dt, year, type, code, 
                             code_group, code_group_description) ,

  red_index_drug_atc %>% mutate(exam_dt = edatum, code_group = atc, code_group_description = atc_name,
                            type = "drug", code = atc) %>%  # Drug collection days have a length of stay of 1 day
                      select(lopnr, riskset, entry_dt, exam_dt, year, type, code, 
                             code_group, code_group_description)
) %>%
arrange(lopnr, entry_dt, exam_dt, year) 
# %>%
# filter(!is.na(disease_group_code))
}

# View(head(combined_index_data, 100))
combined_index_data %>% filter(is.na(code_group)) %>%
head(10000) %>%
View()

# 5- Use the code lists to identify the different types of CVD ------------------

# Identify types of CVD
# Supplementary Table 1 & 2: Diagnosis and ATC code lists
cvd_codes <- list(
  all_cvd = list(
    icd10 = "I00-I99",
    icd9 = "390-459", 
    atc = c("C02", "C03", "C07", "C08", "C09", "C10")
  ),
  arterial_hypertension = list(
    icd10 = "I10-I15",
    icd9 = "401-405",
    atc = c("C02", "C03", "C07", "C08", "C09")
  ),
  cardiac_arrest = list(
    icd10 = "I46",
    icd9 = "427.5"
  ),
  cerebrovascular_disease = list(
    icd10 = c("I61", "I63-I64"),
    icd9 = "430-438"
  ),
  hyperlipidaemia = list(
    icd10 = "E78.5",
    icd9 = "272.4",
    atc = "C10"
  ),
  coronary_artery_disease = list(
    icd10 = "I20-I25",
    icd9 = "410-414"
  ),
  atrial_fibrillation = list(
    icd10 = "I48",
    icd9 = c("427.31", "427.32")
  ),
  heart_failure = list(
    icd10 = "I50",
    icd9 = "428"
  ),
  peripheral_vascular_carotid = list(
    icd10 = c("I70", "I73", "I65"),
    icd9 = c("443.9", "440.2", "433.1")
  ),
  overweight_obesity = list(
    icd10 = "E66",
    icd9 = c("278.0", "278.00", "278.01", "278.02", "278.03")
  ),
  rheumatoid_arthritis = list(
    icd10 = "M05",
    icd9 = "714"
  )
)

# Identify the different types of CVD in the combined_index_data data - create a new variable for each type of CVD
for (cvd in names(cvd_codes)) {
  print(paste0("Identifying .... ", cvd))

  # Get in a vector the icd10, icd9, and atc patterns
  codes <- as.vector(unlist(cvd_codes[[cvd]]))
  # remove dots from codes 
  codes <- gsub("\\.", "", codes)
  # Expand any code in codes that contains '-' 
  for (i in 1:length(codes)) {
    if (grepl("-", codes[i])) {
      range_parts <- unlist(strsplit(codes[i], "-"))
      if (length(range_parts) == 2) {
        expanded_codes <- expand_icd10_range(range_parts[1], range_parts[2])
        codes[i] <- gsub("\\^|\\(|\\)", "", expanded_codes)
      }
    }
  }
 
  # Create the pattern for icd10, icd9, and atc
  codes_pattern <- paste0("^(", paste(codes, collapse = "|"), ")")

  # Create a new variable for each type of CVD
  combined_index_data[[cvd]] <- ifelse(grepl(codes_pattern, combined_index_data$code, ignore.case = TRUE), 1, 0)

}

View(head(combined_index_data, 100))
intersect(names(cvd_codes), names(combined_index_data))

apply(combined_index_data %>% select(all_of(names(cvd_codes))), 2, table, exclude = NULL)
# grep("e66", combined_index_data$code, ignore.case = TRUE, value = TRUE)

# 6- Create a dataset with patients having at least one CVD code merged with the index data ------------------

# Find the first occurrence of each CVD type per person and merge with the index data

cvd_index  <- index

for (cvd in names(cvd_codes)) {
  print(paste0("Checking .... ", cvd))
 # Find the first occurrence of each CVD type per person
  first_cvd <- combined_index_data %>%
    filter(!!sym(cvd) == 1) %>%
    arrange(lopnr, exam_dt) %>%
    group_by(lopnr) %>%
    slice_min(order_by = exam_dt, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(lopnr, first_cvd_exam_dt = exam_dt, first_cvd_year = year)  
  
  # Rename columns to include the CVD type
  colnames(first_cvd)[colnames(first_cvd) == "first_cvd_exam_dt"] <- paste0("first_", cvd, "_exam_dt")
  colnames(first_cvd)[colnames(first_cvd) == "first_cvd_year"] <- paste0("first_", cvd, "_year")
  first_cvd[, paste0(cvd, "_ever")] <- 1
  
  # Merge with the combined_index_data
  cvd_index <- cvd_index %>%
    left_join(first_cvd, by = "lopnr") %>%
    mutate(!!sym(paste0(cvd, "_ever")) := ifelse(is.na(!!sym(paste0(cvd, "_ever"))), 0, !!sym(paste0(cvd, "_ever"))))

  cat("\n Dimensions of cvd_index after merging \n",  dim(cvd_index))
  cat("\n Distribution of ", cvd, "_ever \n")
  print(table(cvd_index[[paste0(cvd, "_ever")]], exclude = NULL))

}

# # Check for duplicated lopnr
# table(duplicated(cvd_index$lopnr))

# Get sum of all CVD ever excluding overweight_obesity, rheumatoid_arthritis, and all_cvd_ever for each patient
sum_cvd_vars <- paste0(names(cvd_codes)[!names(cvd_codes) %in% c("overweight_obesity", "rheumatoid_arthritis", "all_cvd", "hyperlipidaemia")], "_ever")
cvd_index$Max_Enum_CVD <- apply(cvd_index %>% select(all_of(sum_cvd_vars)), 1, sum, na.rm = TRUE)
# Add peeople with cvd codes only in all_cvd
cvd_index$Max_Enum_CVD <- ifelse(cvd_index$Max_Enum_CVD == 0 & cvd_index$all_cvd_ever == 1, 1, cvd_index$Max_Enum_CVD)
# table(cvd_index$Max_Enum_CVD, exclude = NULL)
# table(cvd_index$case, cvd_index$Max_Enum_CVD, exclude = NULL)

# 5- Save the data for further analyses ------------------------------------------------------------
save(cvd_index, index, combined_index_data, cvd_codes, file = paste0(output_file, "/index_data_codelist_", toupper(cohort), ".Rdata"))
# "./Output files/index_data_Script3.Rdata"

# Export to excel: only the cvd_codes which is a list of codes

# Create a data frame from the cvd_codes list
cvd_codes_df <- data.frame(
  Diagnosis = character(),
  Code_Type = character(),
  Codes = character(),
  stringsAsFactors = FALSE
)

# Loop through each diagnosis and code type
for (diagnosis in names(cvd_codes)) {
  for (code_type in names(cvd_codes[[diagnosis]])) {
    codes <- cvd_codes[[diagnosis]][[code_type]]
    # Collapse multiple codes into a single string separated by semicolons
    codes_string <- paste(codes, collapse = "; ")
    
    # Add row to data frame
    cvd_codes_df <- rbind(cvd_codes_df, 
                          data.frame(Diagnosis = diagnosis,
                                   Code_Type = toupper(code_type),
                                   Codes = codes_string,
                                   stringsAsFactors = FALSE))
  }
}

# View the data frame
View(cvd_codes_df)

# Pivot the data frame to have separate columns for ICD-10, ICD-9, and ATC codes
cvd_codes_df <- cvd_codes_df %>%
  pivot_wider(names_from = Code_Type, values_from = Codes, values_fill = NA)


# Alternative: Add to your existing workbook
if (exists("wb")) {
  addWorksheet(wb, "CVD_Codes_List")
  writeDataTable(wb, "CVD_Codes_List", cvd_codes_df, startCol = 1, startRow = 1)
}


openxlsx::saveWorkbook(wb, file = paste0(result_file, "/Results_", toupper(cohort), ".xlsx"), overwrite = TRUE)



#' Author: Antoine KDA
#' Date of creation: 28 Sept 2025
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
  mutate(code = as.character(trimws(code, which = "both")), 
         year = year(entry_dt) - year(utdatuma)) %>%
  filter(!is.na(code) & code != "") %>% 
  filter(!grepl("-", code)) %>% # Remove codes with hyphens (ranges)
  # Remove codes not starting with a letter (e.g., .A15)
  filter(grepl("^[A-Z]", code, ignore.case = TRUE)) 

# View(red_index_inpat %>% head(100))
red_index_inpat$code[1:100]

# Find proportion of 7 characters codes
# prop.table(table(nchar(red_index_inpat$code)))*100

# View 7 and 8 characters codes
# red_index_inpat %>% filter(nchar(code) >= 7) %>% View()

# Distribution of 7 and 8 characters codes
# red_index_inpat %>% filter(nchar(code) >= 3) %>% select(code) %>% table() %>% prop.table()*100 %>% sort()

# The outpatient data
red_index_outpat <- index_outpat %>%  
  select(lopnr, riskset, entry_dt, contains("datuma"), ar, alder,
         contains("dia")) %>%
    select(-dia_ant) %>%
  mutate(indatuma = as.Date(indatuma)) %>% 
  pivot_longer(cols = contains("dia"), 
                    names_to = "diag", 
                    values_to = "code") %>%
  mutate(code = as.character(trimws(code, which = "both")), 
         year = year(entry_dt) - year(indatuma)) %>%
  filter(!is.na(code) & code != "") %>% 
  filter(!grepl("-", code)) %>% # Remove codes with hyphens (ranges)
  # Remove codes not starting with a letter (e.g., .A15)
  filter(grepl("^[A-Z]", code, ignore.case = TRUE)) 

# View(red_index_outpat %>% head(100))
red_index_outpat$code[1:100]


  # Combine inpatient, outpatient, and drug data into one long format
ptra_combined_index_data <- bind_rows(
  red_index_inpat %>% mutate(exam_dt = indatuma, 
                             type = "diagnosis_inpatient", 
                             lopnr_riskset = paste0(lopnr, "_", riskset)) %>%  
                      select(lopnr_riskset, exam_dt, type, code) ,

  red_index_outpat %>% mutate(exam_dt = indatuma, 
                              type = "diagnosis_outpatient", 
                              lopnr_riskset = paste0(lopnr, "_", riskset)) %>%    # outpatients have a length of stay of 1 day
                      select(lopnr_riskset, exam_dt, type, code) ) %>%
arrange(lopnr_riskset, exam_dt) %>% distinct()



# View(head(ptra_combined_index_data, 100))
ptra_combined_index_data %>% filter(!is.na(code)) %>%
head(10000) %>%
View()

dim(ptra_combined_index_data)
# [1] 309508      4

# 5- Save the data for further analyses ------------------------------------------------------------
save(ptra_combined_index_data, file = paste0(output_file, "/ptra_combined_index_data_", toupper(cohort), ".Rdata"))


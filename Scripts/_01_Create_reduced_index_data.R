

#' Author: Antoine KDA
#' Date of creation: 04 September 2025
#' Last modification date: 
#' Last modification date: 
 


# Objectives: Data management ---------------------#
#' 
#' Main task: Reduce the dataset for efficiency in computation
#' 
#' Specific sub-tasks: 
#' 1- Exclude duplicated patients in the index data, if case, remove entire riskset, if comparator, remove only the comparator
#' 2- Identify all diagnosis and prescriptions in the inpatient, outpatient, and prescribed drug data 
#'      with the 10-year lookback up to 3-month before MCL diagnosis
#' 3- Create a dataset with patients, years, diagnoses, and prescriptions, and length of stay
#'    
#'    
#' Changes: 
#' No changes yet
#' 
#' -----------------------------------------------------------------------------#

#' NOTE: 
#' Include MCL patients from 2007 onward as the PDR data are only available
#' for this period and onward.


# 1- Import the index data ------------------------------------------------------------

filpath <- "../00_Original_Data_Antoine/raw_data"

# load the index data 
index <- rio::import(file.path(filpath, "index_pop.csv")); dim(index)

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

# 2- Find all patients with 10 years and 3 MONTHS lookback in the prescribed drug data (point 2) ----------------------------

# Load the prescribed drug data
preddrug <- rio::import(file.path(filpath, "pdr_fk.csv"))
# summary(preddrug$edatum)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2005-04-13" "2009-12-22" "2013-10-17" "2013-09-01" "2017-06-29" "2020-12-31"

# Limit the prescription data to those with 10-year lookback between "2007-01-01" and their most recent available prescription date
index_drug <- preddrug %>%
  right_join(index %>% select(lopnr, riskset, entry_dt), by = c("lopnr")) %>%
  mutate(edatum = as.Date(edatum), 
         entry_dt = as.Date(entry_dt)) %>%
  filter(edatum >= (entry_dt - end_period)) %>% 
  filter(edatum <= (entry_dt - start_period)) # And the most recent prescription date is 3-month before the entry_dt

# Calculate the time to MCL diagnosis
 index_drug %>% 
  mutate(time_to_MCL_Matching = as.numeric(entry_dt - edatum)) %>% 
  select(lopnr, entry_dt, edatum, time_to_MCL_Matching) %>% 
  summary()
nrow(index_drug) - nrow(preddrug)
# [1] -5148183


# 3- Find all patients with 10 years lookback in the inpatient and outpatient data (point 3) ----------------------------

# Load the inpatient data
inpatient <- rio::import(file.path(filpath, "spr_fk.csv"))

# limit to diagnoses between 10 years and 3 months before MCL diagnosis
index_inpat <- inpatient %>% 
  right_join(index %>% select(lopnr, riskset, entry_dt), by = c("lopnr")) %>%
  mutate(indatuma = as.Date(indatuma), 
         entry_dt = as.Date(entry_dt)) %>%
  filter(indatuma <= ymd(as.Date(entry_dt) - start_period)) %>% 
  filter(indatuma >= ymd(as.Date(entry_dt) - end_period)) 

nrow(index_inpat) - nrow(inpatient)
# check if everything worked well
index_inpat %>%
  mutate(time_to_MCL_Matching = as.numeric(entry_dt - indatuma)/365.24) %>%
  select(entry_dt, indatuma, time_to_MCL_Matching) %>% 
  summary()

# Load the outpatient data
outpatient <- rio::import(file.path(filpath, "opr_fk.csv"))

# limit to diagnoses between 10 years and 3 months before MCL diagnosis
index_outpat <- outpatient %>% 
  right_join(index %>% select(lopnr, riskset, entry_dt), by = c("lopnr")) %>%
  mutate(indatuma = as.Date(indatuma), 
         entry_dt = as.Date(entry_dt)) %>%
  filter(indatuma <= ymd(as.Date(entry_dt) - start_period)) %>% 
  filter(indatuma >= ymd(as.Date(entry_dt) - end_period))

# nrow(index_outpat) - nrow(outpatient)
# # check if everything worked well
# index_outpat %>%
#   mutate(time_to_MCL_Matching = as.numeric(entry_dt - indatuma)/365.24) %>%
#   select(entry_dt, indatuma, time_to_MCL_Matching) %>% 
#   summary()


# 4- Save the data for further analyses ------------------------------------------------------------

save(index, index_drug, index_inpat, index_outpat, 
     file = paste0(output_file, "/index_data_", toupper(cohort), ".Rdata"))
     

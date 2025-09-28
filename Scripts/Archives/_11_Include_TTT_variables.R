

#' Author: Antoine KDA
#' Date of creation: 29 May 2024
#' Last modification date: 07 September 2025 


# Objectives: Data management: Include treatment variables --------------------#
#' 
#' Main task: Identify MIPI, Stage, and other TTT variables from the quality 
#'            register data
#'
#'
#'
#' 
#' -----------------------------------------------------------------------------#



# 1- The lymphomareg data ------------------------------------------------------------
# load the lymphomareg data 
filpath <- "../version1.2/data"

lymphomareg <- rio::import(file.path(filpath, "lymphomareg.csv"))



# names in the data
names(lymphomareg)


# select important variables 

lymphomareg %>% 
  select(lopnr, riskset, contains(c("stadium_", "perfwho_", "a_nodal_extra"))) -> trtred

apply(trtred[, grep("stadium_|perfwho_|nodal_extra", names(trtred), value = T)], 2 , table, exclude = NULL)


# 2- Recode Stage variable ------------------------------------

trtred$Stage <-  trtred$stadium_beskrivning
trtred$Stage <- gsub("Musshoff Pe ", "Ann Arbor ", trtred$Stage, ignore.case = T)
trtred$Stage <- gsub("St Jude ", "Ann Arbor ", trtred$Stage, ignore.case = T)

trtred$Stage <- gsub("II1", "II", trtred$Stage, ignore.case = T)
trtred$Stage <- gsub("II2", "II", trtred$Stage, ignore.case = T)

# table(trtred$stadium_beskrivning, trtred$Stage, exclude = NULL)
# table(is.na(trtred$stadium_beskrivning), !is.na(trtred$stadium_varde), exclude = NULL)
# View(trtred[!is.na(trtred$stadium_beskrivning) & is.na(trtred$stadium_varde), ])
# table(trtred$a_nodal_extra_beskrivning, exclude = NULL)
# table(trtred$a_nodal_extra_beskrivning, trtred$a_nodal_extra_varde, exclude = NULL)


# Recode missing information in stage
trtred$Stage[is.na(trtred$Stage) | trtred$Stage %in% c("Oklart", "")] <- "Missing"
table(trtred$Stage, exclude = NULL)

trtred$StageAA <- trtred$Stage; trtred$Stage <- NULL



# 3- Recode Performance variable ------------------------------------

table(trtred$perfwho_beskrivning, trtred$perfwho_varde, exclude = NULL)

trtred %<>% 
  mutate(Performance = case_when(
    perfwho_varde %in% c(0, 1) ~ "0 - 1", 
    perfwho_varde %in% c(2, 3, 4) ~ "2 - 4", 
    perfwho_varde %in% c(9, NA) ~ "Missing"
  ))

table(trtred$Performance, trtred$perfwho_varde, exclude = NULL)
rm(lymphomareg)


# 4- Merge with the cvd_index_labelled data -----------------------------------------

# Load the CVD and index pop data
load(paste0(output_file, "/Studydata_", toupper(cohort), ".Rdata"))

# Merge
cvd_index_labelled %>% 
  filter(case_cat == "Patients") %>% 
  left_join(trtred) -> Patient_trt_CVD

# 5-Update Table 3: Description of DLBCL patients only by CVD status ---------
# Table 3: Description of DLBCL patients only by CVD status


# Recode the variables of interest
Patient_trt_CVD %<>% 
  mutate(StageAA_cat = case_when(
    StageAA %in% c("Ann Arbor I", "Ann Arbor II") ~ 0,
    StageAA %in% c("Ann Arbor III", "Ann Arbor IV") ~ 1,
    StageAA == "Missing" ~ NA
  )) %>% 
  mutate(Performance_cat = case_when(
    Performance == "0 - 1" ~ 0, 
    Performance == "2 - 4" ~ 1, 
    Performance == "Missing" ~ NA
  )) #  %>% 
  # filter(StageAA != "Missing") %>% 
  # filter(Performance != "Missing")

# class(Patient_trt_CVD$StageAA_cat)
# class(Patient_trt_CVD$Performance_cat)
# table(Patient_trt_CVD$StageAA, Patient_trt_CVD$StageAA_cat, exclude = NULL)
# table(Patient_trt_CVD$Performance, Patient_trt_CVD$Performance_cat, exclude = NULL)
# table(Patient_trt_CVD$StageAA_cat, Patient_trt_CVD$CVD_Status_cat, exclude = NULL)
# prop.table(table(Patient_trt_CVD$StageAA_cat, Patient_trt_CVD$CVD_Status_cat, exclude = NULL))





# Variables to describe
vec <- c("age_entry_dt", "age_entry_dt_cat", "age_entry_dt_bin", "Sex", "Year_entry_dt_cat", "partner_cat",
        "educ_max", "fam_hist_lymph",  "fam_hist_hl", "fam_hist_nhl", 
         "overweight_obesity_ever", "rheumatoid_arthritis_ever", "hyperlipidaemia_ever", 
         "CVD_Status", "first_all_cvd_year", "Max_Enum_CVD_cat", "StageAA", "Performance")

vec[!vec %in% names(Patient_trt_CVD)]


# Create an empty list to store the results
Results_tables <- list()

# Description of DLBCL patients only stratified by CVD status 
# Table3_desc <- desc.factor.2levels(data = Patient_trt_CVD, var_to_desc = vec, collapse = TRUE, 
#                                    desc_by = "CVD_Status_cat", digits = 1)

# Recode CVD_Status levels 0 and 1 to "CVD" and "No CVD"
Patient_trt_CVD %<>% 
  mutate(CVD_Status = case_when(
    CVD_Status == 0 ~ "No CVD",
    CVD_Status == 1 ~ "CVD"
  ))

pat_desc_all <- create_table(data = Patient_trt_CVD %>% select(all_of(vec)) %>% mutate(across(setdiff(vec, "age_entry_dt"), as.factor)), 
                     strata_var = "CVD_Status", 
                     labels = manual_labels, 
                     add_smd = FALSE, 
                     add_CI = FALSE, 
                     add_p = TRUE)

Results_tables[["Tab3 DLBCL Pat only Desc"]] <- pat_desc_all


# Description stratified by Stage status 
vec <- c("age_entry_dt", "age_entry_dt_cat", "age_entry_dt_bin", "Sex", "Year_entry_dt_cat", "partner_cat",
        "educ_max", "fam_hist_lymph",  "fam_hist_hl", "fam_hist_nhl", 
         "overweight_obesity_ever", "rheumatoid_arthritis_ever", "hyperlipidaemia_ever", 
         "CVD_Status", "first_all_cvd_year", "Max_Enum_CVD_cat", "StageAA_cat", "Performance")

pat_desc_stage <- create_table(data = Patient_trt_CVD %>% select(all_of(vec)) %>% mutate(across(setdiff(vec, "age_entry_dt"), as.factor)), 
                     strata_var = "StageAA_cat", 
                     labels = manual_labels, 
                     add_smd = FALSE, 
                     add_CI = FALSE, 
                     add_p = TRUE)

Results_tables[["Tab4 DLBCL Pat only Desc Stage"]] <- pat_desc_stage

# Description stratified by Performance status 

pat_desc_performance <- create_table(data = Patient_trt_CVD %>% select(all_of(vec)) %>% mutate(across(setdiff(vec, "age_entry_dt"), as.factor)), 
                     strata_var = "Performance", 
                     labels = manual_labels, 
                     add_smd = FALSE, 
                     add_CI = FALSE, 
                     add_p = TRUE)

Results_tables[["Tab5 DLBCL Pat only Desc Perf"]] <- pat_desc_performance

# 6- Export the results to an excel file ------------------

t <- 3
for (sht in names(Results_tables)) { 
    # Remove sheet if it already exists
    if (sht %in% openxlsx::sheets(wb)) {
        print(paste("Sheet", sht, "already exists. Removing and rewriting it."))
        openxlsx::removeWorksheet(wb, sht)
    }
    openxlsx::addWorksheet(wb, sht)

    if(t == 3){
      openxlsx::writeData(wb, sht, x = paste("Table", t, ": Description of DLBCL patients only stratified by CVD status"), startCol = 1, startRow = 2)
    } else if (t == 4) {
      openxlsx::writeData(wb, sht, x = paste("Table", t, ": Description of DLBCL patients only stratified by Stage status"), startCol = 1, startRow = 2)
    } else if (t == 5) {
      openxlsx::writeData(wb, sht, x = paste("Table", t, ": Description of DLBCL patients only stratified by Performance status"), startCol = 1, startRow = 2)
    }
    t <- t + 1
    # Write the table starting from row 5 to leave space for the title
    openxlsx::writeDataTable(wb, sht, x = Results_tables[[sht]] %>% as.data.frame(), startCol = 1, startRow = 5)
}

# Save the workbook
openxlsx::saveWorkbook(wb, file = paste0(result_file, "/Results_", toupper(cohort), ".xlsx"), overwrite = TRUE)



# 7- Save the final data -----------------------------------------

save(Patient_trt_CVD, file = paste0(output_file, "/Patient_trt_CVD_", toupper(cohort), ".Rdata"))

# Export to excel 
openxlsx::write.xlsx(Patient_trt_CVD, file = paste0(output_file, "/Patient_trt_CVD_", toupper(cohort), ".xlsx"), overwrite = TRUE)




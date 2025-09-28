




#' Author: Antoine KDA
#' Date of creation: 04 September 2025
#' Last modification date:

# Objectives: Descriptions --------------------------#
#' 
#' Table 1: Description of the study population + p-value
#' Table 2: Description of types of CVDs
#' Table 3: Description of MCL patients only by CVD status
#' 
#' 
#' ----------------------------------------------------------------------------#


# Load the CVD and index pop data
# e <- new.env()
# load(paste0(output_file, "/Studydata_", toupper(cohort), ".Rdata"), envir = e)
# cvd_index_labelled <- e$cvd_index_labelled
# rm(e)


#' List of important variables to describe
#' case :==> case
#' Age at MCL diagnosis :==> age_entry_dt
#' Sex :==> female
#' Year at MCL diagnosis :==> entry_dt
#' Marital status :==> partner
#' Family histry of lymphoma :==> fam_hist_lymph, fam_hist_hl, fam_hist_nhl
#' Educ max :==>  educ_max
#' Obesity  :==> OverWgt
#' Stage of MCL
#' Performance Status Ecog
#' 


# # 1- Create a function to describe the study population ------------------


# Define a function to create summary tables 

create_table <- function(data, strata_var=NULL, labels=labels_all, add_smd=TRUE, add_CI=TRUE, add_p=TRUE) {

    if (length(unique(na.omit(data[,c(strata_var)])))>2) {
        print("WARNING: there are too many levels in the strata to calculate SMDs")
        add_smd <- FALSE
    }
    
    summary_table <- data %>%
        tbl_summary(
        by = strata_var,
        type = list(where(is.numeric) ~ "continuous2",
                    where(is.integer) ~ "continuous2",
                    all_categorical() ~ 'categorical',
                    all_dichotomous() ~ 'dichotomous'
                    ),
        statistic = list(
            all_continuous() ~ c("{N_nonmiss}", 
                                "{mean} ({sd})", 
                                "{median} ({p25}-{p75})",
                                "{p5}-{p95}",
                                "{min}-{max}"),
            all_categorical() ~ c("{n} ({p}%)"),  
            all_dichotomous() ~ c("{n} ({p}%)") 
        ),
        missing="ifany",
        label=labels,
        digits = list(
            all_dichotomous() ~ c(n=0, p=1),
            all_categorical() ~ c(n=0, p=1),
            all_continuous() ~ c(N_nonmiss=0, mean=2, sd=2, median=1, p5=1, p95=1, 
                                p25=1, p75=1, min=1, max=1)
        )
        ) %>% 
        add_overall() %>%
        add_stat_label()
    
    # Fix the conditional logic for add_p
    if (is.null(add_p) || add_p) {
        summary_table <- summary_table %>% add_p()
        summary_table
    }

        if (add_smd==TRUE) {
        summary_table <- summary_table %>%        
        add_difference(test = everything() ~ "smd") 
        summary_table
    }

        
    if (add_CI==TRUE) {
        summary_table <- summary_table %>%
            add_ci(pattern = NULL, 
            method=list(all_categorical() ~ "wilson",
                        all_dichotomous() ~ "wilson",
                        all_continuous() ~ 't.test'),
            style_fun = list(all_continuous() ~ purrr::partial(style_number, digits=2),
                                all_categorical() ~ purrr::partial(style_number, digits=2, scale=100),
                                all_dichotomous() ~ purrr::partial(style_number, digits=2, scale=100)))
    }
    
    return(summary_table)
}


# 2- Create an automatic summary table ------------------
Results_tables <- list()
# wb <- createWorkbook()
# sht <- addWorksheet(wb, "Abst2 Baseline Char")


# Table 1: Description of the study population
# Variables to describe
# names(cvd_index_labelled)
vec <- c("age_entry_dt", "age_entry_dt_cat", "age_entry_dt_bin", "Sex", "Year_entry_dt_cat", "partner_cat",
        "educ_max", "fam_hist_lymph",  "fam_hist_hl", "fam_hist_nhl", 
         "overweight_obesity_ever", "rheumatoid_arthritis_ever", "hyperlipidaemia_ever", 
         "CVD_Status", "case_cat", "first_all_cvd_year", "Max_Enum_CVD_cat")


desc_all <- create_table(data = cvd_index_labelled %>% select(all_of(vec)) %>% mutate(across(setdiff(vec, "age_entry_dt"), as.factor)), 
                     strata_var = "case_cat", 
                     labels = manual_labels, 
                     add_smd = FALSE, 
                     add_CI = FALSE, 
                     add_p = TRUE)

Results_tables[["Tab1 Desc study pop"]] <- desc_all

# Table 2: Description of types of CVDs
# Variables to describe

# load(paste0(output_file, "/index_data_codelist_", toupper(cohort), ".Rdata"), verbose = TRUE)

# e <- new.env()
# load(paste0(output_file, "/index_data_codelist_", toupper(cohort), ".Rdata"), envir = e)
# cvd_codes <- e$cvd_codes
# rm(e)

vec <- c(paste0(names(cvd_codes)[!names(cvd_codes) %in% c("overweight_obesity", "rheumatoid_arthritis", "hyperlipidaemia")], "_ever"), "case_cat", "atherosclerotic_cvd_ever")

desc_type_cvd <- create_table(data = cvd_index_labelled %>% select(all_of(vec)) %>% mutate(across(everything(), as.factor)), 
                     strata_var = "case_cat", 
                     labels = manual_labels, 
                     add_smd = FALSE, 
                     add_CI = FALSE, 
                     add_p = TRUE)

Results_tables[["Tab2 Desc typ CVDs"]] <- desc_type_cvd

# # 2bis- Create table 2: Description of types of CVD by age group ----------------------------------

desc_type_cvd_age64 <- create_table(data = cvd_index_labelled %>% 
                        filter(age_entry_dt_bin == "<=64") %>%
                        select(all_of(vec)) %>%
                        mutate(across(everything(), as.factor)), 
                     strata_var = "case_cat", 
                     labels = manual_labels, 
                     add_smd = FALSE, 
                     add_CI = FALSE, 
                     add_p = TRUE)

desc_type_cvd_age65 <- create_table(data = cvd_index_labelled %>% 
                        filter(age_entry_dt_bin == "65+") %>%
                        select(all_of(vec)) %>%
                        mutate(across(everything(), as.factor)), 
                     strata_var = "case_cat", 
                     labels = manual_labels, 
                     add_smd = FALSE, 
                     add_CI = FALSE, 
                     add_p = TRUE)

Results_tables[["Tab2 Desc typ CVDs age64"]] <- desc_type_cvd_age64
Results_tables[["Tab2 Desc typ CVDs age65plus"]] <- desc_type_cvd_age65


# # Table 3: Description of MCL patients only by CVD status
# # Variables to describe
# names(indexCVD)
# vec <- c("age_entry_dt_cat", "Sex", "Year_entry_dt_cat", "partner_cat",
#          "educ_max", "OverWgt", "Reumatoid_Artrit","fam_hist_lymph_cat", 
#          "fam_hist_hl_cat", "fam_hist_nhl_cat")


# desc_all <- create_summary_table(data = indexCVD, 
#                      strata_var = "case_cat", 
#                      labels = all_labels, 
#                      add_smd = FALSE, 
#                      add_CI = FALSE)




# 3- Export the results to an excel file ------------------
for (sht in names(Results_tables)) {
    # Remove sheet if it already exists
    if (sht %in% openxlsx::sheets(wb)) {
        print(paste("Sheet", sht, "already exists. Removing and rewriting it."))
        openxlsx::removeWorksheet(wb, sht)
    }
    openxlsx::addWorksheet(wb, sht)
    openxlsx::writeDataTable(wb, sht, x = Results_tables[[sht]] %>% as.data.frame(), startCol = 1, startRow = 5)
}

openxlsx::saveWorkbook(wb, file = paste0(result_file, "/Results_", toupper(cohort), ".xlsx"), overwrite = TRUE)

# Save data 
# save(cvd_index_labelled, file = paste0(output_file, "/Studydata_desc_", toupper(cohort), ".Rdata"))

# "./Output files/Studydata_desc.Rdata")

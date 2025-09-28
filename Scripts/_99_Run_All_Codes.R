

#' Author: Antoine KDA
#' Date of creation: 05 September 2025
#' Last modification date: 
#' Last modification date: 
 


# Objectives: Create output folders and run all scripts automatically ---------------------#


# -------------------- INITIALIZATION --------------------
# Create the folders if they do not exist
# Run only one time

# dir.create("Datasets") # Already exists
dir.create("Output files")
dir.create("Scripts") # Already exists

# Clear the environment and load the functions
rm(list = ls())
source("./Scripts/_00_List_of_functions.R")

# Packages 
install(c("dplyr", "magrittr", "rio", "lubridate", "tidyr",
  "labelled", "gtsummary", "purrr", "survival", "forestmodel", 
  "forestplot", "broom", "gt", "patchwork", "magick", "openxlsx"))

# -------------------- RUN ANALYSES --------------------

for (cohort in c( "main", 
"lag0month", "lag12months", "severeonly"
)) {
  if (cohort == "main") {
    start_period <- (365.24/12)*3 # 3 months
    end_period <- 365.24*10 # 10 years
  } else if (cohort == "lag0month") {
    start_period <- (365.24/12)*0 # 0 months
    end_period <- 365.24*10 # 10 years
  } else if (cohort == "lag12months") {
    start_period <- (365.24/12)*12 # 12 months
    end_period <- 365.24*10 # 10 years
  } else if (cohort == "severeonly") {
    start_period <- (365.24/12)*3 # 3 months
    end_period <- 365.24*10 # 10 years
  }
  
  output_file <- paste0("./Output files/", gsub(" ", "_", format(Sys.time(), "%d %b %Y")), "_Output_Files_", toupper(cohort))
  result_file <- paste0("./Output files/", gsub(" ", "_", format(Sys.time(), "%d %b %Y")), "_Results_", toupper(cohort))
  
  dir.create(output_file)
  dir.create(result_file)

  # Load workbook if exists, otherwise create a new one
  wb_path <- paste0(result_file, "/Results_", toupper(cohort), ".xlsx")
  if (file.exists(wb_path)) {
    wb <- openxlsx::loadWorkbook(wb_path)
  } else {
    wb <- openxlsx::createWorkbook()
  }
  
  scripts <- list.files("./Scripts/", pattern = "^_\\d{2}_.*\\.R$", full.names = TRUE)
  # Exclude _00_ and _99_ scripts
  scripts <- scripts[!grepl("_00_|_99_", basename(scripts))]
  # Order scripts numerically by the number after the underscore
  scripts <- scripts[order(as.numeric(sub("^_(\\d{2})_.*", "\\1", basename(scripts))))]
  for (script in scripts) {
    cat("Running script:", script, "\n")
    source(script)
  }
  # Save the workbook
  openxlsx::saveWorkbook(wb, file = wb_path, overwrite = TRUE)

  # Clear workspace before next cohort run
  rm(list = ls())

}






# Dones:
# Environmental variables

# In the runner file,  create environmental variables for running sensitivity analysis and move all output into a named folder with date. Done
# Move all tables to one Excel sheet. DONE
# Use write table that provides a better outputs in the excel.  Done
# Build a model with year since the first ever cvd overall + stratification table + description table, Done
# Avoid over adjustment in the cvd specific regression. Done
# Include the atherosclerotic cvd into the resulsts pipeline: Table 1, 2, 3 + regression models. Done

# To be dones
# Add descriptions to the figures and tables in the excel sheet.
# Export the code list used and all important information: look back period, first cvd, etc...



# Changes I made: 
# - included the icd9 codes into the code lists.
# I removed duplicated patients (comparators that later became cases) in the data management script
# So now I need to recreate the descriptive table with the updated dataset  
# No change in the conclusions.
# I added hyperlipidaemia as a covariate in the regression models. Because it is a risk factor for CVD and not a cvd per se.
# I added a model with adjustment on the time from first ever CVD to MCL diagnosis or matching date.
# I added a model with maximum number of CVDs as exposure.

# -------------------- END OF THE SCRIPT --------------------












# Compare the diagnosis in dx in ptra_all_diagnosis to the diagnosis in dx in diagnosis file. 

# Import diagnosis data
diagnosis <- read.csv("C:\\Users\\kwhr625\\Downloads\\diagnosis.csv")
head(diagnosis)
ncol(diagnosis)

head(ptra_all_diagnosis)
ncol(ptra_all_diagnosis)


# Set the names of the diagnosis columns to be the same for merging
names(diagnosis) <- names(ptra_all_diagnosis)

# check for difference 
diff_diagnosis <- anti_join(ptra_all_diagnosis, diagnosis, by = c("patient_id", "dx"))
dim(diff_diagnosis)
dim(ptra_all_diagnosis)
dim(diagnosis)

(set_diff <- setdiff(ptra_all_diagnosis$dx, diagnosis$dx))
table(nchar(set_diff))
table(nchar(diagnosis$dx))
table(nchar(ptra_all_diagnosis$dx))

set_diff


# Read ICD data from  text file 
icd_data <- read.table("C:\\Users\\kwhr625\\Box\\Personal\\Old_Studies\\CVD_in_DLBCL\\Clustering_CVD\\Data\\Codelists_ICD_10_9_ATC\\icd102019enMeta\\icd102019syst_codes.txt", 
                       sep = ";", header = FALSE, stringsAsFactors = FALSE, fill = TRUE, quote = "", comment.char = "") 

View(icd_data)
names(icd_data)

(set_diff_icd <- setdiff(set_diff, icd_data$V7))
(set_diff_icd_lvl3 <- setdiff(substr(set_diff, 1, 3), icd_data$V7))
(set_diff_icd_lvl2 <- setdiff(substr(set_diff, 1, 2), icd_data$V7))


# Find all diagnoses that are not in the ICD codes
missing_icd <- ptra_all_diagnosis %>%
    anti_join(icd_data, by = c("dx" = "V7"))
dim(missing_icd)
# [1]  0 10
nrow(missing_icd)
# [1] 0
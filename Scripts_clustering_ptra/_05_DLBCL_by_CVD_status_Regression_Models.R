





#' Author: Antoine KDA
#' Date of creation: 04 September 2025
#' Last modification date:

# Objectives: Regression Models -----------------------------------------------#
#' 
#' Comparison classic logistic regression versus conditional logistic regression
#' Adjusted regression models
#' 
#' 
#' ----------------------------------------------------------------------------#


# Load the CVD and index pop data
# load("./Output files/Studydata_desc.Rdata")


# View(cvd_index_labelled[, c("lopnr", "case", "riskset", "Sex", "CVD_Status")])

# 1- Conditional logistic regression model ----------------------------------#

# apply(cvd_index_labelled[, c("Sex", "CVD_Status", "age_entry_dt_cat", "Year_entry_dt_cat")], 2, 
#       table, exclude = NULL)

class(cvd_index_labelled$case_cat)
class(cvd_index_labelled$case)
class(cvd_index_labelled$age_entry_dt_cat)
class(cvd_index_labelled$Sex)
cvd_index_labelled$Sex <- as.factor(cvd_index_labelled$Sex)
class(cvd_index_labelled$Year_entry_dt_cat)
cvd_index_labelled$Year_entry_dt_cat <- as.factor(cvd_index_labelled$Year_entry_dt_cat)



# clmod <- clogit(case ~ CVD_Status + partner_cat + educ_max + overweight_obesity_ever + hyperlipidaemia_ever + fam_hist_lymph_cat + 
#                   rheumatoid_arthritis_ever + strata(as.factor(riskset)), data = cvd_index_labelled)

# clmod

# # Classic logistic model 
# glmfit <- glm(case ~ CVD_Status + Sex + age_entry_dt_cat + Year_entry_dt_cat +
#                 partner_cat + educ_max + overweight_obesity_ever + hyperlipidaemia_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever,
#               data = cvd_index_labelled, family = binomial(link = "logit"))
# glmfit

# summary(glmfit)


# # Compare the classic logistic model vs conditional model
# cbind(exp(coef(glmfit)), exp(confint(glmfit)))


# # Waiting for profiling to be done...
# #                                            2.5 %    97.5 %
# # (Intercept)                0.09957087 0.09376545 0.1056875
# # CVD_Status                 1.02362939 0.97850080 1.0708773
# # SexMale                    1.00276686 0.96319675 1.0440129
# # age_entry_dt_cat61-70      0.97674027 0.92188475 1.0348848
# # age_entry_dt_cat71-80      0.98179568 0.92606368 1.0409587
# # age_entry_dt_cat81+        0.99308759 0.92814955 1.0624957
# # Year_entry_dt_cat2006-2010 0.98931745 0.93480936 1.0469950
# # Year_entry_dt_cat2011-2015 0.98451028 0.93043104 1.0417577
# # Year_entry_dt_cat2016-2019 0.98562450 0.92819423 1.0465451
# # partner_catBNever married  0.95691843 0.91855048 0.9968357
# # educ_max.L                 0.97805581 0.94075408 1.0167085
# # educ_max.Q                 0.99319112 0.96039149 1.0271802
# # overweight_obesity_ever    1.03811814 0.87074244 1.2283806
# # fam_hist_lymph_catYes      1.46622210 1.33686513 1.6051136
# # rheumatoid_arthritis_ever  2.42757494 2.11092161 2.7815056

# cbind(exp(coef(clmod)), exp(confint(clmod)))
# #                                         2.5 %    97.5 %
# # CVD_Status                1.0254576 0.9795674 1.0734976
# # partner_catBNever married 0.9548649 0.9158883 0.9955001
# # educ_max.L                0.9757369 0.9381691 1.0148090
# # educ_max.Q                0.9906974 0.9578196 1.0247038
# # overweight_obesity_ever   1.0412022 0.8765992 1.2367135
# # fam_hist_lymph_catYes     1.4620397 1.3338343 1.6025679
# # rheumatoid_arthritis_ever 2.4470541 2.1305348 2.8105965




#' Conclusion from the comparisons: 
#' Estimate of CVD effect from the classic logistic regression is the same as the 
#' one from the conditional model. 
#' estimates for age, sex, and year were almost 1, and all non-significant meaning
#' no need to keep them in the model if using classic regression.

# 2- Conditional logistic regression model ----------------------------------#

## 2.1- Adjusted for partner, education, overweight, and fam hist of lymp -----#


# recode marital status
unique(cvd_index_labelled$partner_cat)
table(nchar(as.character(cvd_index_labelled$partner_cat)))
cvd_index_labelled$partner_cat <- substr(cvd_index_labelled$partner_cat, start = 2, stop = nchar(as.character(cvd_index_labelled$partner_cat)))
unique(cvd_index_labelled$partner_cat)
cvd_index_labelled$partner_cat <- as.factor(cvd_index_labelled$partner_cat)

# Recode educ_max
unique(cvd_index_labelled$educ_max)
cvd_index_labelled$educ_max <- substr(cvd_index_labelled$educ_max, start = 2, stop = nchar(as.character(cvd_index_labelled$educ_max)))
cvd_index_labelled$educ_max <- as.factor(cvd_index_labelled$educ_max)

# # Rename the variables for the output
# cvd_index_labelled %<>% 
#   rename("Cardiovascular Disease" = CVD_Status_cat,
#          "Marital Status" = partner_cat,
#          "Highest Education Level" = educ_max,
#          "Overweight" = overweight_obesity_ever,
#          "Family History of Lymphoma" = fam_hist_lymph_cat,
#          "Family History of Hodgkin Lymphoma" = fam_hist_hl_cat,
#          "Family History of Non-Hodgkin Lymphoma" = fam_hist_nhl_cat, 
#          "Rheumatoid arthritis" =  rheumatoid_arthritis_ever)




### With adjust on family history of lymphoma
clmod_lymph <- clogit(case ~ CVD_Status + partner_cat + factor(educ_max) + overweight_obesity_ever + hyperlipidaemia_ever +
                        fam_hist_lymph_cat + rheumatoid_arthritis_ever +
                        strata(as.factor(riskset)), data = cvd_index_labelled)

clmod_lymph
summary(clmod_lymph)


### With adjust on family history of HL
clmod_HL <- clogit(case ~ CVD_Status + partner_cat + factor(educ_max) + overweight_obesity_ever + hyperlipidaemia_ever +
                     fam_hist_hl_cat + rheumatoid_arthritis_ever +
                     strata(as.factor(riskset)), data = cvd_index_labelled)

clmod_HL
summary(clmod_HL)

### With adjust on family history of NHL
clmod_NHL <- clogit(case ~ CVD_Status + partner_cat + factor(educ_max) + overweight_obesity_ever + hyperlipidaemia_ever +
                      fam_hist_nhl_cat + rheumatoid_arthritis_ever +
                      strata(as.factor(riskset)), data = cvd_index_labelled)

clmod_NHL
summary(clmod_NHL)



# # Forestplot of the models:

# class(clmod_lymph)
# forestmodel::forest_model(clmod_lymph)
# forestmodel::forest_model(clmod_HL)
# forestmodel::forest_model(clmod_NHL)



# 3- Forest plot of the regression models ----------------------------------#

# Create a panel list object 

# Example with custom panels from the package forestplot

panels <- list(
  list(width = 0.03),
  list(width = 0.1, display = ~variable, fontface = "bold", heading = "Variable"),
  list(width = 0.1, display = ~level),
  list(width = 0.05, display = ~n, hjust = 1, heading = "N"),
  # list(width = 0.05, display = ~n_events, width = 0.05, hjust = 1, heading = "Events"),
  # list(
  #   width = 0.05,
  #   display = ~ replace(sprintf("%0.1f", person_time / 365.25), is.na(person_time), ""),
  #   heading = "Person-\nYears", hjust = 1
  # ),
  list(width = 0.03, item = "vline", hjust = 0.5),
  list(
    width = 0.55, item = "forest", hjust = 0.5, heading = "Odds ratio", linetype = "dashed",
    line_x = 0
  ),
  list(width = 0.03, item = "vline", hjust = 0.5),
  list(width = 0.12, display = ~ ifelse(reference, "Reference", sprintf(
    "%0.2f (%0.2f, %0.2f)",
    trans(estimate), trans(conf.low), trans(conf.high)
  )), display_na = NA),
  list(
    width = 0.05,
    display = ~ ifelse(reference, "", format.pval(p.value, digits = 1, eps = 0.001)),
    display_na = NA, hjust = 1, heading = "p"
  ),
  list(width = 0.03)
)



(clmod_lymph_exp <- forestmodel::forest_model(clmod_lymph, panels = panels))
(clmod_HL_exp <- forestmodel::forest_model(clmod_HL, panels = panels))
(clmod_NHL_exp <- forestmodel::forest_model(clmod_NHL, panels = panels))

# Plot differently: 
# Plot the forest models with improved layout and titles
library(patchwork)

clmod_lymph_exp <- forestmodel::forest_model(clmod_lymph, panels = panels) +
    ggplot2::ggtitle("Conditional Logistic Regression: Family History of Lymphoma") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"))

clmod_HL_exp <- forestmodel::forest_model(clmod_HL, panels = panels) +
    ggplot2::ggtitle("Conditional Logistic Regression: Family History of HL") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"))

clmod_NHL_exp <- forestmodel::forest_model(clmod_NHL, panels = panels) +
    ggplot2::ggtitle("Conditional Logistic Regression: Family History of NHL") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"))

# Combine plots in a single row for comparison
combined_plot <- clmod_lymph_exp + clmod_HL_exp + clmod_NHL_exp + patchwork::plot_layout(ncol = 1)

windows()
print(combined_plot)




# Export the plot -----------------------
ggsave(combined_plot, file = paste0(result_file, "/FIg1a_Conditional_reg_", toupper(cohort), ".png"), 
       units = "cm", width = 25, height = 30)



# Using gt package 
# Create a tidy summary table for each model using broom
library(broom)
library(gt)

# Helper function to tidy and format model output
tidy_clogit <- function(model, model_name) {
    broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
        dplyr::mutate(model = model_name) |>
        dplyr::select(model, term, estimate, conf.low, conf.high, p.value)
}

# Get tidy tables for each model
tbl_lymph <- tidy_clogit(clmod_lymph, "Family history of lymphoma")
tbl_HL    <- tidy_clogit(clmod_HL, "Family history of HL")
tbl_NHL   <- tidy_clogit(clmod_NHL, "Family history of NHL")

# Format and display with gt
format_regression_table <- function(tbl, title = "Conditional Logistic Regression Results", subtitle = "Odds Ratios (95% CI) and p-values") {
    tbl |>
        dplyr::mutate(
            estimate = sprintf("%.2f", estimate),
            conf.low = sprintf("%.2f", conf.low),
            conf.high = sprintf("%.2f", conf.high),
            OR_CI = paste0(estimate, " (", conf.low, ", ", conf.high, ")"),
            p.value = format.pval(p.value, digits = 2, eps = 0.001)
        ) |>
        dplyr::select(model, term, OR_CI, p.value) |>
        gt::gt() |>
        gt::tab_header(
            title = title,
            subtitle = subtitle
        ) |>
        gt::cols_label(
            model = "Model",
            term = "Variable",
            OR_CI = "Odds Ratio (95% CI)",
            p.value = "p-value"
        ) |>
        gt::fmt_missing(columns = everything(), missing_text = "-")
}

# Example usage:
format_regression_table(tbl_lymph)
# Helper to create annotation rows as data frames
annotation_row <- function(text) {
    data.frame(
        model = text,
        term = NA,
        OR_CI = NA,
        p.value = NA,
        stringsAsFactors = FALSE
    )
}

empty_row <- function() {
    data.frame(
        model = NA,
        term = NA,
        OR_CI = NA,
        p.value = NA,
        stringsAsFactors = FALSE
    )
}

tbl_all <- dplyr::bind_rows(
    annotation_row("Conditional Logistic Regression Results"),
    annotation_row("Odds Ratios (95% CI) and p-values"),
    annotation_row("Model 1 adjusted for: Marital status, Highest education level, Overweight/Obesity diagnosis, Rheumatoid arthritis diagnosis, hyperlipidaemia diagnosis, and Family history of lymphoma"),
    format_regression_table(tbl_lymph) %>% as.data.frame(),
    empty_row(),
    annotation_row("Model 2 adjusted for: Marital status, Highest education level, Overweight/Obesity diagnosis, Rheumatoid arthritis diagnosis, hyperlipidaemia diagnosis, and Family history of HL"),
    format_regression_table(tbl_HL) %>% as.data.frame(),
    empty_row(),
    annotation_row("Model 3 adjusted for: Marital status, Highest education level, Overweight/Obesity diagnosis, Rheumatoid arthritis diagnosis, hyperlipidaemia diagnosis, and Family history of NHL"),
    format_regression_table(tbl_NHL) %>% as.data.frame()
)

View(tbl_all)











# --------------------- Update on 27 April 2025 ---------------------
#' Rerun the general model stratified by age
#


# Stratified conditional logistic regression by age (<65 and >=65)
# Use correct variable names from cvd_index_labelled

clmod_lymph_65 <- clogit(case ~ CVD_Status + partner_cat + educ_max + overweight_obesity_ever + hyperlipidaemia_ever +
                         fam_hist_lymph_cat + rheumatoid_arthritis_ever +
                         strata(as.factor(riskset)),
                         data = cvd_index_labelled[cvd_index_labelled$age_entry_dt < 65, ])

clmod_lymph_65
summary(clmod_lymph_65)

clmod_lymph_65plus <- clogit(case ~ CVD_Status + partner_cat + educ_max + overweight_obesity_ever + hyperlipidaemia_ever +
                             fam_hist_lymph_cat + rheumatoid_arthritis_ever +
                             strata(as.factor(riskset)),
                             data = cvd_index_labelled[cvd_index_labelled$age_entry_dt >= 65, ])

clmod_lymph_65plus
summary(clmod_lymph_65plus)

# Export models results into a table


# Save as forestplot 
(clmod_lymph_exp65 <- forestmodel::forest_model(clmod_lymph_65, panels = panels))
(clmod_lymph_exp65plus <- forestmodel::forest_model(clmod_lymph_65plus, panels = panels))

    # Save as forestplot 
    clmod_lymph_exp65 <- forestmodel::forest_model(clmod_lymph_65, panels = panels) +
        ggplot2::ggtitle("Conditional Logistic Regression: Age < 65") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"))

    clmod_lymph_exp65plus <- forestmodel::forest_model(clmod_lymph_65plus, panels = panels) +
        ggplot2::ggtitle("Conditional Logistic Regression: Age ≥ 65") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"))

    # Combine the two plots vertically
    combined_age_plot <- clmod_lymph_exp65 + clmod_lymph_exp65plus + patchwork::plot_layout(ncol = 1)

    windows()
    print(combined_age_plot)



# Get tidy tables for each model
clmod_lymph_65 <- tidy_clogit(clmod_lymph_65, "Family history of lymphoma stratified age < 65")
clmod_lymph_65plus <- tidy_clogit(clmod_lymph_65plus, "Family history of lymphoma stratified age >= 65")

# Combine all tables
tbl_all <- dplyr::bind_rows(
    tbl_all,
    empty_row(),
    annotation_row("Conditional Logistic Regression Results Stratified by Age"),
    annotation_row("Model 4 age < 65 and adjusted for: Marital status, Highest education level, Overweight/Obesity diagnosis, Rheumatoid arthritis diagnosis, hyperlipidaemia diagnosis, and Family history of lymphoma"),
    format_regression_table(clmod_lymph_65) %>% as.data.frame(),
    empty_row(),
    annotation_row("Conditional Logistic Regression Results Stratified by Age"),
    annotation_row("Model 5 age ≥ 65 and adjusted for: Marital status, Highest education level, Overweight/Obesity diagnosis, Rheumatoid arthritis diagnosis, hyperlipidaemia diagnosis, and Family history of lymphoma"),
    format_regression_table(clmod_lymph_65plus) %>% as.data.frame()
)
View(tbl_all)

# Export the plot -----------------------
ggsave(combined_age_plot, file = paste0(result_file, "/FIg1a_Strata_age_Conditional_reg_Lymph_", toupper(cohort), ".png"),
       units = "cm", width = 25, height = 30)











# --------------------- Update on 06 September 2025 ---------------------
#' Rerun the general model adjusted for first_all_cvd_year
#
# table(cvd_index_labelled$first_all_cvd_year, exclude = NULL)

### With adjust on family history of lymphoma
### first_all_cvd_year == Never diagnosed is the reference category
cvd_index_labelled$first_all_cvd_year <- relevel(as.factor(cvd_index_labelled$first_all_cvd_year), ref = "Never diagnosed")
# table(cvd_index_labelled$first_all_cvd_year , exclude = NULL)
yr_cvd_lymph <- clogit(case ~ CVD_Status + partner_cat + factor(educ_max) + overweight_obesity_ever + hyperlipidaemia_ever +
                        fam_hist_lymph_cat + rheumatoid_arthritis_ever + first_all_cvd_year +
                        strata(as.factor(riskset)), data = cvd_index_labelled)

# Get tidy tables for each model
yr_cvd_lymph_tidy <- tidy_clogit(yr_cvd_lymph, "Family history of lymphoma adjusted for year of first CVD")

# Combine all tables
tbl_all <- dplyr::bind_rows(
    tbl_all,
    empty_row(),
    annotation_row("Conditional Logistic Regression Results with Adjustment for Year from First ever CVD to DLBC Diagnosis or matching date"),
    annotation_row("Model 6 adjusted for: Marital status, Highest education level, Overweight/Obesity diagnosis, Rheumatoid arthritis diagnosis, hyperlipidaemia diagnosis, 
    Family history of lymphoma, and Year from first ever CVD to MCL diagnosis or matching date"),
    format_regression_table(yr_cvd_lymph_tidy) %>% as.data.frame()    
)
# View(tbl_all)




# ------------- Repeate the lymph for all the different types of CVD -----------------#
# Use a loop to run the models for each CVD type, tidy the mode and store results in tbl_all with appropriate annotations

cvd_types <- c(paste0(names(cvd_codes)[!names(cvd_codes) %in% c("all_cvd", "overweight_obesity", "rheumatoid_arthritis", "hyperlipidaemia")], "_ever"), "atherosclerotic_cvd_ever")


n = 7
for (cvd in cvd_types) {

    cat("Running model for ", paste(cvd, ":", manual_labels[[cvd]]), "\n")

    # Run the model
    formula_str <- paste0("case ~ ", cvd, " + partner_cat + factor(educ_max) + overweight_obesity_ever + hyperlipidaemia_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever + strata(as.factor(riskset))")
    yr_cvd_lymph <- clogit(as.formula(formula_str), data = cvd_index_labelled)

    # Get tidy tables for each model
    yr_cvd_lymph_tidy <- tidy_clogit(yr_cvd_lymph, paste("Family history of lymphoma adjusted for year of first CVD -", manual_labels[[cvd]]))

    # Combine all tables
    tbl_all <- dplyr::bind_rows(
        tbl_all,
        empty_row(),
        annotation_row(paste(cvd, ":", manual_labels[[cvd]])),
        annotation_row(paste("Conditional Logistic Regression Results for specific CVD type -", manual_labels[[cvd]])),
        annotation_row(paste("Model", n, ":", manual_labels[[cvd]], "; adjusted for: Marital status, Highest education level, Overweight/Obesity diagnosis, 
        Rheumatoid arthritis diagnosis, hyperlipidaemia diagnosis, and Family history of lymphoma")),
        format_regression_table(yr_cvd_lymph_tidy) %>% as.data.frame()
    )
    n = n + 1
}

# Do the same regression by age group (<65 and >=65)


t = 7 + length(cvd_types)

for (age in c("<65", ">=65")) {

    cat("Running stratified model for age group ", age, "\n")

    age_filter <- if (age == "<65") {
        cvd_index_labelled$age_entry_dt < 65
    } else {
        cvd_index_labelled$age_entry_dt >= 65
    }

    # Run the model

    for (cvd in cvd_types) {

        cat("Running model for ", paste(cvd, ":", manual_labels[[cvd]]), "\n")

        # Run the model
        formula_str <- paste0("case ~ ", cvd, " + partner_cat + factor(educ_max) + overweight_obesity_ever + hyperlipidaemia_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever + strata(as.factor(riskset))")
        yr_cvd_lymph <- clogit(as.formula(formula_str), data = cvd_index_labelled[age_filter, ])

        # Get tidy tables for each model
        yr_cvd_lymph_tidy <- tidy_clogit(yr_cvd_lymph, paste("Family history of lymphoma adjusted for year of first CVD -", manual_labels[[cvd]]))

        # Combine all tables
        tbl_all <- dplyr::bind_rows(
            tbl_all,
            empty_row(),
            annotation_row(paste(cvd, ":", manual_labels[[cvd]], "and age group", age, " years old")),
            annotation_row(paste("Conditional Logistic Regression Results for specific CVD type -", manual_labels[[cvd]], "and age group", age, " years old")),
            annotation_row(paste("Model", t, ":", manual_labels[[cvd]], "; adjusted for: Marital status, Highest education level, Overweight/Obesity diagnosis, 
            Rheumatoid arthritis diagnosis, hyperlipidaemia diagnosis, and Family history of lymphoma")),
            format_regression_table(yr_cvd_lymph_tidy) %>% as.data.frame()
        )
        t = t + 1
    }

}





# Regression with the max number of CVDs (as continuous variable) -----------------#
table(cvd_index_labelled$Max_Enum_CVD_cat, exclude = NULL)
### With adjust on family history of lymphoma

max_cvd_lymph <- clogit(case ~ Max_Enum_CVD_cat + partner_cat + factor(educ_max) + overweight_obesity_ever + hyperlipidaemia_ever +
                        fam_hist_lymph_cat + rheumatoid_arthritis_ever + 
                        strata(as.factor(riskset)), data = cvd_index_labelled)

# Get tidy tables for each model
max_cvd_lymph_tidy <- tidy_clogit(max_cvd_lymph, "Family history of lymphoma adjusted for year of first CVD")

# Combine all tables
tbl_all <- dplyr::bind_rows(
    tbl_all,
    empty_row(),
    annotation_row("Conditional Logistic Regression Results for Maximum Number of CVDs vs No CVD"),
    annotation_row(paste("Model ", t+1, "for 'Maximum Number of CVDs vs No CVD' and adjusted for: Marital status, 
    Highest education level, Overweight/Obesity diagnosis, Rheumatoid arthritis diagnosis, hyperlipidaemia diagnosis, 
    and Family history of lymphoma")),
    format_regression_table(max_cvd_lymph_tidy) %>% as.data.frame()    
)
# View(tbl_all)











#--------------------- Save the two tables in an excel file  ------------------
Regression_tables <- list()
Regression_tables[["Cond_log_reg_mod"]] <- tbl_all 

# 3- Export the results to an excel file ------------------

for (sht in names(Regression_tables)) {
    if (!(sht %in% names(wb))) {
        openxlsx::addWorksheet(wb, sht)
    } else {
        print(paste("Sheet", sht, "already exists. Removing and rewriting it."))
        openxlsx::removeWorksheet(wb, sht)
        openxlsx::addWorksheet(wb, sht)
    }
    openxlsx::writeDataTable(wb, sht, x=Regression_tables[[sht]] %>% as.data.frame(), startCol = 1, startRow = 5)
}

openxlsx::saveWorkbook(wb, file = paste0(result_file, "/Results_", toupper(cohort), ".xlsx"), overwrite = TRUE)









#' Author: Antoine KDA
#' Date of creation: 25 May 2024
#' Last modification date: 07 September 2025

# Objectives: Regression Models -----------------------------------------------#
#' 
#' Stage risk according to CVD yes/no
#' 
#' 
#' 
#' ----------------------------------------------------------------------------#


# 1- Prepare the dataset ------------------------------------------------------


# class(Patient_trt_CVD$StageAA_cat)
# class(Patient_trt_CVD$Performance_cat)

# table(Patient_trt_CVD$StageAA, Patient_trt_CVD$StageAA_cat, exclude = NULL)
# table(Patient_trt_CVD$Performance, Patient_trt_CVD$Performance_cat, exclude = NULL)


# table(Patient_trt_CVD$StageAA_cat, Patient_trt_CVD$CVD_Status_cat, exclude = NULL)
# prop.table(table(Patient_trt_CVD$StageAA_cat, Patient_trt_CVD$CVD_Status_cat, exclude = NULL))


# apply(Patient_trt_CVD[, c("Sex", "CVD_Status", "age_entry_dt_bin", "Year_entry_dt_cat",
#                          "CVD_Status_cat", "Max_Enum_CVD")], 2, 
#       table, exclude = NULL)
# # View(Patient_trt_CVD[, c("lopnr", "riskset", "case", "Sex", "CVD_Status", "age_entry_dt_bin", "Year_entry_dt_cat", "CVD_Status_cat", "Max_Enum_CVD_cat")])

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

# 2- Unconditional logistic model -----------------------------------------------#

## 2.1- Adjusted for partner, education, overweight, and fam hist of lymp -----#
### With adjust on familly history of lymphoma
#### With Stage
table(Patient_trt_CVD$StageAA_cat, exclude = NULL)

glmStage <- glm(StageAA_cat ~ CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
                  partner_cat + educ_max + fam_hist_lymph_cat + overweight_obesity_ever + 
                  rheumatoid_arthritis_ever + hyperlipidaemia_ever,
                data = Patient_trt_CVD[Patient_trt_CVD$StageAA_cat != "Missing", ], 
                family = binomial(link = "logit"))


glmStage
summary(glmStage)


glmStage_tidy <- tidy_clogit(glmStage, "Stage")
format_regression_table(glmStage_tidy, 
              title = "DLBCL patients only analysis using a Logistic Regression", 
              subtitle = "Odds Ratios (95% CI) and p-values of advanced stage (III-IV) vs limited stage (I-II) (reference)")




#### With Performance

glmPerformance <- glm(Performance_cat ~ CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
                  partner_cat + educ_max + fam_hist_lymph_cat + overweight_obesity_ever + 
                  rheumatoid_arthritis_ever + hyperlipidaemia_ever,
                data = Patient_trt_CVD[Patient_trt_CVD$Performance_cat != "Missing", ], 
                family = binomial(link = "logit"))




glmPerformance
summary(glmPerformance)

glmPerformance_tidy <- tidy_clogit(glmPerformance, "Performance")
format_regression_table(glmPerformance_tidy, 
              title = "DLBCL patients only analysis using a Logistic Regression", 
              subtitle = "Odds Ratios (95% CI) and p-values of a poor performance (2-4) vs good performance (0-1) (reference)")


tbl_pat_only <- dplyr::bind_rows(
    annotation_row("DLBCL patients only Logistic Regression Results"),
    empty_row(),
    annotation_row("Odds Ratios (95% CI) and p-values"),
    empty_row(),
    annotation_row("The model compared advanced stage (III-IV) vs limited stage (I-II) (reference) at DLBCL diagnosis"),
    annotation_row("Model 1 for stage, adjusted for: Age at DLBCL diagnosis, Sex, Year at DLBCL diagnosis, 
    Marital status, Highest education level, Overweight/Obesity diagnosis, Rheumatoid arthritis diagnosis, 
    hyperlipidaemia diagnosis, and Family history of lymphoma"),
    empty_row(),
    format_regression_table(glmStage_tidy) %>% as.data.frame(),

    empty_row(),
    empty_row(),
    annotation_row("The model compared poor performance (2-4) vs good performance (0-1) (reference) at DLBCL diagnosis"),
    empty_row(),
    annotation_row("Model 2 for performance status, adjusted for: Age at DLBCL diagnosis, Sex, Year at DLBCL diagnosis, 
    Marital status, Highest education level, Overweight/Obesity diagnosis, Rheumatoid arthritis diagnosis, 
    hyperlipidaemia diagnosis, and Family history of lymphoma"),
    empty_row(),
    format_regression_table(glmPerformance_tidy) %>% as.data.frame()
)



# # 3- Forestplot and export ----------------------------------------------------#

# # Example with custom panels from the package forestplot

# panels <- list(
#   list(width = 0.03),
#   list(width = 0.1, display = ~variable, fontface = "bold", heading = "Variable"),
#   list(width = 0.1, display = ~level),
#   list(width = 0.05, display = ~n, hjust = 1, heading = "N"),
#   # list(width = 0.05, display = ~n_events, width = 0.05, hjust = 1, heading = "Events"),
#   # list(
#   #   width = 0.05,
#   #   display = ~ replace(sprintf("%0.1f", person_time / 365.25), is.na(person_time), ""),
#   #   heading = "Person-\nYears", hjust = 1
#   # ),
#   list(width = 0.03, item = "vline", hjust = 0.5),
#   list(
#     width = 0.55, item = "forest", hjust = 0.5, heading = "Odds ratio", linetype = "dashed",
#     line_x = 0
#   ),
#   list(width = 0.03, item = "vline", hjust = 0.5),
#   list(width = 0.12, display = ~ ifelse(reference, "Reference", sprintf(
#     "%0.2f (%0.2f, %0.2f)",
#     trans(estimate), trans(conf.low), trans(conf.high)
#   )), display_na = NA),
#   list(
#     width = 0.05,
#     display = ~ ifelse(reference, "", format.pval(p.value, digits = 1, eps = 0.001)),
#     display_na = NA, hjust = 1, heading = "p"
#   ),
#   list(width = 0.03)
# )



# (Stage_exp <- forestmodel::forest_model(glmStage, panels = panels))
# (Performance_exp <- forestmodel::forest_model(glmPerformance, panels = panels))


# # Export the plot -----------------------
# ggsave(Stage_exp, file = "./Results/FIg3_Unconditional_Stage_reg.png", 
#        units = "cm", width = 21, height = 22)

# ggsave(Performance_exp, file = "./Results/FIg4_Unconditional_Performance_reg.png", 
#        units = "cm", width = 21, height = 22)

# # Export the ORs
# rio::export(ResglmStage, file = "./Results/FIg3_Unconditional_Stage_reg_OR.xlsx")
# rio::export(ResglmPerformance, file = "./Results/FIg4_Unconditional_Performance_reg_OR.xlsx")









# --------------------- Update on 27 April 2025 ---------------------
#' Rerun the general model stratified by age
#


# # 5- Unconditional logistic model with interactions Stage Ann Arbor -------------------

# # # Reference model 
# # glmStage <- glm(StageAA_cat ~ CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
# #                   partner_cat + educ_max + fam_hist_lymph_cat + overweight_obesity_ever + 
# #                   rheumatoid_arthritis_ever + hyperlipidaemia_ever,
# #                 data = Patient_trt_CVD[Patient_trt_CVD$StageAA_cat != "Missing", ], 
# #                 family = binomial(link = "logit"))

# # Age 
# glmStage_age <- glm(StageAA_cat ~ age_entry_dt_bin:CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
#                   partner_cat + educ_max + overweight_obesity_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever + hyperlipidaemia_ever,
#                 data = Patient_trt_CVD, family = binomial(link = "logit"))


# # glmStage_age
# # summary(glmStage_age)
# cbind(exp(coef(glmStage_age)), exp(confint(glmStage_age)))

# # Sex
# glmStage_sex <- glm(StageAA_cat ~ Sex:CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
#                       partner_cat + educ_max + overweight_obesity_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever + hyperlipidaemia_ever,
#                     data = Patient_trt_CVD, family = binomial(link = "logit"))


# # glmStage_sex
# # summary(glmStage_sex)
# cbind(exp(coef(glmStage_sex)), exp(confint(glmStage_sex)))



# # Year_entry_dt_cat
# glmStage_Year_entry_dt_cat <- glm(StageAA_cat ~ Year_entry_dt_cat:CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
#                       partner_cat + educ_max + overweight_obesity_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever + hyperlipidaemia_ever,
#                     data = Patient_trt_CVD, family = binomial(link = "logit"))


# summary(glmStage_Year_entry_dt_cat)
# cbind(exp(coef(glmStage_Year_entry_dt_cat)), exp(confint(glmStage_Year_entry_dt_cat)))


# # partner_cat
# glmStage_partner_cat <- glm(StageAA_cat ~ partner_cat:CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
#                                     partner_cat + educ_max + overweight_obesity_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever + hyperlipidaemia_ever,
#                                   data = Patient_trt_CVD, family = binomial(link = "logit"))


# summary(glmStage_partner_cat)
# cbind(exp(coef(glmStage_partner_cat)), exp(confint(glmStage_partner_cat)))



# # educ_max
# glmStage_educ_max <- glm(StageAA_cat ~ educ_max:CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
#                               partner_cat + educ_max + overweight_obesity_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever + hyperlipidaemia_ever,
#                             data = Patient_trt_CVD, family = binomial(link = "logit"))


# summary(glmStage_educ_max)
# cbind(exp(coef(glmStage_educ_max)), exp(confint(glmStage_educ_max)))




# # overweight_obesity_ever
# glmStage_overweight_obesity_ever <- glm(StageAA_cat ~ overweight_obesity_ever:CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
#                            partner_cat + educ_max + overweight_obesity_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever + hyperlipidaemia_ever,
#                          data = Patient_trt_CVD, family = binomial(link = "logit"))


# summary(glmStage_overweight_obesity_ever)
# cbind(exp(coef(glmStage_overweight_obesity_ever)), exp(confint(glmStage_overweight_obesity_ever)))



# # fam_hist_lymph_cat
# glmStage_fam_hist_lymph_cat <- glm(StageAA_cat ~ fam_hist_lymph_cat:CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
#                           partner_cat + educ_max + overweight_obesity_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever + hyperlipidaemia_ever,
#                         data = Patient_trt_CVD, family = binomial(link = "logit"))


# summary(glmStage_fam_hist_lymph_cat)
# cbind(exp(coef(glmStage_fam_hist_lymph_cat)), exp(confint(glmStage_fam_hist_lymph_cat)))



# # rheumatoid_arthritis_ever
# glmStage_rheumatoid_arthritis_ever <- glm(StageAA_cat ~ rheumatoid_arthritis_ever:CVD_Status_cat + age_entry_dt_bin + Sex + Year_entry_dt_cat + 
#                                      partner_cat + educ_max + overweight_obesity_ever + fam_hist_lymph_cat + rheumatoid_arthritis_ever + hyperlipidaemia_ever,
#                                    data = Patient_trt_CVD, family = binomial(link = "logit"))


# summary(glmStage_rheumatoid_arthritis_ever)
# cbind(exp(coef(glmStage_rheumatoid_arthritis_ever)), exp(confint(glmStage_rheumatoid_arthritis_ever)))



# 6- Create a function to run automatically --------------



analyze_interactions <- function(data, outcome_var, interaction_vars, cvd_var = "CVD_Status_cat") {
  library(car)
  
  all_or_results <- list()
  lrt_results <- data.frame()
  wald_results <- data.frame()
  
  # Define base covariates (remove interaction_vars from this list if needed)
  base_covariates <- c("age_entry_dt_bin", "Sex", "Year_entry_dt_cat", 
                       "partner_cat", "educ_max", "fam_hist_lymph_cat", 
                       "overweight_obesity_ever", "rheumatoid_arthritis_ever", "hyperlipidaemia_ever")
  
  # Make formula string
  base_formula_str <- paste(outcome_var, "~", paste(base_covariates, collapse = " + "))
  
  for (var in interaction_vars) {
    message("Processing interaction with: ", var)
    
    # Build full and reduced model formulas
    interaction_term <- paste0(var, ":", cvd_var)
    full_formula_str <- paste(base_formula_str, "+", interaction_term)
    
    full_model <- glm(as.formula(full_formula_str), data = data, family = binomial(link = "logit"))
    summary(full_model)
    reduced_model <- glm(as.formula(base_formula_str), data = data, family = binomial(link = "logit"))
    
    # --- 1. Extract ORs and CIs ---
    result <- cbind(exp(coef(full_model)), exp(confint(full_model)))
    colnames(result) <- c("OR", "CI_lower", "CI_upper")
    
    interaction_rows <- grep(paste0(":", cvd_var), rownames(result), value = TRUE)
    interaction_data <- result[interaction_rows, , drop = FALSE]
    
    if (nrow(interaction_data) > 0) {
      interaction_df <- as.data.frame(interaction_data)
      interaction_df$Parameter <- rownames(interaction_df)
      interaction_df$Model <- var
      interaction_df$OR_95_CI <- sprintf("%.2f (%.2f-%.2f)", interaction_df$OR, interaction_df$CI_lower, interaction_df$CI_upper)
      rownames(interaction_df) <- NULL
      all_or_results[[var]] <- interaction_df
    }
    
    # --- 2. Likelihood Ratio Test ---
    lrt <- anova(reduced_model, full_model, test = "LRT")
    lrt_pvalue <- lrt$`Pr(>Chi)`[2]
    lrt_results <- rbind(lrt_results, data.frame(
      Interaction_Variable = var,
      LRT_pvalue = lrt_pvalue
    ))
    
    # --- 3. Wald Test for Interaction Level Comparison ---
    coefs <- names(coef(full_model))
    interaction_terms <- grep(":", coefs, value = TRUE)
    
    if (length(interaction_terms) >= 2) {
      for (i in 1:(length(interaction_terms) - 1)) {
        for (j in (i + 1):length(interaction_terms)) {
          term1 <- interaction_terms[i]
          term2 <- interaction_terms[j]
          hypothesis <- paste0(term1, " = ", term2)
          test <- tryCatch({
            lh <- linearHypothesis(full_model, hypothesis)
            pval <- lh[2, "Pr(>Chisq)"]
            data.frame(Model = var, Comparison = paste(term1, "vs", term2), P_value = pval)
          }, error = function(e) {
            data.frame(Model = var, Comparison = paste(term1, "vs", term2), P_value = NA)
          })
          wald_results <- rbind(wald_results, test)
        }
      }
    }
  }
  
  final_or_df <- do.call(rbind, all_or_results)
  final_or_df <- final_or_df[, c("Model", "Parameter", "OR", "CI_lower", "CI_upper", "OR_95_CI")]
  
  list(
    ORs_CIs = final_or_df,
    LRTs = lrt_results,
    Wald_Comparisons = wald_results
  )
}


# 7- Apply the function -------------------------
# Here I want to recode "overweight_obesity_ever", "rheumatoid_arthritis_ever", "hyperlipidaemia_ever" into factors Yes vs No
Patient_trt_CVD$overweight_obesity_ever <- factor(ifelse(Patient_trt_CVD$overweight_obesity_ever == 1, "Yes", "No"))
Patient_trt_CVD$rheumatoid_arthritis_ever <- factor(ifelse(Patient_trt_CVD$rheumatoid_arthritis_ever == 1, "Yes", "No"))
Patient_trt_CVD$hyperlipidaemia_ever <- factor(ifelse(Patient_trt_CVD$hyperlipidaemia_ever == 1, "Yes", "No"))


# Stage Ann Arbor at diagnosis
Stage_results <- analyze_interactions(data = Patient_trt_CVD, 
                                            outcome_var = "StageAA_cat", 
                                            interaction_vars = c("age_entry_dt_bin", "Sex", "Year_entry_dt_cat", 
                                                                 "partner_cat", "educ_max", "fam_hist_lymph_cat", 
                                                                 "overweight_obesity_ever", "rheumatoid_arthritis_ever", 
                                                                 "hyperlipidaemia_ever"), 
                                            cvd_var = "CVD_Status_cat")

Stage_results$ORs_CIs
Stage_results$LRTs
Stage_results$Wald_Comparisons
View(Stage_results$Wald_Comparisons)

interactions_ORs <- Stage_results$ORs_CIs %>% 
  mutate(Subgroup = gsub(paste0(paste0(unique(Stage_results$ORs_CIs$Model), collapse = "|"), "|", ":CVD_Status_catYes"), "", 
                         Stage_results$ORs_CIs$Parameter),
         Group = case_when(Model == "age_entry_dt_bin" ~ "Age", 
                           Model == "Sex" ~ "Sex",
                           Model == "Year_entry_dt_cat" ~ "Year",
                           Model == "partner_cat" ~ "Marital status",
                           Model == "educ_max" ~ "Highest education level",
                           Model == "fam_hist_lymph_cat" ~ "Family history of lymphoma",
                           Model == "overweight_obesity_ever" ~ "Overweight/Obesity diagnosis",
                           Model == "rheumatoid_arthritis_ever" ~ "Rheumatoid Arthritis diagnosis",
                           Model == "hyperlipidaemia_ever" ~ "Hyperlipidaemia diagnosis")) %>% 
  select(contains(c("OR", "CI_", "group"))) %>% 
  filter(!Subgroup %in% grep("Missing", Subgroup, value = TRUE)) %>% 
  group_by(Group) %>% 
  arrange( )%>% 
  ungroup() %>% 
  mutate(Subgroup = gsub("^A|B|C|D", "", Subgroup, ignore.case = FALSE),
         Subgroup = paste0("   ", Subgroup),
         Archy = "Stage Ann Arbor") %>%
  group_by(Group) %>%
  mutate(Group = ifelse(row_number() > 1, NA, Group)) %>%
  ungroup()


View(interactions_ORs)

interactions_ORs$Archy


# Performance at diagnosis
Performance_results <- analyze_interactions(data = Patient_trt_CVD, 
                                            outcome_var = "Performance_cat", 
                                            interaction_vars = c("age_entry_dt_bin", "Sex", "Year_entry_dt_cat", 
                                                                 "partner_cat", "educ_max", "fam_hist_lymph_cat", 
                                                                 "overweight_obesity_ever", "rheumatoid_arthritis_ever", 
                                                                 "hyperlipidaemia_ever"), 
                                            cvd_var = "CVD_Status_cat")

Performance_results$ORs_CIs
Performance_results$LRTs
Performance_results$Wald_Comparisons
View(Performance_results$Wald_Comparisons)


Performance_results$ORs_CIs %>% 
  mutate(Subgroup = gsub(paste0(paste0(unique(Performance_results$ORs_CIs$Model), collapse = "|"), "|", ":CVD_Status_catYes"), "", 
                         Performance_results$ORs_CIs$Parameter),
         Group = case_when(Model == "age_entry_dt_bin" ~ "Age", 
                           Model == "Sex" ~ "Sex",
                           Model == "Year_entry_dt_cat" ~ "Year",
                           Model == "partner_cat" ~ "Marital status",
                           Model == "educ_max" ~ "Highest education level",
                           Model == "fam_hist_lymph_cat" ~ "Family history of lymphoma",
                           Model == "overweight_obesity_ever" ~ "Overweight/Obesity diagnosis",
                           Model == "rheumatoid_arthritis_ever" ~ "Rheumatoid Arthritis diagnosis",
                           Model == "hyperlipidaemia_ever" ~ "Hyperlipidaemia diagnosis")) %>% 
  select(contains(c("OR", "CI_", "group"))) %>% 
  filter(!Subgroup %in% grep("Missing", Subgroup, value = TRUE)) %>% 
  group_by(Group) %>% 
  arrange( )%>% 
  ungroup() %>% 
  mutate(Subgroup = gsub("^A|B|C|D", "", Subgroup, ignore.case = FALSE), 
         Subgroup = paste0("   ", Subgroup),
         Archy = "Performance status") %>%
  group_by(Group) %>%
  mutate(Group = ifelse(row_number() > 1, NA, Group)) %>%
  ungroup() %>% 
  bind_rows(interactions_ORs) -> interactions_ORs


View(interactions_ORs)



# 8- Plot a forestplot with a double panel -------------------

# Plot for stage 
str(interactions_ORs)

Stage <- forestplot(x = interactions_ORs[interactions_ORs$Archy == "Stage Ann Arbor", ], 
                    mean = OR,
                    lower = CI_lower,
                    upper = CI_upper,
                    labeltext = c(Group, Subgroup, OR_95_CI),
                    clip = c(0, 2.5),
                    zero = 1,
                    boxsize = 0.4,
                    ci.vertices.height = 0.15,
                    lineheight = unit(10, "mm"),
                    col = fpColors(box = "royalblue", lines = "darkblue", zero = "black"),
                    xlab = "Odds Ratio (95% CI)" , #align = "c"
                    graph.pos = 3,
                    xlog = FALSE,  # Linear scale
                    xticks = c(1, seq(0.6, 1.5, by = .2)),  # X-axis range from -2 to 22
                    ci.vertices = TRUE, 
                    hrzl_lines = list("2" = gpar(lty = 1, lwd = 3, alpha = 0.2, col = "darkblue")),
                    lwd.xaxis = 2, 
                    lwd.zero = 2, 
                    lwd.ci = 2, 
                    title = "Stage Ann Arbor",
                    txt_gp = fpTxtGp(label = list(gpar(fontface = "bold"), gpar(), gpar()),
                                     xlab = gpar(fontsize = 25), 
                                     ticks = gpar(fontsize = 25)),
                    graphwidth = unit(10, "cm"),
                    txt_align = c("left", "center")#,
                    # colgap = unit(0.1, "mm"), # Custom layout
                    # mar = unit(rep(0.1, 4), "mm")
) |>
  fp_set_zebra_style("#FFFFFF", "#EFEFEF")|>
  fp_set_style(box = c("coral4") |> 
                 lapply(function(x) gpar(fill = x, col = x)), 
               line = c("coral3") |> 
                 lapply(function(x) gpar(lwd = 2, col = x))) |>
  fp_add_header(OR_95_CI = "OR (95% CI)\n") |>
  fp_decorate_graph(grid = structure(c(seq(0.6, 1.5, by = .2))))

#windows()

# Set graphics device for VS Code
if (Sys.getenv("TERM_PROGRAM") == "vscode") {
    options(device = function(...) {
        grDevices::png(...)
        dev.control("enable")
    })
}

windows()
Stage
# Save the plot
png(filename = paste0(result_file, "/tempStage_", toupper(cohort), ".png"),
    width = 26.3, height = 25, units = "cm", res= 500)

Stage
dev.off()



# Plot for Performance


pfmc <- forestplot(x = interactions_ORs[interactions_ORs$Archy == "Performance status", ], 
                  mean = OR,
                  lower = CI_lower,
                  upper = CI_upper,
                  labeltext = c(OR_95_CI),
                  clip = c(0, 2.7),
                  zero = 1,
                  boxsize = 0.4,
                  ci.vertices.height = 0.15,
                  lineheight = unit(10, "mm"),
                  col = fpColors(box = "royalblue", lines = "darkblue", zero = "black"),
                  xlab = "Odds Ratio (95% CI)" , #align = "c"
                  graph.pos = 1,
                  xlog = FALSE,  # Linear scale
                  xticks = c(1, seq(0.8, 1.8, by = .2)),  # X-axis range from -2 to 22
                  ci.vertices = TRUE, 
                  hrzl_lines = list("2" = gpar(lty = 1, lwd = 3, alpha = 0.2, col = "darkblue")),
                  lwd.xaxis = 2, 
                  lwd.zero = 2, 
                  lwd.ci = 2, 
                  title = "Performance status",
                  txt_gp = fpTxtGp(xlab = gpar(fontsize = 25, fontface = "bold"), 
                                   ticks = gpar(fontsize = 25, fontface = "bold")),
                  graphwidth = unit(10, "cm")#,
                  # colgap = unit(0.1, "mm"), # Custom layout
                  # mar = unit(rep(0.1, 4), "mm")
) |>
  fp_set_zebra_style("#FFFFFF", "#EFEFEF")|>
  fp_set_style(box = c("brown") |> 
                 lapply(function(x) gpar(fill = x, col = x)), 
               line = c("brown3") |> 
                 lapply(function(x) gpar(lwd = 2, col = x))) |>
  fp_add_header(OR_95_CI = "OR (95% CI)\n") |>
  fp_decorate_graph(grid = structure(c(seq(0.8, 1.8, by = .2))))


windows()
pfmc
# Save the plot
png(filename = paste0(result_file, "/tempPerformance_", toupper(cohort), ".png"),
    width = 15.3, height = 25, units = "cm", res= 500)
pfmc
dev.off()



# Concatenate and Save the figure --------------


# Load the two PNG images you want to concatenate
Stage <- image_read(paste0(result_file, "/tempStage_", toupper(cohort), ".png"))
pfmc <- image_read(paste0(result_file, "/tempPerformance_", toupper(cohort), ".png"))

# Concatenate the images horizontally 
concatenated_image <- image_append(c(Stage, pfmc), stack = F)

# Save the concatenated image to a new PNG file
image_write(concatenated_image, path = paste0(result_file, "/Interactions_CVD_In_Patients_Only_Stage_Performance_", toupper(cohort), ".png"))

dev.off()

# rm(Stage, pfmc)
# remove the temporary files
# file.remove(c("./Results/tempPerformance.png"))


#--- Function to describe all categorical variables in a dataset --------------

desc.factor <- function(data, # the dataframe to describe
                        var_to_desc, digits=2, collapse=FALSE){ # a vector of variable to describe
  
  descript.factor <- NULL
  
  for(i in var_to_desc){
    new = as.factor(as.character(data[, i]))
    t = table(data[, i], exclude = NULL)
    new.desc = cbind(Variable = c(i, rep("", length(names(t))-1)),
                     Category = names(t),
                     Frequency = t,
                     Proportion = sprintf(paste0("%.", digits, "f"), prop.table(t)*100))
    descript.factor = data.frame(rbind(descript.factor, "", new.desc))
    descript.factor
    
  }
  
  if(collapse==TRUE) {
    descript.factor[ , "N (Col %)"] = paste0(descript.factor$Frequency, " (", 
                                             descript.factor$Proportion, ")")
    descript.factor = descript.factor[, c("Variable", "Category", "N (Col %)")]
    descript.factor
  }
  
  descript.factor
}







#--- Function to describe all categorical variables in a dataset STRATIFIED ON ANOTHER cathegorical variable --------------
# Thee function works similar to the previous one except this time, it
# adds one stratification variable
#


desc.factor.2levels <- function(data, # the dataframe to describe
                                var_to_desc, digits=2, collapse=FALSE, 
                                desc_by = NULL # stratification variable of 1 character vector
){ # a vector of variable to describe
  
  descript.factor.final <- NULL
  
  
  # No stratification variable
  for(i in var_to_desc){
    
    descript.factor <- NULL
    
    # With 1 stratification variable
    
    if(length(desc_by)){
      
      # Table
      descript.factor.N <- NULL
      # i="Age_cat"; desc_by="StageAA_cat"; data = nlph_trt; digits=1
      
      t = table(data[, i], data[, desc_by], exclude = NULL)
      
      new.desc = cbind(Variable = c(i, rep("", length( row.names(as.data.frame.array(t)))-1)),
                       Category = row.names(as.data.frame.array(t)),
                       Frequency = t)
      
      descript.factor.N = data.frame(rbind(descript.factor.N, new.desc))
      Strata = names(as.data.frame.array(t))
      names(descript.factor.N) = c("Variable", "Category", paste0(desc_by, Strata))
      descript.factor.N
      
      # Proportion
      
      descript.factor.P <- NULL
      p = prop.table(t, 2)*100 # 2 for col %
      
      new.desc = cbind(Variable = c(i, rep("", length( row.names(as.data.frame.array(p)))-1)),
                       Category = row.names(as.data.frame.array(p)),
                       Frequency = p)
      
      descript.factor.P = data.frame(rbind(descript.factor.P, new.desc))
      Strata = names(as.data.frame.array(p))
      names(descript.factor.P) = c("Variable", "Category", paste0(desc_by, Strata))
      descript.factor.P 
      
      
      if(collapse==TRUE) {
        
        for(s in 1:length(Strata)){
          descript.factor.N[paste0(desc_by, Strata[s])] = paste0(descript.factor.N[[paste0(desc_by, Strata[s])]] , 
                                                                 " (",
                                                                 sprintf(paste0("%.", digits, "f"), (as.numeric(as.character(descript.factor.P[[paste0(desc_by, Strata[s])]])))) ,
                                                                 ")")
        }
        
        names(descript.factor.N) = c("Variable", "Category", paste0(desc_by, "_", Strata, "_N_(ColPercent)"))
        
      }
      
      
      descript.factor =  data.frame(rbind(descript.factor, "", descript.factor.N))
      rm(descript.factor.N, descript.factor.P, Strata)
      descript.factor
      
      
      
    }else{
      
      
      t = table(data[, i], exclude = NULL)
      new.desc = cbind(Variable = c(i, rep("", length(names(t))-1)),
                       Category = names(t),
                       Frequency = t,
                       Proportion = sprintf(paste0("%.", digits, "f"), prop.table(t)*100))
      descript.factor = data.frame(rbind(descript.factor, "", new.desc))
      
      if(collapse==TRUE) {
        descript.factor[ , "N (Col %)"] = paste0(descript.factor$Frequency, " (", 
                                                 descript.factor$Proportion, ")")
        descript.factor = descript.factor[, c("Variable", "Category", "N (Col %)")]
        descript.factor
      }
      
      descript.factor
      
    }
    descript.factor.final <- rbind(descript.factor.final, descript.factor)
    
  }
  
  
  # Names
  
  
  descript.factor.final
}














#--- Function to describe all date variables in a dataset ---------------------


desc.date <- function(data){ # the dataframe to describe
  
  descript.date <- NULL
  for (i in names(data)) {
    if(class(data[, i])=="Date"){
      min = paste(min(data[, i], na.rm = T))
      max = paste(max(data[, i], na.rm = T))
      median = paste(median(data[, i], na.rm = T))
      na = length(which(is.na(data[, i])))
      available = length(which(!is.na(data[, i])))
      new.date = cbind(Variable = i,
                       Minimum = min,
                       Maximum = max,
                       Median = median,
                       Not_Available = na,
                       Available = available)
      descript.date = data.frame(rbind(descript.date, new.date))
      descript.date
    }
    
  }
  descript.date
}







#create a function to check for installed packages and install them if they are not installed
install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}







# Function to compute the default wald-based 95% CI of the model ---------
CI_default <- function(model, digits=4){
  CI_default <- cbind(row.names(data.frame(model$coefficients)), 
                      round(exp(model$coefficients), digits), 
                      round(exp(confint.default(model)), digits))
  CI_default <- as.data.frame(CI_default)
  names(CI_default) <- c("Variables", "Estimate", "LL", "UL")
  CI_default$EST_CI <- paste0(sprintf(paste0("%.", digits, "f"), as.numeric(as.character(CI_default$Estimate))), " (",
                              sprintf(paste0("%.", digits, "f"), as.numeric(as.character(CI_default$LL))), " - ",
                              sprintf(paste0("%.", digits, "f"), as.numeric(as.character(CI_default$UL))), ")")
  
  
  CI_default
}



# Median age/FU at diagnosis + min and max

#' this function is originally designed for computing median age at diagnosis with 
#' min and max range. 
#' However it could be used for any continuous variable to compute median (min - max) + range

medrang <- function(x, # A character vector of lenght 1L
                    data = NULL, # The dataset to be used. Should be a dataframe or matrix object
                    digits = 1 # Number of decimal to display. 
                    ){
  
  Median = sprintf(paste0("%.", digits, "f"), median(data[,x]))
  Min = sprintf(paste0("%.", digits, "f"), min(data[,x]))
  Max = sprintf(paste0("%.", digits, "f"), max(data[,x]))
  Median_Min_Max = paste0(Median, " (", Min, "-", Max, ")")
  
  res = data.frame(cbind(Variable = paste0(x, ", Median (", "Min", "-", "Max", ")"),
                         # Median, Min, Max, 
                         Median_Min_Max))
  res
}







## Function for a chisquare test
pval.chisq <- function(X, Y, data, ...){
  stat = chisq.test(data[, X], data[, Y])
  pval = stat$p.value
  pval = data.frame(cbind(Variable = X, Factor = Y, chisq_test_P_value = round(pval, 5)))
  pval
}

## Function for a fisher exact test
pval.fisher <- function(X, Y, data, ...){
  stat = fisher.test(data[, X], data[, Y], ...)
  pval = stat$p.value
  pval = data.frame(cbind(Variable = X, Factor = Y, fisher_test_P_value = round(pval, 5)))
  pval
}



# Create a function for cross-tabulation 
cross_tabulation <- function(data, col1) {
  cbind(table(data[[col1]], exclude = NULL),
  round(prop.table(table(data[[col1]], exclude = NULL))*100, 1)) %>%
  as.data.frame() %>%
  setNames(c("Count", "Proportion")) %>%
  arrange(desc(Count)) -> desc
  return(desc)
}




##################################### Create a summary table #############################################

#' Create a Summary Table with Optional Confidence Intervals and SMDs:
#'
#' This function generates a summary table for a given dataset, supporting both continuous and categorical variables.
#' It allows for stratification by a specified variable, and can optionally add standardized mean differences (SMDs)
#' and confidence intervals (CIs) to the summary statistics. If confidence intervals cannot be calculated for some
#' variables, the function will retry those variables without CIs.
#'
#' data: A data frame containing the variables to be summarized.
#' strata_var: (Optional) Character string specifying the name of the variable to stratify by. If \code{NULL}, no stratification is performed.
#' labels: A named list of variable labels to be used in the summary table. Defaults to \code{labels_all}.
#' add_smd: Logical; if \code{TRUE}, standardized mean differences are added to the summary table (only if the strata variable has 2 or fewer levels).
#' add_CI: Logical; if \code{TRUE}, confidence intervals are added to the summary statistics. If calculation fails for a variable, it is retried without CIs.



create_summary_table <- function(data, strata_var = NULL, labels = labels_all, add_smd = TRUE, add_CI = TRUE, force_continuous = "serum_albumin_value", .env = .GlobalEnv) {
  
  require(gtsummary)
  require(dplyr)
  require(magrittr)
  require(purrr)
  require(tidyr)
  require(labelled)
  

  # 1- Create the first parent function 

    create_table <- function(data, strata_var=NULL, labels=labels_all, add_smd=TRUE, add_CI=TRUE) {
    
    #categorical can only have 1 line
    #one strata at a time - just subset if need to nest
    #if strata_var=NULL then do overall
    #if strata_var is >2 categories, add_smd will be changed to FALSE
    
    # we want a table with:
    # continuous: N, Missing, Mean, Median, 5th-95th percentile, Min-Max, 95% CI of mean, 
    # across strata, SMD of mean
    # categorical: N, Missing, % per cat out of overall N (), 95% CI of %
    # across strata, SMD of % per cat
    # binary (most): N, %, 95% CI of %
    # across strata, SMD of %
    
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
                    all_dichotomous() ~ 'dichotomous', # Add all force_continuous variables here if needed
                     all_of(intersect(force_continuous, names(data))) ~ "continuous2"
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
            #bizarrely- putting the names (n, p, etc) in these lists doesn't matter
            #  its just dependent on the order!
            all_dichotomous() ~ c(n=0, p=1),
            all_categorical() ~ c(n=0, p=1),
            all_continuous() ~ c(N_nonmiss=0, mean=2, sd=2, median=1, p5=1, p95=1, 
                                p25=1, p75=1, min=1, max=1)
        )
        ) %>%
        add_stat_label() #%>%
        # add_n(style_fun = list(everything() ~ purrr::partial(style_sigfig, digits=3)))  %>%
        # modify_header(update = list(add_n_n ~ "**N**")) %>%
    if (add_CI==TRUE) {
        summary_table <- summary_table %>%
            add_ci(pattern = NULL, 
            method=list(all_categorical() ~ "wilson",
                        all_dichotomous() ~ "wilson",
                        all_continuous() ~ 't.test'),
            style_fun = list(all_continuous() ~ purrr::partial(style_number, digits=2),
                                all_categorical() ~ purrr::partial(style_number, digits=2, scale=100),
                                all_dichotomous() ~ purrr::partial(style_number, digits=2, scale=100))) #%>%
    # modify_header(update = ci_95 ~ "**95% CI**") #%>%
    # modify_fmt_fun(
    #   update = ci_95 ~ function(x) style_number(x, digits = 1)
    # )
    
    }
    
    if (add_smd==TRUE) {
        summary_table <- summary_table %>%
        
        add_difference(test = everything() ~ "smd") 
        summary_table
    }
    
    return(summary_table)
    }

  # 2- Use the parent function to create a summary table for different scenarios
  
  if (add_CI == FALSE) {
    create_table(data = data, strata_var = strata_var, labels = labels, add_smd = add_smd, add_CI = FALSE)
  } else {
    create_tables_mult <- function(data, strata_var = NULL, labels, add_smd, .env = .GlobalEnv) {
      list_var <- if(!is.null(strata_var)){names(data)[names(data) != paste0(strata_var)]}else{names(data)}
      s1_table_l <- list()
      lapply(list_var, function(x) {
        res <- tryCatch(
          create_table(data = data %>% dplyr::select(all_of(c(x, strata_var))),
                       strata_var = strata_var,
                       labels = labels,
                       add_smd = add_smd,
                       add_CI = TRUE),
          error = function(e) {
            message("\n add_CI failed for ", x, " with error: ", e$message, ". Retrying with add_CI = FALSE. \n")
            create_table(data = data %>% dplyr::select(all_of(c(x, strata_var))),
                         strata_var = strata_var,
                         labels = labels,
                         add_smd = add_smd,
                         add_CI = FALSE)
          }
        )
        s1_table_l[[x]] <<- res
      })
      out_tbl <- gtsummary::tbl_stack(s1_table_l)
      return(out_tbl)
    }
    # CALL the function and return its result!
    create_tables_mult(data = data, strata_var = strata_var, labels = labels, add_smd = add_smd)
  }
}

#




# # 1- Create a function to describe the study population ------------------


# Define a function to create summary tables that adds CIs and SMDs and Pvalue at convenience + overall column



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


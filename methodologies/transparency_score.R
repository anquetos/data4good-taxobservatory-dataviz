#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                                                                                      ~
#                                      CALCULATE TRANSPARENCY SCORE                                    ~
#                                                                                                      ~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## PURPOSE       Calculate a transparency score for each multinational and multinational-year

## AUTHOR        Kane Borders

## DATE CREATED  April 17, 2024

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data and packages -----------

library(tidyverse)
library(magrittr)

path <- "C:/Users/k.borders/Desktop/Transparency Score/data/"

df <- read_csv(paste0(path, "data_final_all.csv"))

num_col_names <- c("total_revenues", "profit_before_tax", "tax_paid", "tax_accrued", 
                   "unrelated_revenues", "related_revenues", "stated_capital", "accumulated_earnings", 
                   "tangible_assets", "employees") # these are the financial data columns

df %<>% select(mnc, year, upe_code, jur_code, jur_name, all_of(num_col_names)) # data needed




# Component I: Geographic ----------

# Here are the steps to calculate this component:
# i. Remove all financial variables for which data are missing for all jur_code.
# ii. Convert all negative values to positive values.
# iii. Calculate the percentage of each remaining variable that is reported in a non-jurisdiction, i.e., in a line where jur_code == OTHER.
# iv. Calculate the average of this percentage.
# v. Reverse the scale so that 100 is the best score and 0 is the worst.

calculate_geographic_score <- function(df, num_col_names) {

  all_na_columns <- sapply(df, function(x) all(is.na(x))) # i.
  df <- df[, !all_na_columns] # i.
  num_col_names_present <- num_col_names[num_col_names %in% names(df)] # i.
  
  if (length(num_col_names_present) == 0) {geographic_score <- 0} else { # can only calculate if some financial data -> if none -> score = 0
    
    df %<>% mutate(across(all_of(num_col_names_present), abs)) # ii.
    
    tot <- sapply(df[, num_col_names_present], function(x) sum(x, na.rm = TRUE)) # iii.
    df_other <- filter(df, jur_code == "OTHER") # iii.
    other_tot <- sapply(df_other[, num_col_names_present], function(x) sum(x, na.rm = TRUE)) # iii.
    per_other <- other_tot / tot # iii.
    
    mean_per_other <- mean(per_other, na.rm = TRUE) * 100 # iv. = Average % of financial variables in non-jurisdictions
    
    geographic_score <- 100 - mean_per_other # v.
    
  }
  
  return(geographic_score)
  
}

geographic_scores_df <- df %>% group_by(mnc, year) %>% nest() %>%
  mutate(geographic_score = map_dbl(data, ~calculate_geographic_score(.x, num_col_names))) %>%
  select(mnc, year, geographic_score) %>%
  ungroup()

summary(geographic_scores_df$geographic_score) # all values should be between 0 and 100





# Component II: Financial Variables ----------

# Here are the steps to calculate this component:
# i. Remove all financial variables for which data are missing for all jur_code.
# ii. Apply the weighting to columns with data (+1 for profits and paid taxes).
# iii. Calculate the score.

calculate_financial_variable_score <- function(df, num_col_names) {
  
  all_na_columns <- sapply(df, function(x) all(is.na(x))) # i.
  df <- df[, !all_na_columns] # i.
  num_col_names_present <- num_col_names[num_col_names %in% names(df)] # i.
  
  if (length(num_col_names_present) == 0) {financial_variable_score <- 0} else { # can only calculate if some financial data -> if none -> score = 0
    
    score <- length(num_col_names_present) # ii.
    if ("profit_before_tax" %in% num_col_names_present) {score <- score + 1} # ii.
    if ("tax_paid" %in% num_col_names_present) {score <- score + 1} # ii.
    
    financial_variable_score <- score/12*100 # iii.
    
  }
  
  return(financial_variable_score)
  
}

financial_variable_scores_df <- df %>% group_by(mnc, year) %>% nest() %>%
  mutate(financial_variable_score = map_dbl(data, ~calculate_financial_variable_score(.x, num_col_names))) %>%
  select(mnc, year, financial_variable_score) %>%
  ungroup()

summary(financial_variable_scores_df$financial_variable_score) # all values should be between 0 and 100




# Final score --------

final_df <- geographic_scores_df %>% left_join(financial_variable_scores_df, by = c("mnc", "year"))

final_df %<>% mutate(final_score = (geographic_score + financial_variable_score)/2)

final_df %>% filter(is.na(final_score)) # should be empty

write_csv(final_df, paste0(path, "transparency_score.csv"))













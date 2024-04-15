



# Data

library(tidyverse)

path <- "C:/Users/kaneb/Desktop/data/"

df <- read_csv(paste0(path, "data_final_all.csv"))

df <- df %>% filter(mnc == "ANGLO AMERICAN" & year == 2020)

df <- df %>% select(upe_code, jur_code, jur_name, jur_tax_haven, unrelated_revenues, related_revenues, total_revenues)

# Data is ANGLO AMERICAN for 2020





# Dealing with total revenues ----------

# We do not use total_revenues -> we will recreate it using related and unrelated
# This is because the total_revenues reported by companies is not always reliable
# This should be true related revenues + unrelated revenues = total revenues

check <- df %>% filter(!is.na(unrelated_revenues) & !is.na(related_revenues) & !is.na(total_revenues))

check$unrelated_revenues + check$related_revenues == check$total_revenues # not always true

df$total_revenues <- NULL # drop total revenues





# By tax haven/non-tax haven/domestic

# i. Calculate unrelated/related share in domestic if we have both unrelated and related revenues
# ii. Calculate unrelated/related share for tax havens
# iii. Calculate unrelated/related share for tax havens



# i. Calculate total employees in tax haven jurisdictions (including jurisdictions with NA)
# ii. Calculate total employees in non-tax haven jurisdictions (including jurisdictions with NA)
# iii. Calculate total employees in domestic

# i. domestic

dom <- df %>% filter(upe_code == jur_code) 

dom <- dom %>% filter(!is.na(unrelated_revenues) & !is.na(related_revenues))

if (nrow(dom) > 0) { # we only calculate if we have both related and unrelated
  
  dom <- dom %>% mutate(total_revenues = unrelated_revenues + related_revenues) %>%
    mutate(unrelated_share = unrelated_revenues/total_revenues) %>%
    mutate(related_share = related_revenues/total_revenues) %>%
    select(jur_name, unrelated_share, related_share)
  
} else {
  
  message("Do not have unrelated and related for domestic.")
  
}

dom # result



# ii. 

# Now that we have a group of countries, we can ignore NAs and just sum

unrel <- df %>% filter(jur_tax_haven == TRUE) %>% summarise(unrelated_revenues = sum(unrelated_revenues, na.rm = TRUE))
rel <- df %>% filter(jur_tax_haven == TRUE) %>% summarise(related_revenues = sum(related_revenues, na.rm = TRUE))

tot <- unrel + rel

unrel_share <- unrel/tot
rel_share <- rel/tot

unrel_share # unrelated share for tax havens
rel_share # related share for tax havens






# iii. 

# Same thing as for ii. by for non-tax havens:

unrel <- df %>% filter(jur_tax_haven == FALSE) %>% summarise(unrelated_revenues = sum(unrelated_revenues, na.rm = TRUE))
rel <- df %>% filter(jur_tax_haven == FALSE) %>% summarise(related_revenues = sum(related_revenues, na.rm = TRUE))

tot <- unrel + rel

unrel_share <- unrel/tot
rel_share <- rel/tot

unrel_share # unrelated share for tax havens
rel_share # related share for tax havens







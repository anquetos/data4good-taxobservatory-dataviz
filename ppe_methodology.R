



# Data

library(tidyverse)

path <- "C:/Users/k.borders/Dropbox/EUTO/03. Research/11_MNC_CbCR/02. Multinationals CbCR/database/output/final/"

df <- read_csv(paste0(path, "data_final_all.csv"))

df <- df %>% filter(mnc == "ANGLO AMERICAN" & year == 2020)

df <- df %>% select(jur_code, jur_name, jur_tax_haven, employees, profit_before_tax)

# Data is ANGLO AMERICAN for 2020




# By tax haven/non-tax haven groups ---------------

# % of profits
# i. Remove jur with losses
# ii. Calculate total profits in tax haven jurisdictions (including jurisdictions with NA)
# iii. Calculate total profits in non-tax haven jurisdictions (including jurisdictions with NA)
# iv. % of profits in tax havens = ii./iii.*100

df_p <- df %>% filter(profit_before_tax >= 0) # i.

p1 <- df_p %>% filter(jur_tax_haven == TRUE) %>% summarise(x = sum(profit_before_tax, na.rm = TRUE)) # ii.
p2 <- df_p %>% filter(jur_tax_haven == FALSE) %>% summarise(x = sum(profit_before_tax, na.rm = TRUE)) # iii.

p1/p2*100 # iv.



# % of employees
# i. Calculate total employees in tax haven jurisdictions (including jurisdictions with NA)
# ii. Calculate total employees in non-tax haven jurisdictions (including jurisdictions with NA)
# iii. % of employees in tax havens = ii./iii.*100

e1 <- df %>% filter(jur_tax_haven == TRUE) %>% summarise(x = sum(employees, na.rm = TRUE)) # i.
e2 <- df %>% filter(jur_tax_haven == FALSE) %>% summarise(x = sum(employees, na.rm = TRUE)) # ii.

e1/e2*100 # iii.




# profit per employee

# i. total profits/total employees for tax haven group (as before, including jurisdictions with NA)
# ii. total profits/total employees for non-tax haven group (as before, including jurisdictions with NA)

p1/e1 # i.
p2/e2 # ii.







# By jurisdiction ---------------

# We only focus on PROFITABLE jurisdictions if we will put profits and employees together on a graph

# % of profits
# i. Remove jur with losses
# ii. Calculate % profits of total

df <- df %>% filter(profit_before_tax >= 0) # i.

p1 <- df %>% mutate(total_profits = sum(profit_before_tax, na.rm = TRUE)) %>% mutate(per_profits = profit_before_tax/total_profits*100) # ii.



# % of employees
# i. Remove jur with losses
# ii. Calculate % employees of total

df <- df %>% filter(profit_before_tax >= 0) # i.

e1 <- df %>% mutate(total_employees = sum(employees, na.rm = TRUE)) %>% mutate(per_employees = employees/total_employees*100) # ii.





# profit per employee
# i. Remove jur with losses (already done above)
# ii. Remove jur that have NA for either employees or profit_before_tax
# iii. Replace 0 employees by 1
# iv. Calculate ppe

ppe <- p1 %>% left_join(e1) %>% select(jur_name, employees, profit_before_tax, matches("^per"))

ppe <- ppe %>% filter(!is.na(profit_before_tax) & !is.na(employees)) # ii.

ppe[ppe$employees == 0, ]$employees <- 1 # iii.

ppe <- ppe %>% mutate(ppe = profit_before_tax/employees)








# graph with all three indicators

ppe <- ppe %>% select(jur_name, ppe) # ppe

p1 <- p1 %>% select(jur_name, per_profits) # % profits 

e1 <- e1 %>% select(jur_name, per_employees) # % employees

all <- ppe %>% full_join(p1) %>% full_join(e1) # notice DRC only has % profits b/c % employees = NA


























# Packages used

library(tidyverse)
library(lubridate)
library(broom)
library(pROC)

# Read the data

loan_credit <- read_rds("data/Loan_Cred/loan_data.rds")


# Clean the data to the necessary data only

loan_credit <- loan_credit %>%
    mutate(
        loan_status_binary = case_when(
            loan_status == "Fully Paid" ~ 0,
            loan_status %in% c("Charged Off", "Default") ~ 1,
            TRUE ~ NA_real_
        )
    )


df <- loan_credit %>%
    filter(!is.na(loan_status_binary))
# Current loans are excluded because their final default outcome is not yet known.

df <- df %>%
    mutate(
        int_rate = as.numeric(gsub("%", "", int_rate)),
        revol_util = as.numeric(gsub("%", "", revol_util)),
        term = trimws(term),
        short_term = ifelse(term == "36 months", 1, 0),
        emp_10plus = ifelse(emp_length == "10+ years", 1, 0),
        homeowner = ifelse(home_ownership %in% c("MORTGAGE", "OWN"), 1, 0)
    )

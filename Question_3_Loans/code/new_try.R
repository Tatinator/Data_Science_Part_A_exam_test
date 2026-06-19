library(tidyverse)
library(lubridate)
library(broom)
install.packages("pROC")
library(pROC)

loan_credit <- read_rds("data/Loan_Cred/loan_data.rds")

glimpse(loan_credit)
names(loan_credit)
table(loan_credit$loan_status, useNA = "always")


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


df %>%
    group_by(grade) %>%
    summarise(
        default_rate = mean(loan_status_binary),
        n = n()
    )

df %>%
    group_by(sub_grade) %>%
    summarise(
        default_rate = mean(loan_status_binary),
        n = n()
    )

df %>%
    group_by(term) %>%
    summarise(
        default_rate = mean(loan_status_binary),
        n = n()
    )

df %>%
    mutate(dti_band = cut(dti, breaks = seq(0, 60, 5))) %>%
    group_by(dti_band) %>%
    summarise(
        default_rate = mean(loan_status_binary, na.rm = TRUE),
        n = n()
    )


df %>%
    group_by(grade) %>%
    summarise(default_rate = mean(loan_status_binary)) %>%
    ggplot(aes(grade, default_rate)) +
    geom_col() +
    labs(x = "Credit grade", y = "Default rate")

df %>%
    group_by(sub_grade) %>%
    summarise(default_rate = mean(loan_status_binary)) %>%
    ggplot(aes(sub_grade, default_rate)) +
    geom_col() +
    labs(x = "Credit grade", y = "Default rate")


df %>%
    filter(short_term == 1) %>%
    group_by(homeowner, emp_10plus) %>%
    summarise(
        default_rate = mean(loan_status_binary),
        n = n()
    )


model_short <- glm(
    loan_status_binary ~ homeowner + emp_10plus + grade + dti + annual_inc,
    data = df %>% filter(short_term == 1),
    family = binomial
)

summary(model_short)
exp(coef(model_short))


state_defaults <- df %>%
    group_by(addr_state) %>%
    summarise(
        default_rate = mean(loan_status_binary),
        n = n()
    ) %>%
    filter(n > 100) %>%
    arrange(desc(default_rate))

model_state <- glm(
    loan_status_binary ~ addr_state + grade + dti + int_rate + term + annual_inc,
    data = df,
    family = binomial
)

summary(model_state)


# Borrower age is not available in the dataset.
# Therefore, the claim about younger borrowers cannot be tested directly.

df %>%
    group_by(grade) %>%
    summarise(
        default_rate = mean(loan_status_binary),
        n = n()
    )

model_default <- glm(
    loan_status_binary ~ grade + term + int_rate + dti +
        annual_inc + home_ownership + emp_length + purpose +
        addr_state + delinq_2yrs + inq_last_6mths + revol_util,
    data = df,
    family = binomial
)

summary(model_default)

odds_ratios <- exp(coef(model_default))
odds_ratios


dti_table <- df %>%
    mutate(dti_band = cut(dti, breaks = seq(0, 60, 5))) %>%
    group_by(dti_band) %>%
    summarise(
        default_rate = mean(loan_status_binary, na.rm = TRUE),
        n = n()
    )

dti_table

# Recommendation
# Low default tolerance: DTI cap around 20–25
# Medium default tolerance: DTI cap around 30
# High default tolerance: DTI cap around 35–40

df <- df %>%
    mutate(texas = ifelse(addr_state == "TX", 1, 0))

df %>%
    group_by(texas) %>%
    summarise(
        default_rate = mean(loan_status_binary),
        n = n()
    )

model_texas <- glm(
    loan_status_binary ~ texas + grade + dti + int_rate + term + annual_inc,
    data = df,
    family = binomial
)

summary(model_texas)
exp(coef(model_texas))

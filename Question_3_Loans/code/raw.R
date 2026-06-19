library(tidyverse)
library(dplyr)

#Read the data
loan_credit <- read_rds("data/Loan_Cred/loan_data.rds")

# Create binary variable

loan_credit <- loan_credit |>
    mutate(
        default_flag = case_when(
            loan_status %in% c("Charged Off", "Default") ~ 1,
            loan_status %in% c("Fully Paid") ~ 0,
            TRUE ~ NA_real_
        ),
        collection_flag = ifelse(loan_status %in% c("Late (31-120 days)",
                                                    "Late (16-30 days)",
                                                    "In Grace Period"), 1, 0)
    )

# Clean data & Filter important columns

df <- loan_credit |>
    select(
        loan_amnt, int_rate, installment, grade, sub_grade, term,
        emp_length, home_ownership, annual_inc, annual_inc_joint,
        verification_status, verification_status_joint,
        loan_status, desc, purpose, addr_state, dti, dti_joint,
        default_flag, collection_flag,
        revol_util, revol_bal,
        open_acc, total_acc,
        delinq_2yrs, inq_last_6mths,
    ) |>
    mutate(
        int_rate = as.numeric(gsub("%", "", int_rate)),
        revol_util = as.numeric(gsub("%", "", revol_util)),
        emp_10plus = ifelse(emp_length %in% c("10+ years"), 1, 0),
        homeowner = ifelse(home_ownership %in% c("MORTGAGE", "OWN"), 1, 0)
    ) |>
    filter(!is.na(default_flag))


# Are the Institute’s heuristics realistic given the data at hand?

# Analyse the grading system
df |>
    group_by(grade) |>
    summarise(default_rate = mean(default_flag), n = n())

df |>
    group_by(sub_grade) |>
    summarise(default_rate = mean(default_flag), n = n())


# See if credit grades are good indicators for young individuals defaults

loan_credit$desc[!is.na(loan_credit$desc)]

# 7, 8


# Check how home owners and people working for more than 10 years are doing
# with short term loan defaults

df |>
    group_by(home_ownership, emp_10plus, term) |>
    summarise(default_rate = mean(default_flag), n = n())


# Check if and how the US states differ with short term loan defaults

df |>
    group_by(addr_state) |>
    summarise(default_rate = mean(default_flag), n = n()) |>
    arrange(desc(default_rate))



ggplot(df, aes(grade, default_flag)) +
    stat_summary(fun = mean, geom = "bar") +
    labs(y = "Default rate", x = "Credit grade")

library(mgcv)

ggplot(df, aes(dti, default_flag)) +
    geom_smooth(method = "gam") +
    labs(y = "Probability of default", x = "DTI")

# Check if interest rates are determined by age, occupation and credit scores



# What are key risk drivers for default rates among borrowers

# What could be done, if anything, to reduce default risk when lenders assess borrower
# credibility?

# what would be an appropriate hard-cap for DTI levels be?

# Does this differ by State in the US?

# Provide the Director with different levels of default tolerance.

# Is Texas any different to other US States?




# Which type of customers are most likely to default on their debts?


df |>
    group_by(term) |>
    summarise(default_rate = mean(default_flag), n = n())


model1 <- glm(
    default_flag ~ grade + term + int_rate + dti +
        annual_inc + home_ownership + emp_10plus + purpose +
        addr_state + delinq_2yrs + inq_last_6mths,
    data = df,
    family = binomial
)

summary(model1)
exp(coef(model1))


df |>
    mutate(
        dti_band = cut(dti, breaks = c(0, 10, 20, 30, 40, 100)),
        income_band = cut(annual_inc, quantile(annual_inc, probs = seq(0,1,0.25), na.rm=TRUE))
    ) |>
    group_by(grade, dti_band, term) |>
    summarise(default_rate = mean(default_flag), n = n()) |>
    filter(n > 100) |>
    arrange(desc(default_rate))

df |>
    mutate(dti_band = cut(dti, breaks = seq(0, 60, 5))) |>
    group_by(dti_band) |>
    summarise(default_rate = mean(default_flag), n = n())

df <- df |>
    mutate(texas = ifelse(addr_state == "TX", 1, 0))

texas_model <- glm(
    default_flag ~ texas + grade + dti + int_rate + term + annual_inc,
    data = df,
    family = binomial
)

summary(texas_model)

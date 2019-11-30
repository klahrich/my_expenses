library(googledrive)
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)


read_data <- function(){
  
  drive_download('Depenses.csv', path = 'Depenses.csv', overwrite = TRUE)
  data <- read_csv('Depenses.csv')
  data %<>% mutate(date = as.Date(date, "%m/%d/%Y"))
  
  accounts <- c('BMO', 'Cash', 'Tangerine', 'TangerineTFSA', 'Mastercard', 
                'CELI', 'MAD', 'Koho', 'CapitalOne', 'TangerineCC', 'Loans',
                'MastercardBlack')
  
  incomes <- c('Salary', 'Impots', 'OtherIncome', 'InterestIncome', 'SidehustleIncome',
               'Cashback', 'Salary-Concordia')
  
  expense_groups <- 
    tribble(
      ~to,                ~category,
      'Resto',            'Food',
      'Parking',          'Transportation',
      'Coffee',           'Crap',
      'Home',             'Home',
      'Epicerie',         'Food',
      'IceCream',         'Food',
      'Patisserie',       'Food',
      'Shopping',         'Clothing',
      'Loyer',            'Rent',
      'Rent',             'Rent',
      'Health & Hygiene', 'Health',
      'Oumaima',          'Oumaima',
      'Essence',          'Transportation',
      'Entertainment',    'Entertainment',
      'Internet',         'Utilities',
      'Accessoires',      'Other',
      'Fees',             'Fees',
      'Adjustment',       'Other',
      'Cellphone',        'Utilities',
      'Crap',             'Crap',
      'OtherExpense',     'Other',
      'Metro/Bus',        'Transportation',
      'Voiture',          'Transportation',
      'Hydro',            'Utilities',
      'Gifts',            'Other',
      'SchoolOumaima',    'Education',
      'MBA',              'Education',
      'Bebe',             'Bebe',
      'Books',            'Education',
      'Uber',             'Transportation',
      'Travel',           'Entertainment',
      'Taxi',             'Transportation',
      'Ice cream',        'Crap',
      'Chocolate & Candy', 'Crap',
      'SidehustleExpenses', 'Other',
      'Other',             'Other'
    )
  
  data %>%
    mutate(
      is_transfer = (from %in% accounts & to %in% accounts),
      is_income = (from %in% incomes),
      is_loan = (to == 'Loans'),
      is_expense = !is_transfer & !is_income & !is_loan,
      type = case_when(
        is_transfer ~ 'Transfer',
        is_income ~ 'Income',
        is_loan ~ 'Loan',
        is_expense ~ 'Expense'
      ),
      trx_year = year(date),
      trx_year = as.integer(ifelse(trx_year < 2000, 2000 + trx_year, trx_year)),
      trx_month = as.integer(month(date)),
      trx_day = day(date),
      quarter = case_when(trx_month <= 3 ~ 'Q1', 
                          trx_month <= 6 ~ 'Q2', 
                          trx_month <= 9 ~ 'Q3', 
                          trx_month <= 12 ~ 'Q4'),
      date = as.Date(ISOdate(trx_year, trx_month, trx_day))
    ) %>%
    left_join(
      expense_groups, by = 'to'
    ) %>%
    mutate(
      category = ifelse(is.na(category), paste("Unknown category: ", to), category)
    )
}



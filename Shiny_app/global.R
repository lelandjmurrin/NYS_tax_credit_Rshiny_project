library(tidyverse)
library(PerformanceAnalytics)
library(car)

#Cleaning up Credit Name field
credit_name_func <- function(df){
  return (df %>% 
            mutate(`Credit Name` = ifelse(str_starts(`Credit Name`, 'Alcoholic'), 'Alcoholic Beverage Production Credit', `Credit Name`), 
                   `Credit Name` = ifelse(str_starts(`Credit Name`, 'Manufacture'), 'Real Property Tax Relief Credit for Manufacturing', `Credit Name`))
  )
}

#Loading Datasets
NYS_tax_credit_net_income <- credit_name_func(read_csv('../NYS_Corp_Tax_Credit_data/NYS_tax_credit_net_income.csv'))
NYS_tax_credit_industry <- credit_name_func(read_csv('../NYS_Corp_Tax_Credit_data/NYS_tax_credit_industry.csv'))
colnames(NYS_tax_credit_net_income)[5] <- 'Group'
colnames(NYS_tax_credit_industry)[5] <- 'Group'

income_cleaned <- read_csv('../NYS_Corp_Tax_Credit_data/income_cleaned.csv')
industry_cleaned <- read_csv('../NYS_Corp_Tax_Credit_data/industry_cleaned.csv')

all.dataframes <- list('income' = NYS_tax_credit_net_income, 
                       'industry' = NYS_tax_credit_industry, 
                       'income_cleaned' = income_cleaned, 
                       'industry_cleaned' = industry_cleaned)

all.samples.credit.name <- list('income' = 'Alcoholic Beverage Production Credit', 
                                'industry' = 'Investment Tax Credit',
                                'income_cleaned' = 'Alcoholic Beverage Production Credit',
                                'industry_cleaned' = 'Investment Tax Credit')
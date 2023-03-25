library(tidyverse)
library(caret)
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

all.dataframes <- list('income' = NYS_tax_credit_net_income, 'industry' = NYS_tax_credit_industry)
all.samples.credit.name <- list('income' = 'Alcoholic Beverage Production Credit', 'industry' = 'Investment Tax Credit')




# flights <- read.csv(file = "./flights14.csv")
# 
# origin_values <- flights %>% select(origin) %>% distinct()
# dest_values <- flights %>% select(dest) %>% distinct()
# month_values <- flights %>% select(month) %>% distinct()


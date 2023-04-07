library(tidyverse)
library(PerformanceAnalytics)
library(car)
library(glmnet)
library(DT)

#Cleaning up Credit Name field function
credit_name_func <- function(df){
  return (df %>%
            mutate(`Credit Name` = ifelse(str_starts(`Credit Name`, 'Alcoholic'), 'Alcoholic Beverage Production Credit', `Credit Name`),
                   `Credit Name` = ifelse(str_starts(`Credit Name`, 'Manufacture'), 'Real Property Tax Relief Credit for Manufacturing', `Credit Name`))
  )
}

#Loading Datasets
NYS_tax_credit_net_income <- credit_name_func(read_csv('data/NYS_tax_credit_net_income.csv')) 
NYS_tax_credit_industry <- credit_name_func(read_csv('data/NYS_tax_credit_industry.csv'))
colnames(NYS_tax_credit_net_income)[5] <- 'Group'
colnames(NYS_tax_credit_industry)[5] <- 'Group'

income_cleaned <- read_csv('data/income_cleaned.csv')
industry_cleaned <- read_csv('data/industry_cleaned.csv')


all.dataframes <- list('income' = NYS_tax_credit_net_income, 
                       'industry' = NYS_tax_credit_industry, 
                       'income_cleaned' = income_cleaned, 
                       'industry_cleaned' = industry_cleaned)

all.samples.credit.name <- list('income' = 'Alcoholic Beverage Production Credit', 
                                'industry' = 'Investment Tax Credit',
                                'income_cleaned' = 'Alcoholic Beverage Production Credit',
                                'industry_cleaned' = 'Investment Tax Credit')



#BoxCox transformation functions for producing likelihood plots
bc_func_income <- function (){
  bc = boxCox(lm(Avg ~ ., data = all.dataframes[['income_cleaned']]), lambda = seq(-0.2, 0.2, 1/10))
  lambda.bc = bc$x[which(bc$y == max(bc$y))]
  return(all.dataframes[['income_cleaned']] %>%
           mutate(Avg.bc = (Avg^lambda.bc -1)/lambda.bc) %>%
           select(-c(Avg)))
}

bc_func_industry <- function (){
  bc = boxCox(lm(Avg ~ ., data = all.dataframes[['industry_cleaned']]), lambda = seq(-0.2, 0.2, 1/10))
  lambda.bc = bc$x[which(bc$y == max(bc$y))]
  return(all.dataframes[['industry_cleaned']] %>%
           mutate(Avg.bc = (Avg^lambda.bc -1)/lambda.bc) %>%
           select(-c(Avg)))
}

income_cleaned_bc <- bc_func_income()
industry_cleaned_bc <-bc_func_industry()

all.dataframes <- append(all.dataframes, 
                         list('income_cleaned_bc' = income_cleaned_bc, 'industry_cleaned_bc' = industry_cleaned_bc))

stepwise_BIC <- read_csv('data/stepwiseBIC_results.csv')

final_metrics_df <- read_csv('data/final_metrics_table.csv')

#User Input Key datasets
user_input_key <- read_csv('data/user_input_key.csv')

best.saved.model <- readRDS('data/best_models.rds')
lambda.bcs <- readRDS('data/lambda.bcs.rds')

boxcox_to_dollars <- function(x, dataset){
  (x*lambda.bcs[[dataset]]+1)^(1/lambda.bcs[[dataset]])
}

# income_groups <- user_input_key %>% filter(dataset == 'income',
#                                            str_starts(dummy_col, 'Group')) %>% 
#                                     select(col) %>%
#                                     mutate(col = c('Zero or Net Loss', 
#                                                    '$1 - $99,999', 
#                                                    '100,000 - 499,999', 
#                                                    '500,000 - 999,999', 
#                                                    '1,000,000 - 24,999,999', 
#                                                    '25,000,000 - 49,999,999', 
#                                                    '50,000,000 - 99,999,999', 
#                                                    '100,000,000 - 499,999,999',
#                                                    '500,000,000 - and over')) %>%
#                                     as.vector() %>%
#                                     dplyr::first()
# 
# # %>% 
# #                       filter(dataset == 'income', str_starts(dummy_col, 'Group')) %>% 
# #                       select(col) %>% 
# #                       arrange(col) %>%
# #                       as.vector() %>%
# #                       dplyr::first()
# 
# income_creditnames <- user_input_key %>% 
#                         filter(dataset == 'income', str_starts(dummy_col, 'Name')) %>% 
#                         select(col) %>%
#                         arrange(col) %>%
#                         as.vector() %>%
#                         dplyr::first()







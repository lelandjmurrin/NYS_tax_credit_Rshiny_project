library(tidyverse)
library(PerformanceAnalytics)
library(car)
library(glmnet)
library(DT)

#Cleaning up Credit Name field
credit_name_func <- function(df){
  return (df %>%
            mutate(`Credit Name` = ifelse(str_starts(`Credit Name`, 'Alcoholic'), 'Alcoholic Beverage Production Credit', `Credit Name`),
                   `Credit Name` = ifelse(str_starts(`Credit Name`, 'Manufacture'), 'Real Property Tax Relief Credit for Manufacturing', `Credit Name`))
  )
}
# 
# #Test and Train data splitting function
# test_train_split <- function(dummy_bc, best.formula) {
#   X <- model.matrix(best.formula, data = dummy_bc)[,-1]
#   y <- as.matrix(dummy_bc %>% select(all.vars(best.formula)[1]))
#   
#   set.seed(0)
#   train.i = sample(1:nrow(dummy_bc), 0.8*nrow(dummy_bc), replace = F)
#   
#   #train
#   X.train <- X[train.i,]
#   y.train <- y[train.i,]
#   
#   #test
#   X.test <- X[-train.i,]
#   y.test <- y[-train.i,]
#   
#   data.train <- as.data.frame(cbind(y.train, X.train))
#   data.test <- as.data.frame(cbind(y.test, X.test))
#   colnames(data.train)[1] = all.vars(best.formula)[1]
#   colnames(data.test)[1] = all.vars(best.formula)[1]
#   
#   return (list('X.train' = X.train, 'y.train' = y.train, 'X.test' = X.test, 'y.test' = y.test, 'data.train' = data.train, 'data.test' = data.test))
# }
# 
# #creating dummy variable columns for stepwise
# dummy_func <- function (df){
#   x = model.matrix(Avg.bc ~., df)[, -1]
#   dummy_bc = as.data.frame(x) %>% mutate(Avg.bc = df$Avg.bc)
#   #colnames(dummy_bc) <- str_replace_all(colnames(dummy_bc), "-|'|/| |,|�|&" , '_')
#   colnames(dummy_bc) <- str_replace_all(colnames(dummy_bc), "[-'/ ,�&()`]" , '_')
#   return(dummy_bc)
# }


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

rsquare_df <- read_csv('data/final_rsquare_table.csv')
RMSE_df <- read_csv('data/final_RMSE_table.csv')

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







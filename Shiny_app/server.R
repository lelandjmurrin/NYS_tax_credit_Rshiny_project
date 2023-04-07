library(shiny)


function(input, output, session){
  
#REACTIVE EXPRESSIONS
  
  #Changes the selected dataframe between income and industry 
  selected.df <- reactive({all.dataframes[[input$dataset]]})

  selected.df.cleaned <- reactive({all.dataframes[[paste0(input$dataset, '_cleaned')]]})
  
  selected.df.index <- reactive({input$dataset})
  
  observeEvent(input$dataset,
               {updateSelectizeInput(session, input = "groupnames",
                                      choices = user_input_key %>%
                                        filter(dataset == input$dataset, str_starts(dummy_col, 'Group')) %>% 
                                        dplyr::select(col) %>% 
                                        arrange(col) %>%
                                        as.vector() %>% 
                                        dplyr::first()
                                     )
                })
                 
  observeEvent(input$dataset,
               {updateSelectizeInput(session, input = "creditnames",
                                      choices = c()
                                     )
               })
  
  observeEvent(input$groupnames,
               {updateSelectizeInput(session, input = "creditnames",
                                     choices = all.dataframes[[paste0(input$dataset, '_cleaned')]] %>%
                                       filter(Group == input$groupnames) %>% 
                                       dplyr::select(Name) %>% 
                                       distinct() %>% 
                                       arrange(Name) %>% 
                                       as.vector() %>%
                                       dplyr::first()
                                     )
               })

  #Forecast Section--------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  #EDA Section--------------------------------------------------------------------------------------------------------------------------------------------
  
  #Changes the selected raw sample data records between income and industry and Credit Names
  sample.df <- reactive({
    selected.df() %>% 
      dplyr::select(`Tax Year`,
             `Credit Type`, 
             `Credit Name`, 
             Group,
             `Number of Taxpayers`,
             `Amount of Credit`) %>% 
      filter(`Tax Year` == 2019, 
             `Credit Name` == all.samples.credit.name[[input$dataset]],
             `Credit Type` == 'Credit Earned') %>%
      dplyr::select(-`Credit Type`)
  })

  #Changes the group category (Year, Group and Credit Name) for the facetwraps in the Distribution Section of EDA
  selected.grouping <- reactive({
    as.formula(input$grouping)
  })
  
  #Transform Section---------------------------------------------------------------------------------------------------------------------------------------
  
  #Changes the selected cleaned sample data records between income and industry and Credit Names
  sample.df_cleaned <- reactive({
    selected.df.cleaned() %>%
      filter(Year == 2019, Name == all.samples.credit.name[[paste0(input$dataset, '_cleaned')]])
  })
  
  #Regression Section---------------------------------------------------------------------------------------------------------------------------------------
  
  pre.boxcox.model <- reactive({
      lm(Avg ~ ., data = all.dataframes[[paste0(input$dataset, '_cleaned')]])
  })
  
  boxcox.model <- reactive({
    lm(Avg.bc ~ ., data = all.dataframes[[paste0(input$dataset, '_cleaned_bc')]]) 
  })
  
  
  
  
  
  
#OUTPUT VARIABLES
  
  #Forecast Section------------------------------------------------------------------------------------------------------------------------------------------
  output$introduction <- renderUI({includeHTML('www/introduction.html')})
  
  output$forecast <- renderPlot({
    user_input_df_updated <- user_input_key %>%
        filter(dataset == input$dataset) %>%
        mutate(value = ifelse(col == input$creditnames, 1, value),
               value = ifelse(col == 'Year', as.numeric(input$pred_year), value),
               value = ifelse(col == input$groupnames, 1, value)) %>%
        dplyr::select(-col, -dataset) %>%
        pivot_wider(names_from = dummy_col,
                    values_from = value)

      user_input_df_updated <- user_input_df_updated[rep(1,21),] %>%
        mutate(Num = c(1, seq(25, 500, by = 25)))

      user_input_pred <- predict.lm(best.saved.model[[input$dataset]], newdata = user_input_df_updated, interval = 'confidence')

      user_input_pred_final <- as.data.frame(user_input_pred) %>%
        mutate(fit_dollars = boxcox_to_dollars(fit, input$dataset),
               lwr_dollars = boxcox_to_dollars(lwr, input$dataset),
               upr_dollars = boxcox_to_dollars(upr, input$dataset)) %>%
        mutate(Num = user_input_df_updated$Num)

      user_input_pred_final %>%
        ggplot(aes(Num, fit_dollars)) +
        geom_ribbon(aes(ymin = lwr_dollars,
                        ymax = upr_dollars),
                    fill = 'lightblue3') +
        geom_point() +
        geom_errorbar(aes(ymin = lwr_dollars,
                          ymax = upr_dollars)) +
        geom_line(color = 'blue') +
        lims(y = c(0, NA)) +
        labs(x = 'Number of Taxpayers',
             y = 'Avg Credit Earned per Taxpayer ($)',
             title = paste0('PREDICTIONS', '\nCredit: ', input$creditnames, '\nYear: ', input$pred_year, ', ', 'Group: ', input$groupnames))
  })
  
  output$num_estimate_caption <- renderUI({
    HTML(paste0('The following datatable pulls the Number of Taxpayers from the NYS Tax Credit database for the<br><b>', 
           input$creditnames, 
           '</b><br> in the <br><b>', 
           input$groupnames, 
           ' group</b><br>for up to 5 of the most recently available tax years'))
  })
  
  output$num_estimate <- renderDataTable({
    all.dataframes[[paste0(input$dataset, '_cleaned')]] %>% 
      filter(Name == input$creditnames, 
             Group == input$groupnames) %>% 
      group_by(Name, Year, Group) %>% 
      summarise(`Number of Taxpayers` = sum(Num)) %>% 
      arrange(desc(Year)) %>% 
      head(5) %>% 
      ungroup() %>% 
      dplyr::select(-c(Name,Group))
  })
  
  #EDA Section----------------------------------------------------------------------------------------------------------------------------------------------
  
  #Returns the data from the background.html file for the background tab
  output$background <- renderUI({includeHTML('www/background.html')})
  
  #Returns the raw sample data
  output$sample <- renderDataTable(sample.df())

  #Returns the correlation plot of the raw data
  output$correlations <- renderPlot(
    selected.df() %>% 
      filter(`Number of Taxpayers` > 0,
             `Amount of Credit` > 0,
             `Mean Amount of Credit` > 0,
             Group != 'Total') %>%
      mutate(`Log Number of Taxpayers` = log(`Number of Taxpayers`),
             `Log Amount of Credit` = log(`Amount of Credit`),
             `Log Mean Amount of Credit` = log(`Mean Amount of Credit`)) %>%
      dplyr::select(`Tax Year`,
             `Log Number of Taxpayers`,
             `Log Amount of Credit`,
             `Log Mean Amount of Credit`) %>%
      chart.Correlation(histogram = T)
  )
  
  #Returns the histogram distributions for Log Number of Taxpayers by each of the groupings (Year, Name, Group)
  output$distributions.taxpayer <- renderPlot(
    selected.df() %>% 
      mutate(log_taxpayers = log(`Number of Taxpayers` + 1)) %>% 
      filter(log_taxpayers > 0) %>% 
      ggplot(aes(`log_taxpayers`)) + 
      labs(x = 'Log Taxpayers', 
           y = 'Count', 
           title = paste0('Taxpayer Distribution by ', all.vars(selected.grouping())[1])) + 
      geom_histogram() + 
      facet_wrap(selected.grouping())
  )
  
  #Returns the histogram distributions for Mean Amount of Credit per Taxpayer by each of the groupings (Year, Name, Group)
  output$distributions.credit <- renderPlot(
    selected.df() %>% 
      filter(`Mean Amount of Credit` > 0) %>% 
      mutate(log_mean_amount = log(`Mean Amount of Credit`)) %>% 
      ggplot(aes(log_mean_amount)) + 
      labs(x = 'Log Mean Amount', 
           y = 'Count', 
           title = paste0('Amount of Credit per Taxpayer by ', all.vars(selected.grouping())[1])) + 
      geom_histogram() + 
      facet_wrap(selected.grouping())
  ) #TO DO: change the title to input the selected grouping instead of hardcoded Tax Year
  
  #Transform Section---------------------------------------------------------------------------------------------------------------------------------------
  
  #Returns the methods used for transforming the data within a methods.text file (joining, filtering, imputation, adding Avg, dropping 0 etc...)
  output$methods <- renderUI({includeHTML('www/methods.html')})
  
  #Returns the raw sample dataset for comparison with cleaned sample dataset in Results tab
  output$sample2 <- renderDataTable(sample.df())
  
  #Returns the cleaned sample dataset
  output$results.cleaned <- renderDataTable(sample.df_cleaned())
  
  #Regression Section-------------------------------------------------------------------------------------------------------------------------------------
  
  output$pre_bc1 <- renderPlot(
    plot(pre.boxcox.model(), which = 1)
  )

  output$pre_bc2 <- renderPlot(
    plot(pre.boxcox.model(), which = 2)
  )
  
  output$pre_bc3 <- renderPlot(
    plot(pre.boxcox.model(), which = 3)
  )
  
  output$pre_bc4 <- renderPlot(
    plot(pre.boxcox.model(), which = 5)
  )

  output$pre_bc5 <- renderPlot(
    all.dataframes[[paste0(input$dataset, '_cleaned')]] %>%
      ggplot(aes(Avg)) + 
      geom_histogram(bins = 30) + 
      labs(title = 'Average Credit Amount Distribution') + 
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  output$likelihood <- renderPlot(
    ifelse(input$dataset == 'income', 
           boxCox(lm(Avg ~ ., data = all.dataframes[['income_cleaned']]), seq(-0.2, 0.2, 1/10)),
           boxCox(lm(Avg ~ ., data = all.dataframes[['industry_cleaned']]), seq(-0.2, 0.2, 1/10))
           ) 
  )

  output$bc1 <- renderPlot(
    plot(boxcox.model(), which = 1)
  )

  output$bc2 <- renderPlot(
    plot(boxcox.model(), which = 2)
  )

  output$bc3 <- renderPlot(
    plot(boxcox.model(), which = 3)
  )

  output$bc4 <- renderPlot(
    plot(boxcox.model(), which = 5)
  )

  output$bc5 <- renderPlot(
    all.dataframes[[paste0(input$dataset, '_cleaned_bc')]] %>%
      ggplot(aes(Avg.bc)) +
      geom_histogram(bins = 30) +
      labs(title = 'Average Credit Amount Distribution') +
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  output$bc6 <- renderDataTable(
    BIC(boxcox.model(), pre.boxcox.model()) %>% 
      tibble::rownames_to_column(var = 'model') %>% 
      mutate(model = str_replace_all(model, '.model[()]+', ''))
  )
  
  output$stepwise_bic <- renderDataTable(
    stepwise_BIC %>% filter(dataset == input$dataset) %>% dplyr::select(-dataset)
  )
  
  output$rsquare <- renderDataTable(
    final_metrics_df %>% filter(str_detect(Index, input$dataset), metric == 'adj.Rsquare') %>% dplyr::select(-Index, -metric)
  )
  
  output$rmse <- renderDataTable(
    final_metrics_df %>% filter(str_detect(Index, input$dataset), metric == 'RMSE') %>% dplyr::select(-Index, -metric)
  )
  
  
  #Discussion Section ---------------------------------------------------------------------------------------------------------------------------------------------

  output$conclusions <- renderUI({includeHTML('www/conclusions.html')})
  
  output$future_work <- renderUI({includeHTML('www/future_work.html')})
}
library(shiny)


function(input, output){
  
#Reactive Expressions
  
  #Changes the selected dataframe between income and industry 
  selected.df <- reactive({all.dataframes[[input$dataset]]}) 
  
  selected.df.index <- reactive({input$dataset})
  
  #EDA Section--------------------------------------------------------------------------------------------------------------------------------------------
  
  #Changes the selected raw sample data records between income and industry and Credit Names
  sample.df <- reactive({
    all.dataframes[[input$dataset]] %>% 
      select(`Tax Year`, 
             `Credit Type`, 
             `Credit Name`, 
             Group,
             `Number of Taxpayers`,
             `Amount of Credit`) %>% 
      filter(`Tax Year` == 2019, 
             `Credit Name` == all.samples.credit.name[[input$dataset]],
             `Credit Type` == 'Credit Earned') %>%
      select(-`Credit Type`)
  })
  
  #Changes the group category (Year, Group and Credit Name) for the facetwraps in the Distribution Section of EDA
  selected.grouping <- reactive({
    as.formula(input$grouping)
  })
  
  #Transform Section---------------------------------------------------------------------------------------------------------------------------------------
  
  #Changes the selected cleaned sample data records between income and industry and Credit Names
  sample.df_cleaned <- reactive({
    all.dataframes[[paste0(input$dataset, '_cleaned')]] %>%
      filter(Year == 2019, Name == all.samples.credit.name[[paste0(input$dataset, '_cleaned')]])
  })
  
  #Regression Section---------------------------------------------------------------------------------------------------------------------------------------
  
  boxcox.model <- reactive({
    lm(Avg ~ ., data = income_cleaned)
  })

#Output Variables
  
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
      select(`Tax Year`,
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
           title = 'Taxpayer Distribution by Tax Year') +
      geom_histogram() + 
      facet_wrap(selected.grouping())
  )
  
  #Returns the histogram distributions for Mean Amount of Credit per Taxpayer by each of the groupings (Year, Name, Group)
  output$distributions.credit <- renderPlot(
    selected.df() %>% 
      filter(`Mean Amount of Credit` > 0) %>% 
      mutate(log_mean_amount = log(`Mean Amount of Credit`)) %>% 
      ggplot(aes(log_mean_amount)) + 
      labs(x = 'Log Taxpayers', 
           y = 'Count', 
           title = paste0('Mean Amount of Credit per Taxpayer by ', all.vars(selected.grouping())[2])) +
      geom_histogram() + 
      facet_wrap(selected.grouping())
  ) #TO DO: change the title to input the selected grouping instead of hardcoded Tax Year
  
  #Transform Section---------------------------------------------------------------------------------------------------------------------------------------
  
  #Returns the methods used for transforming the data within a methods.text file (joining, filtering, imputation, adding Avg, dropping 0 etc...)
  output$methods <- renderText({includeText('www/methods.txt')})
  
  #Returns the raw sample dataset for comparison with cleaned sample dataset in Results tab
  output$sample2 <- renderDataTable(sample.df())
  
  #Returns the cleaned sample dataset
  output$results.cleaned <- renderDataTable(sample.df_cleaned())
  
  #Regression Section-------------------------------------------------------------------------------------------------------------------------------------
  
  #
  output$pre_bc <- renderPlot(
    #df = all.dataframes[[paste0(selected.df.index(), '_cleaned')]]
    plot(boxcox.model(), which = 2)
  )
  
  output$likelihood <- renderImage({
    filename = paste0('www/', selected.df.index(), '_bc_likelihood_plot.png')
    list(src = filename,
         height = 700,
         contentType = 'image/png')
  }, deleteFile = F)
}


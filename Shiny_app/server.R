library(shiny)


function(input, output){
  selected.df <- reactive({
    all.dataframes[[input$dataset]]
  })
  
  selected.sample <- reactive({
    all.samples.credit.name[[input$dataset]]
  })
  
  selected.results <- reactive({
    all.samples.credit.name[[paste(input$dataset, '_cleaned')]]
  })
  
  selected.grouping <- reactive({
    as.formula(input$grouping)
  })
  
  
  output$background <- renderUI({includeHTML('background.html')})
  
  output$sample <- renderDataTable(
    selected.df() %>% 
      select(`Tax Year`, 
             `Credit Type`, 
             `Credit Name`, 
             Group,
             `Number of Taxpayers`,
             `Amount of Credit`) %>% 
      filter(`Tax Year` == 2019, 
             `Credit Name` == selected.sample(),
             `Credit Type` == 'Credit Earned')
  )
  
  output$correlations <- renderPlot(
    selected.df() %>% 
      filter(`Number of Taxpayers` > 0) %>% 
      mutate(`Number of Taxpayers` = log(`Number of Taxpayers` + 1)) %>% 
      select(`Tax Year`, 
             `Number of Taxpayers`, 
             `Amount of Credit`, 
             `Mean Amount of Credit`) %>% 
      chart.Correlation(histogram = T)
  )
  
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
  
  output$distributions.credit <- renderPlot(
    selected.df() %>% 
      filter(`Mean Amount of Credit` > 0) %>% 
      mutate(log_mean_amount = log(`Mean Amount of Credit`)) %>% 
      ggplot(aes(log_mean_amount)) + 
      labs(x = 'Log Taxpayers', 
           y = 'Count', 
           title = 'Mean Amount of Credit per Taxpayer by Tax Year') +
      geom_histogram() + 
      facet_wrap(selected.grouping())
  )
  
  output$methods <- renderText({includeText('methods.txt')})
  
  
  output$results.cleaned <- renderDataTable(
    income_cleaned %>% 
      filter(Year == 2019, Name == 'Alcoholic Beverage Production Credit')
  )
}
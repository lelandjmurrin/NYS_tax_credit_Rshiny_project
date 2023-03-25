library(shiny)


function(input, output){
  selected.df <- reactive({
    all.dataframes[[input$dataset]]
    # flights %>%
    #   filter(origin == input$origin,
    #          dest == input$dest,
    #          month == input$month)
  })
  
  selected.sample <- reactive({
    all.samples.credit.name[[input$dataset]]
  })
  
  selected.facetwrap <- reactive({
    input$facetwrap
  })
  
  getpage <- function (){
    return (includeHTML('background.html'))
  }
  
  output$background <- renderUI({getpage()})
  
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
      geom_histogram() + 
      facet_wrap(~ `Tax Year`)
  )
  # output$months <- renderPlot(
  #   flights_delay() %>% 
  #     mutate(month = as.factor(month)) %>% 
  #     ggplot(aes(month)) + 
  #     geom_bar()
  # )
  # output$delay <- renderPlot(
  #   flights_delay() %>%
  #     group_by(carrier) %>% 
  #     summarise(departure = mean(dep_delay), 
  #               arrival = mean(arr_delay)) %>% 
  #     pivot_longer(c(departure, arrival), 
  #                  names_to = 'type', 
  #                  values_to = 'delay') %>% 
  #     ggplot(aes(carrier, delay, fill = type)) + 
  #     geom_col(position = 'dodge') +
  #     ggtitle('Average delay')
  # )
  # output$table <- renderDataTable(
  #   flights_delay() %>% group_by(carrier) %>% 
  #     summarise(departure_delay = mean(dep_delay), 
  #               arrival_delay = mean(arr_delay), 
  #               month = max(month), 
  #               number_of_flights = n())
  # )
}
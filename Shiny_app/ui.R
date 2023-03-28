library(shinydashboard)

dashboardPage(
  dashboardHeader(title='NYS Article 9A Tax Credit Analysis'),
  dashboardSidebar(
    sidebarUserPanel("Leland Murrin",
                     image = 'NYCDSA.png'),
    sidebarMenu(
      selectizeInput('dataset',
                     'Dataset',
                     choices = c('income', 'industry')), #dataset selection
      menuItem("EDA", tabName = "eda", icon = icon("database"),
               menuSubItem('Background', tabName = 'background'),
               menuSubItem('Sample of Data', tabName = 'sample'),
               menuSubItem('Correlations', tabName = 'correlations'),
               menuSubItem('Distributions', tabName = 'distributions'),
               selectizeInput('grouping',
                              'Grouping',
                              choices = c('Group' = '~ Group', 
                                          'Tax Year' = '~ `Tax Year`', 
                                          'Credit Name' = '~ `Credit Name`')) #display facetwrap histograms
               ),
      menuItem("Transform", tabName = "transform", icon = icon("cog"),
               menuSubItem('Methods', tabName = 'methods'),
               menuSubItem('Results', tabName = 'results')
               ),
      menuItem("Regression", tabName = "regression", icon = icon("line-chart"),
               menuSubItem('BoxCox', tabName = 'boxcox'),
               menuSubItem('Stepwise BIC', tabName = 'stepwise_bic'),
               menuSubItem('Regularization', tabName = 'regularization'),
               menuSubItem('Best Model', tabName = 'best_model')
               ),
      menuItem("Discussion", tabName = "discussion", icon = icon("refresh"),
               menuSubItem('Conclusions', tabName = 'conclusions'),
               menuSubItem('Future Work', tabName = 'future_work')
               )
    )
  ),
  dashboardBody(
      tabItems(
          tabItem(tabName = 'background',
                  htmlOutput('background')
                  ),
          tabItem(tabName = 'sample',
                  dataTableOutput('sample')
                  ),
          tabItem(tabName = 'correlations',
                  fluidRow(column(12, box(h2("Correlations Plots"), width = NULL, plotOutput("correlations",  height = 600)))
                          )
                  ),
          tabItem(tabName = 'distributions',
                  fluidRow(
                            tabBox(
                                    width = NULL,
                                    tabPanel("Number of Taxpayers", plotOutput("distributions.taxpayer",height = 700)),
                                    tabPanel("Amount of Credit per Taxpayer", plotOutput("distributions.credit", height = 700))
                                  )
                          )
                ),
          tabItem(tabName = 'methods',
                  textOutput('methods')
                  ),
          tabItem(tabName = 'results',
                  fluidRow(
                              tabBox(
                                      width = NULL,
                                      tabPanel("Raw Data", dataTableOutput("sample2")),
                                      tabPanel("Cleaned Data", dataTableOutput("results.cleaned"))
                                    )
                          )
                  ),
          tabItem(tabName = 'boxcox',
            fluidRow(
              tabBox(
                width = NULL,
                tabPanel("Analysis of Average", plotOutput('pre_bc')),
                tabPanel("BoxCox Likelihood", imageOutput('likelihood', height = 700)),
              )
            )
          )
              )
              )
)
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
                    box(h2("Number of Taxpayers"), width = NULL, plotOutput("distributions.taxpayer",  height = 700)),
                            box(h2("Amount of Credit per Taxpayer"), width = NULL, plotOutput("distributions.credit",  height = 700))
                          )
                  ),
          tabItem(tabName = 'methods',
                  textOutput('methods')
                  )
          # tabItem(tabName = 'results',
          #         fluidRow(
          #                 box(h2("Raw Data"),
          #                     width = NULL,
          #                     dataTableOutput("sample")
          #                     )
          #                 box(h2("Cleaned Data"),
          #                     width = NULL,
          #                     dataTableOutput("results.cleaned")
          #                     )
          #                 )
          #         ) NEXT TIME (fix issue with output token "sample" can't be called in 2 places)
              )
              )
)

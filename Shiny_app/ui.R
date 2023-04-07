library(shinydashboard)

dashboardPage(
  dashboardHeader(title='NYS Tax Cred Analysis (Article 9A)'),
  dashboardSidebar(
    sidebarUserPanel("Leland Murrin",
                     image = 'empire-state-building.png'),
    sidebarMenu(
      selectizeInput('dataset',
                     'Dataset',
                     choices = c('income', 'industry')), #dataset selection
      selectizeInput('groupnames',
                     'Income/Industry Group',
                     choices = c()), #Income/Industry Group Selection
      selectizeInput('creditnames',
                     'Credit Name',
                     choices = c()), #Credit Name Selection
      selectizeInput('pred_year',
                     'Year',
                     choices = 2019:2030),
      menuItem("Forecast", tabName = "forecast", icon = icon("line-chart")
              ),
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
               menuSubItem('Regularization/Best Model', tabName = 'regularization_bestmodel')
               ),
      menuItem("Discussion", tabName = "discussion", icon = icon("refresh"),
               menuSubItem('Conclusions', tabName = 'conclusions'),
               menuSubItem('Future Work', tabName = 'future_work')
               )
    )
  ),
  dashboardBody(
      tabItems(
          tabItem(tabName = 'forecast',
                  fluidRow(
                    tabBox(
                      width = NULL,
                      tabPanel("Introduction",
                               htmlOutput('introduction')
                      ),
                      tabPanel("Forecast", 
                               fluidRow(
                                  column(12, plotOutput('forecast', height = 500)),
                                  column(4, htmlOutput('num_estimate_caption'), style = 'padding:50px'),
                                  column(6, dataTableOutput('num_estimate'), style = 'padding:50px')
                                        )
                      )
                    )
                    )
                  ),
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
                  htmlOutput('methods')
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
                tabPanel("Diagnostics of Linear Model Pre-BoxCox", 
                         fluidRow(
                            column(6, plotOutput('pre_bc1', height = 350)),
                            column(6, plotOutput('pre_bc2', height = 350)),
                            column(6, plotOutput('pre_bc3', height = 350)),
                            column(6, plotOutput('pre_bc4', height = 350)),
                            column(6, offset = 3, plotOutput('pre_bc5', height = 350))
                          )
                ),
                tabPanel("BoxCox Likelihood", plotOutput('likelihood', height = 700)),
                tabPanel("Diagnostics of Linear Model Post-BoxCox",
                         fluidRow(
                           column(6, plotOutput('bc1', height = 350)),
                           column(6, plotOutput('bc2', height = 350)),
                           column(6, plotOutput('bc3', height = 350)),
                           column(6, plotOutput('bc4', height = 350)),
                           column(6, plotOutput('bc5', height = 350), style = 'padding:50px'),
                           column(6, dataTableOutput('bc6'), style = 'padding:50px')
                         )
                )
              )
            )
          ),
          tabItem(tabName = 'stepwise_bic',
              fluidRow(
                tabBox(
                  width = NULL,
                  tabPanel("Stepwise BIC Results",
                            column(offset = 2, 8, dataTableOutput('stepwise_bic'))
                               )
                    )
                  )
          ),
          tabItem(tabName = 'regularization_bestmodel',
                  fluidRow(
                    tabBox(
                      width = NULL,
                      tabPanel("Adjusted R Squared",
                               column(offset = 2, 8, dataTableOutput('rsquare'))
                      ),
                      tabPanel("Root Mean Squared Error",
                               column(offset = 2, 8, dataTableOutput('rmse'))
                      )
                    )
                  )
          ),
          tabItem(tabName = 'conclusions',
                  fluidRow(
                    tabBox(
                      width = NULL,
                      tabPanel("Summary/Conclusions",
                               htmlOutput('conclusions')
                      )
                      )
                    )
                  ),
          tabItem(tabName = 'future_work',
                  fluidRow(
                    tabBox(
                      width = NULL,
                      tabPanel("Future Work and Research",
                               htmlOutput('future_work')
                      )
                    )
                  )
          )
      )
   )
)
